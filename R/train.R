# CV SPLITS

#' @export
train_test_split <- function(data, split_date) {
  in_test <- data |>
    filter(coalesce(bmt_team_end_date, Sys.Date()) > split_date)

  test_idx <- which(data$csn %in% in_test$csn)
  train_idx <- which(!(data$mrn %in% in_test$mrn))

  make_splits(
    list(analysis = train_idx, assessment = test_idx),
    data = data
  )
}

#' @export
make_cv_splits <- function(discharge_train, admission_train, h, scheduled_admissions = NULL) {
  admission_splits <- make_admission_splits(admission_train, h)

  cv_dates <- as_date(admission_splits$id)

  discharge_train_extended <- discharge_train |>
    filter(date %in% cv_dates, time > 0) |>
    extend_discharge_data(h, end_var = bmt_team_end_date) |>
    bind_rows(discharge_train)

  discharge_splits <- make_discharge_splits(
    discharge_train_extended,
    dates = cv_dates
  )

  out <- list(
    admission = admission_splits,
    discharge = discharge_splits,
    data = discharge_train_extended
  )

  if (!is.null(scheduled_admissions)) {
    scheduled_admissions_nested <- scheduled_admissions |>
      filter(date %in% cv_dates) |>
      rename(cv_date = date, date = admission_date) |>
      nest(.by = cv_date)

    scheduled_admissions_nested <- left_join(
      tibble(id = admission_splits$id, cv_date = cv_dates),
      scheduled_admissions_nested,
      by = "cv_date"
    )

    scheduled_admissions_out <- pmap(
      scheduled_admissions_nested,
      \(cv_date, data, ...) {
        prep_admission_data(
          day_0 = cv_date,
          h = 30,
          scheduled_admissions = data,
          na_zero_transplant_types = c("Autologous", "Allogeneic")
        ) |>
          # remove date range where stemtrak audit trail was broken
          filter(date < "2019-03-12" | date >= "2020-06-01")
      }
    )

    out$scheduled_admissions <- tibble(
      id = scheduled_admissions_nested$id,
      data = scheduled_admissions_out
    )
  }

  out
}

make_admission_splits <- function(
  data,
  h,
  skip = "2 months",
  slice_limit = 15
) {
  make_census_ts_splits(
    data = data,
    h = h,
    skip = skip,
    slice_limit = slice_limit,
    extend = 1
  )
}

make_census_ts_splits <- function(
  data,
  h,
  skip = "2 months",
  slice_limit = 15,
  extend = 0
) {
  assess <- paste(h + extend, "days")

  out <- time_series_cv(
    data,
    date_var = date,
    assess = assess,
    skip = skip,
    cumulative = TRUE,
    slice_limit = slice_limit
  )

  # relabel splits. Need to use manual_rset since ids are also stored
  # at the split level
  ids <- map_vec(out$splits, ~ min(pull(testing(.), "date")))

  manual_rset(splits = out$splits, ids = factor(ids))
}

make_discharge_splits <- function(data, dates) {
  splits <- map(dates, \(x) make_one_discharge_split(x, data = data))

  manual_rset(splits, ids = factor(dates))
}

make_one_discharge_split <- function(cv_date, data) {
  temp_data <- mutate(data, .idx = row_number())

  test_data <- temp_data |>
    filter(min(date) == cv_date, .extended == TRUE, .by = .row)

  train_data <- temp_data |>
    filter(!mrn %in% test_data$mrn, date < cv_date, is.na(.extended))

  make_splits(
    list(analysis = train_data$.idx, assessment = test_data$.idx),
    data = data
  )
}

# DISCHARGE RESAMPLE

#' @export
fit_discharge_resamples <- function(wflowset, splits) {
  res <- workflow_map(
    wflowset,
    resamples = splits$discharge,
    control = control_resamples(save_pred = TRUE, extract = identity, verbose = TRUE)
  )

  preds <- collect_predictions(res, summarize = FALSE)

  preds <- bind_cols(
    preds,
    splits$data |>
      select(time, date, .src_row = .row, mrn, csn) |>
      slice(preds$.row)
  ) |>
    calc_survival_prob(by = c(wflow_id, .config, id, .src_row)) |>
    mutate(.h = row_number(), .by = c(wflow_id, .config, id, .src_row))

  results <- preds |>
    summarize(
      .pred = sum(.pred_survival),
      .actual = sum(discharge == "No"),
      .by = c(wflow_id, .config, id, .h)
    ) |>
    nest(.by = c(wflow_id, .config, id), .key = "results")

  extracts <- res |>
    mutate(extracts = map(result, collect_extracts)) |>
    select(wflow_id, extracts) |>
    unnest(extracts)

  fits <- extracts |>
    select(wflow_id, .config, id, fit = .extracts)

  hyperparams <- extracts |>
    distinct(pick(-c(id, .extracts))) |>
    nest(.by = c(wflow_id, .config), .key = "hyperparams")

  preds <- preds |>
    nest(.by = c(wflow_id, .config, id), .key = "pred")

  results |>
    left_join(fits, by = c("wflow_id", ".config", "id")) |>
    left_join(preds, by = c("wflow_id", ".config", "id")) |>
    left_join(hyperparams, by = c("wflow_id", ".config")) |>
    rename(discharge_model = wflow_id, discharge_config = .config)
}

# ADMISSION RESAMPLE

#' @export
fit_admission_resamples <- function(model_tbl, splits, verbose = TRUE) {
  res <- modeltime_fit_resamples(
    model_tbl,
    resamples = splits,
    control = control_resamples(verbose = verbose)
  )

  # Inner loop over CV folds
  inner_loop_impl <- function(splits, id, .predictions, ...) {
    test_data <- testing(splits)
    bind_cols(test_data, .predictions[".pred"]) |>
      mutate(id = id, .h = dense_rank(date) - 1) |>
      # align with modeltime_forecast() results
      rename(.value = .pred)
  }

  # Outer loop over models
  out <- res |>
    mutate(.resample_results = map(
      .resample_results,
      .f = function(x) {
        pmap(x, .f = inner_loop_impl) |>
          bind_rows()
      }
    )) |>
    select(.model_id, any_of(c(".config", "hyperparams")), .resample_results)

  if (!".config" %in% colnames(out)) {
    out$.config <- 1
  }

  if (!"hyperparams" %in% colnames(out)) {
    out$hyperparams <- list(tibble(.rows = 1))
  }

  out |>
    unnest(.resample_results) |>
    nest(.by = c(.model_id, .config, id, hyperparams), .key = "results") |>
    rename(admission_model = .model_id, admission_config = .config)
}

## COLLECT

#' @export
collect_cv_results <- function(admission_results,
                               discharge_results,
                               splits,
                               control = control_sim()) {
  # Prep for sim
  combined_results <- admission_results |>
    left_join(
      select(discharge_results, discharge_model, discharge_config, id, fit),
      by = "id",
      relationship = "many-to-many"
    )

  if (!is.null(splits$scheduled_admissions)) {
    combined_results <- combined_results |>
      left_join(
        splits$scheduled_admissions,
        by = "id"
      ) |>
      mutate(results = map2(
        results,
        data,
        \(x, y) left_join(x, y, by = c("date", ".h", "transplant_type"))
      )) |>
      select(-data)
  }

  admission_surv_data <- tibble(id = splits$discharge$id)
  admission_surv_data$admission_surv_data <- map(splits$discharge$splits, \(x) filter(training(x), time == 0))

  combined_results <- combined_results |>
    left_join(admission_surv_data, by = "id")

  # Run sim
  combined_results$results <- future_pmap(
    select(combined_results, results, fit, admission_surv_data),
    .f = wrap_sim_admission_discharge,
    control = control,
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )

  # Post-process results
  discharge <- discharge_results |>
    select(discharge_model, discharge_config, id, hyperparams, results) |>
    unnest(results) |>
    select(
      discharge_model,
      discharge_config,
      discharge_hyperparams = hyperparams,
      id,
      .h,
      .actual_current_admission = .actual,
      .pred_current_admission = .pred
    )

  admission_pred <- combined_results |>
    select(admission_model, admission_config, discharge_model, discharge_config, id, results) |>
    unnest(results) |>
    select(admission_model, admission_config, discharge_model, discharge_config, id, .h, .pred_new_admission = .pred)

  admission_actual <- admission_results |>
    filter(dense_rank(admission_model) == 1, dense_rank(admission_config) == 1) |>
    select(id, results) |>
    mutate(results = map(results, extract_actual_new_admission)) |>
    unnest(results) |>
    select(id, .h, .actual_new_admission = .actual)

  # Combine

  combined_results |>
    select(admission_model, admission_config, discharge_model, discharge_config, id) |>
    left_join(discharge, by = c("discharge_model", "discharge_config", "id"), relationship = "many-to-many") |>
    left_join(admission_pred, by = c("admission_model", "admission_config", "discharge_model", "discharge_config", "id", ".h")) |>
    left_join(admission_actual, by = c("id", ".h")) |>
    left_join(
      select(admission_results, admission_model, admission_config, id, admission_hyperparams = hyperparams),
      by = c("admission_model", "admission_config", "id")
    ) |>
    mutate(
      .actual_census = .actual_current_admission + .actual_new_admission,
      .pred_census = .pred_current_admission + .pred_new_admission
    ) |>
    nest(.by = c(
      admission_model,
      admission_config,
      admission_hyperparams,
      discharge_model,
      discharge_config,
      discharge_hyperparams,
      id
    ), .key = "results")
}

extract_actual_new_admission <- function(results) {
  results |>
    filter(date == min(date)) |>
    select(transplant_type, census_contribution) |>
    unnest(census_contribution) |>
    rename(.h = h) |>
    summarize(.actual = sum(actual), .by = .h)
}

wrap_sim_admission_discharge <- function(results, fit, admission_surv_data, control) {
  sim_admission_discharge(
    pred_admission = results,
    model = fit,
    admission_surv_data = admission_surv_data,
    control = control
  )
}
