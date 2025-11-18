#' Predict BMT census
#'
#' Produce a prediction of the BMT census `h` days into the future using admission and discharge
#' models, the current census, and optionally upcoming scheduled admissions.
#'
#' @param models named list containing `discharge`, a workflow for predicting hospital discharges;
#' `admission`, a workflow for predicting new admissions; and `admission_surv_data`, training data
#' to compute survival probabilities for new admissions
#' @param h numeric vector of future times to predict
#' @param current_census patient-level dataframe containing currently admitted patients
#' @param scheduled_admissions dataframe containing scheduled admissions
#' @param control a control object
#' @param type 'census' or 'survival'
#' 
#' @returns dataframe of prediction for current census component, new admission component, and total census
#'
#' @details
#' The `admission_surv_data` component of `models` is only relevant if `discharge` has predictors
#' other than `transplant_type`. Otherwise it can be populated with `default_admission_surv_data()`.
#' `current_census` must contain a `date` column indicating the date of the current census. `scheduled_admissions`
#' must contain:
#' - `date`, the day for which admissions are scheduled
#' - `transplant_type`
#' - `scheduled`, the number of admissions, possibly 0
#'
#' @export
predict_census <- function(models,
                           h,
                           current_census,
                           scheduled_admissions = NULL,
                           control = control_sim(),
                           type = "census") {
  
  validate_type(type)
  
  max_h <- max(h)
  day_0 <- guess_day_0(current_census)

  # Current admissions
  pred_discharge <- predict_discharge(models$discharge, current_census, h = h, type = type)

  # New admissions
  new_data_admission <- prep_admission_data(day_0, max_h, scheduled_admissions)

  pred_admission <- predict_admission(models$admission, new_data_admission)

  pred_admission_discharge <- sim_admission_discharge(
    pred_admission = select(pred_admission, date, .h, transplant_type, .value, any_of("scheduled")),
    model = models$discharge,
    admission_surv_data = models$admission_surv_data,
    h = h,
    type = type,
    control = control
  )

  if (type == "survival") {
    
    pred_admission_discharge <- pred_admission_discharge |>
      # Fill in start time of 0 since these are predicted new admits
      # Recalculate date since sim_admission_discharge isn't date aware
      mutate(time = 0, current_or_new = "new", date = .h_start + day_0) |>
      select(-.h_start) |>
      # Make .rep unique within iteration
      # Currently unique within .iter, transplant_type, date but this is more intuitive
      mutate(.rep = row_number(), .by = .iter)
    
    out <- bind_rows(
      bind_cols(current_census, pred_discharge) |>
        mutate(current_or_new = "current"),
      pred_admission_discharge
    ) |>
      relocate(c(.iter, .rep), .after = csn)

    return(out)
  }

  # Combine
  tibble(
    date = day_0 + h,
    .h = h,
    .pred_current_admission = pred_discharge$.pred,
    .pred_new_admission = pred_admission_discharge$.pred
  ) |>
    mutate(.pred_census = .pred_current_admission + .pred_new_admission)
}

guess_day_0 <- function(data) {
  out <- unique(data$date)
  if (length(out) != 1) {
    cli_abort("current census should contain a single date")
  }
  out
}

#' Create frame for admission prediction
#'
#' @param day_0 date of current census
#' @param h number of days to predict
#' @param scheduled_admissions dataframe containing scheduled admissions
#' @param transplant_types vector of transplant types
#' @param na_zero_transplant_types vector of transplant types where absence of admissions are
#' converted to zero. The default makes no conversion.
#'
#' @returns dataframe with a row for each transplant_type and date between `day_0` and `day_0` + `h - 1`
#' @noRd
prep_admission_data <- function(day_0,
                                h,
                                scheduled_admissions = NULL,
                                transplant_types = default_transplant_types(),
                                na_zero_transplant_types = NULL) {
  day_0 <- as_date(day_0)

  out <- expand_grid(
    tibble(
      date = seq(day_0, by = "day", length.out = h),
      .h = 0:(h-1)
    ),
    transplant_type = transplant_types
  )

  if (!is.null(scheduled_admissions)) {
    out <- out |>
      left_join(scheduled_admissions, by = c("date", "transplant_type"))

    if (!is.null(na_zero_transplant_types)) {
      out <- out |>
        mutate(scheduled = if_else(
          transplant_type %in% na_zero_transplant_types,
          replace_na(scheduled, 0),
          scheduled
        ))
    }
  }

  out
}

## Discharge

#' Forecast current census into the future
#'
#' @param model discharge workflow
#' @param new_data dataframe containing current census
#' @param h numeric vector of future times to predict
#' @param type 'census' or 'survival'
#'
#' @returns see details
#'
#' @details
#' If `type` is 'census' a dataframe containing `.h` and `.pred` is returned where `.pred` is the
#' predicted number of remaining patients at each time point. If `type` is 'survival' a dataframe
#' with the same number of rows as `new_data` is returned where `.pred` is a list column containing
#' survival probabilities at each time.
#' @export
predict_discharge <- function(model, new_data, h, type = "census") {

  validate_type(type)

  max_h <- max(h)
  new_data <- extend_discharge_data(new_data, max_h)

  out <- augment(model, new_data = new_data, type = "prob") |>
    calc_survival_prob() |>
    filter(.h %in% .env$h)

  if (type == "survival") {
    out <- out |>
      select(.row, .h, .pred_survival) |>
      nest(.by = .row, .key = ".pred") |>
      select(.pred)

    return(out)
  }

  if (type == "census") {
    out <- out |>
      summarize(.pred = sum(.pred_survival), .by = .h)

    return(out)
  }

}

#' Calculate survival probabilities from hazards
#'
#' @param data dataframe containing `time` and `.pred_No`, 1 minus the hazard
#' @param by grouping specification to identify units
#' @noRd
calc_survival_prob <- function(data, by = .row) {
  data |>
    arrange(pick({{ by }}), time) |>
    mutate(
      .pred_survival = cumprod(.pred_No),
      .by = {{ by }}
    )
}

#' Convert encounter-level data to person-period format
#'
#' @param data dataframe minimally containing `time`
#' @param h number of periods to extend data
#' @param end_var variable denoting discharge time
#'
#' @details
#' This function assumes covariates are not time-varying and carries their values
#' forward
#' @noRd
extend_discharge_data <- function(data, h, end_var = NULL) {
  data <- mutate(data, .row = row_number())

  insert_data <- data |>
    mutate(time = map2(time + 1, time + .env$h - 1, seq)) |>
    unnest(time)

  # If date was included in input data, advance it forward
  if ("date" %in% colnames(data)) {
    insert_data <- insert_data |>
      mutate(date = date + row_number(), .by = .row)
  }

  out <- data |>
    bind_rows(insert_data) |>
    arrange(.row, time) |>
    mutate(.h = row_number(), .by = .row) |>
    mutate(.extended = TRUE)

  if (!quo_is_null(enquo(end_var))) {
    out <- add_discharge_outcome(out, {{ end_var }})
  }

  out
}

## Admission

#' Forecast admissions
#'
#' @param model admission workflow
#' @param new_data dataframe to predict, see `prep_admission_data()`
#' @returns `new_data` with `.value` column containing predicted admissions
#' @export
predict_admission <- function(model, new_data) {
  modeltime_forecast(
    model,
    new_data,
    keep_data = TRUE
  ) |>
    select(all_of(colnames(new_data)), .value)
}

#' Simulate discharges from predicted admissions
#'
#' This function takes predictions made by `predict_admission()` and a workflow
#' for predicting discharges and returns a predicted number of new admissions still
#' admitted for each future time, `h`.
#'
#' @param pred_admission admission predictions from `predict_admission()`
#' @param model discharge workflow
#' @param admission_surv_data data used to compute admission survival curves, likely training data
#' @param h numeric vector of future times to predict, inferred from `pred_admission` by default
#' @param type 'census' or 'survival'
#' @param control a control object
#'
#' @returns dataframe containing `.h` and `.pred` is returned where `.pred` is the
#' predicted number of remaining patients at each time point
#' @export
sim_admission_discharge <- function(pred_admission,
                                    model,
                                    admission_surv_data,
                                    h = NULL,
                                    type = "census",
                                    control = control_sim()) {
  
  validate_type(type)

  # Get h from pred_admission if not specified
  if (is.null(h)) {
    h <- unique(pred_admission$.h + 1)
  }

  max_h <- max(h)
  max_h <- min(max_h, control$h_trunc)
  survival_curves <- make_survival_curves(model, max_h, admission_surv_data)

  sim <- sample_admissions(pred_admission, control$n_iter) |>
    # disambiguate .h from the one that will be added by discharge model
    rename(.h_start = .h) |>
    sample_survival_curves(survival_curves)

  if (type == "survival") {
    return(select(sim, .iter, .rep, .h_start, transplant_type, .pred))
  }

  # Summarize sim results
  sim |>
    unnest(.pred) |>
    # Advance .h to prediction horizon (start + days projected forward)
    mutate(.h = .h_start + .h) |>
    filter(.h %in% .env$h) |>
    summarize(.pred = sum(.pred_survival), .by = c(.iter, .h)) |>
    # Fill zeros for iterations where we simulated no admissions
    complete(
      .iter = 1:control$n_iter,
      .h = h,
      fill = list(.pred = 0)
    ) |>
    summarize(.pred = median(.pred), .by = .h)
}

#' Admission-discharge simulation control
#'
#' @param n_iter number of iterations to use in simulation
#' @param h_trunc maximum steps ahead for simulating discharges
#'
#' @details
#' `h_trunc` should be large enough that survival probabilities are approximately zero.
#' Setting it too low will underestimate census.
#'
#' @export
control_sim <- function(n_iter = 100, h_trunc = 300) {
  list(n_iter = n_iter, h_trunc = h_trunc)
}

default_admission_surv_data <- function(transplant_types = default_transplant_types()) {
  tibble(
    transplant_type = transplant_types,
    time = 0
  )
}

make_survival_curves <- function(model, h, new_data) {
  if (any(new_data$time != 0)) {
    cli_abort("new_data contains post-admission times. Ensure time = 0 for all observations.")
  }

  # If transplant_type and time are only predictors we don't care about covaritate
  # distribution -> just keep one row per tx type
  if (setequal(colnames(new_data), c("time", "transplant_type"))) {
    new_data <- distinct(new_data)
  }

  bind_cols(
    transplant_type = new_data$transplant_type,
    predict_discharge(model, new_data, h = 1:h, type = "survival")
  )
}

sample_admissions <- function(pred_admission, n_iter) {
  tibble(
    .iter = 1:n_iter,
    .sim = map(.iter, \(x) sample_admissions_one_iter(pred_admission))
  ) |>
    unnest(.sim)
}

sample_admissions_one_iter <- function(pred_admission) {
  sample_fn <- function(...) {
    dots <- list(...)

    # Use scheduled value if pass
    n <- dots$scheduled

    # Otherwise sample
    if (is.null(n) || is.na(n)) {
      n <- rpois(1, lambda = dots$.value)
    }

    if (n == 0) {
      return(NULL)
    }

    1:n
  }

  out <- pred_admission

  out$.rep <- pmap(pred_admission, sample_fn)

  unnest(out, .rep)
}

sample_survival_curves <- function(x, y) {
  # Ignore tx types from y not in x
  y <- filter(y, transplant_type %in% x$transplant_type)

  x_idx <- split(1:nrow(x), x$transplant_type)
  y_idx <- split(1:nrow(y), y$transplant_type)
  if ("weight" %in% colnames(y)) {
    weight <- split(y$weight, y$transplant_type)
  } else {
    weight <- vector("list", length(x_idx))
  }
  samp_idx <- pmap(
    list(x_idx, y_idx, weight),
    \(x_idx, y_idx, weight) safe_sample(y_idx, size = length(x_idx), prob = weight)
  )

  x_idx_flat <- unlist(x_idx, use.names = FALSE)
  samp_idx_flat <- unlist(samp_idx, use.names = FALSE)

  samp_idx <- samp_idx_flat[order(x_idx_flat)]

  x$.pred <- y$.pred[samp_idx]
  x
}

safe_sample <- function(x, size, replace = TRUE, prob = NULL) {
  if (length(x) == 1) {
    return(rep(x, size))
  }
  sample(x, size, replace, prob)
}

validate_type <- function(type) {
  if (!type %in% c("census", "survival")) {
    cli_abort("`type` must be one of 'census' or 'survival")
  }
}
