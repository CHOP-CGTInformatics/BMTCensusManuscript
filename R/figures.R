#' @export
generate_bootstraps <- function(data,
                                pred = .pred_census,
                                actual = .actual_census,
                                index = prediction_date,
                                n_boot = 500,
                                bootstrap_block_size = 4) {
  series <- data |>
    mutate(value = {{ actual }} - {{ pred }}) |>
    select({{ index }}, .h, value, {{ actual }}, {{ pred }}) |>
    as_tsibble(index = {{ index }}, key = .h)

  if (any(has_gaps(series)$.gaps)) {
    cli_warn("Gaps detected in series. Filling with mean")

    series <- series |>
      group_by_key() |>
      fill_gaps(value = mean(value)) |>
      ungroup()
  }

  stl <- series |>
    model(stl = STL(value))

  boot <- stl |>
    generate(new_data = series, times = n_boot, bootstrap_block_size = bootstrap_block_size) |>
    rename(.sim_resid = .sim) |>
    mutate(.sim_pred = .actual_census - .sim_resid)

  structure(
    list(stl = stl, boot = boot),
    index = enquo(index),
    actual = enquo(actual)
  )
}

#' @export
summarize_bootstrap <- function(result, actual_metrics = NULL) {
  mset <- metric_set(rmse, mape)
  actual <- attr(result, "actual")

  res_summary <- result$boot |>
    group_by(.h, .rep) |>
    mset({{ actual }}, .sim_pred)

  if (!is.null(actual_metrics)) {
    res_summary <- res_summary |>
      left_join(actual_metrics, by = c(".h", ".metric")) |>
      mutate(res_summary, d = .estimate - value)
  }

  quantiles <- c(.01, .05, .5, .95, .99)

  table <- res_summary |>
    reframe(.estimate = quantile(.estimate, quantiles), q = quantiles, .by = c(.h, .metric)) |>
    pivot_wider(id_cols = c(.h, .metric), names_from = q, values_from = .estimate)

  if (!is.null(actual_metrics)) {
    table_empirical <- res_summary |>
      reframe(d = quantile(d, 1 - quantiles), q = quantiles, .by = c(.h, .metric, value)) |>
      mutate(.estimate = value - d) |>
      pivot_wider(id_cols = c(.h, .metric), names_from = q, values_from = .estimate)
  }

  out <- list(table = table, plot = plot)

  if (!is.null(actual_metrics)) {
    out$table_empirical <- table_empirical
  }

  out
}

#' @export
make_metrics_table <- function(data, h = c(15, 30, 60)) {
  mset <- metric_set(rmse, mape)

  data |>
    filter(.h %in% h) |>
    pivot_longer(
      matches("\\.(actual|pred)_"),
      names_to = c(".value", "stat"),
      names_pattern = "\\.(actual|pred)_(.+)"
    ) |>
    group_by(stat, .h) |>
    mset(actual, pred) |>
    mutate(.metric = toupper(.metric)) |>
    pivot_wider(id_cols = c(stat, .h), names_from = .metric, values_from = .estimate) |>
    mutate(stat = case_match(
      stat,
      "census" ~ "Total Census",
      "current_admission" ~ "Admitted Before Prediction Time",
      "new_admission" ~ "Admitted After Prediction Time"
    )) |>
    select(Horizon = .h, Component = stat, RMSE, MAPE)
}

#' @export
plot_results <- function(data,
                         pred,
                         actual,
                         title = NULL,
                         scales = "fixed") {
  data |>
    filter(.h %in% c(15, 30, 60)) |>
    rename(Predicted = {{ pred }}, Actual = {{ actual }}, `Prediction Horizon (Days)` = .h) |>
    pivot_longer(c(Predicted, Actual)) |>
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    facet_wrap(~`Prediction Horizon (Days)`, ncol = 1, labeller = "label_both", scales = scales) +
    scale_color_chop() +
    labs(
      y = "Midnight Census",
      x = NULL,
      color = NULL,
      title = title
    ) +
    scale_x_date(date_labels = "%b '%y")
}
