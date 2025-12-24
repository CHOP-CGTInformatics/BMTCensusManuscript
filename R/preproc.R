#' @export
discharge_rec <- function(data) {
  data |>
    select(csn, mrn, date, transplant_type, time, discharge) |>
    recipe(formula = discharge ~ .) |>
    update_role(c(csn, mrn, date), new_role = "id") |>
    update_role_requirements("id", bake = FALSE) |>
    step_discretize(time, num_breaks = 15) |>
    step_dummy(all_nominal_predictors())
}

#' @export
admission_rec <- function(data, type = "poisson") {
  allowed_types <- c("poisson", "xgb")

  if (!type %in% allowed_types) {
    cli_abort(c(
      "x" = "{.code type} must be one of {allowed_types}",
      "i" = "{.code type} is {type}"
    ))
  }

  out <- data |>
    select(date, any_of("transplant_type"), value) |>
    recipe(formula = value ~ .) |>
    step_timeseries_signature(date)

  if (type == "poisson") {
    out <- out |>
      # keep month/week features
      step_rm(
        date,
        starts_with("date_") & !any_of(c("date_month.lbl", "date_wday.lbl"))
      )
  } else {
    out <- out |>
      # keep sub-yearly features
      step_rm(
        date,
        date_index.num,
        starts_with("date_year"),
        ends_with(".iso"),
        ends_with(".xts")
      )
  }

  out |>
    step_zv(all_predictors()) |>
    step_dummy(all_nominal_predictors())
}
