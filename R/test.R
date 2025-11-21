#' @export
make_test_data <- function(test_dates, discharge_test, admission_test, scheduled_admissions) {
  current_admissions_test <- discharge_test |>
    filter(date %in% test_dates, time > 0) |>
    mutate(test_date = date) |>
    nest(.by = test_date, .key = "current_admissions")

  scheduled_admissions_test <- scheduled_admissions |>
    filter(date %in% test_dates) |>
    rename(test_date = date, date = admission_date) |>
    nest(.by = test_date, .key = "scheduled_admissions") |>
    mutate(scheduled_admissions = map2(
      test_date,
      scheduled_admissions,
      .f = \(test_date, scheduled_admissions, ...) {
        prep_admission_data(
          day_0 = test_date,
          h = 30,
          scheduled_admissions = scheduled_admissions,
          na_zero_transplant_types = default_transplant_types(remove = "No Transplant")
        ) |>
          select(date, transplant_type, scheduled)
      }
    ))

  actual_new_admission <- admission_test |>
    filter(date %in% test_dates) |>
    mutate(test_date = date) |>
    nest(.by = test_date, .key = "admission_data") |>
    mutate(admission_data = map(admission_data, extract_actual_new_admission)) |>
    unnest(admission_data) |>
    rename(.actual_new_admission = .actual)

  actual_current_admission <- current_admissions_test |>
    unnest(current_admissions) |>
    extend_discharge_data(90, bmt_team_end_date) |>
    summarize(.actual_current_admission = sum(discharge == "No"), .by = c(test_date, .h))

  actual <- left_join(actual_new_admission, actual_current_admission, by = c("test_date", ".h")) |>
    mutate(.actual_census = .actual_new_admission + .actual_current_admission) |>
    nest(.by = test_date, .key = "actual")

  left_join(
    current_admissions_test,
    scheduled_admissions_test,
    by = "test_date"
  ) |>
    left_join(actual, by = "test_date")
}