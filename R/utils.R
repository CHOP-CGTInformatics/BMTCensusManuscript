#' @export
save_data <- function(data, path, quiet = TRUE) {
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }

  saveRDS(data, path)

  if (!quiet) {
    message("Saving data to ", path)
  }

  invisible(data)
}

add_discharge_outcome <- function(data, end_var) {
  data |>
    mutate(
      discharge = case_when(
        date >= Sys.Date() ~ NA,
        date >= as_date({{ end_var }}) ~ "Yes",
        .default = "No"
      ),
      discharge = factor(discharge, levels = c("Yes", "No"))
    )
}

#' Default transplant types
#' @param add vector of transplant types to add
#' @param remove vector of transplant types to remove
#' @export
default_transplant_types <- function(add = NULL, remove = NULL) {
  default <- c("Autologous", "Allogeneic", "No Transplant")

  default |>
    c(add) |>
    setdiff(remove) |>
    unique()
}

#' Create a `bmt_model`
#' @param discharge_model discharge model
#' @param admission_model admission model
#' @param admission_surv_data survival curves for simulating admission-discharge process
#' @export
bmt_model <- function(discharge_model, admission_model, admission_surv_data = default_admission_surv_data()) {
  out <- list(
    discharge = discharge_model,
    admission = admission_model,
    admission_surv_data = admission_surv_data
  )

  class(out) <- c("bmt_model", class(out))

  out
}
