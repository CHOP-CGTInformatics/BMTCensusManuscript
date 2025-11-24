#' @export
scale_color_chop <- function(...) {
  discrete_scale(
    aesthetics = "colour",
    palette = chop_pal(),
    ...
  )
}

#' @export
scale_fill_chop <- function(...) {
  discrete_scale(
    aesthetics = "fill",
    palette = chop_pal(),
    ...
  )
}

chop_pal <- function() {
  function(n) {
    colors <- chop_colors()
    unname(colors[seq_len(n)])
  }
}

#' @export
chop_colors <- function(...) {
  if (missing(...)) {
    colors <- unique(color_table$hex)
  } else {
    colors_concat <- c(...)

    clean_names <- gsub("[[:punct:]]", "", color_table$full_nm)
    clean_choice <- gsub("[[:punct:]]", "", colors_concat)
    colors_clean <- setNames(color_table$hex, clean_names)

    colors <- unname(colors_clean[clean_choice])
  }
  colors
}