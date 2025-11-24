# these colors are based on the CHOP style guide: http://www.chop.edu/design/
color_table <- read.csv("data-raw/rocqi_colors.csv", stringsAsFactors = FALSE)

usethis::use_data(color_table, overwrite = TRUE)
