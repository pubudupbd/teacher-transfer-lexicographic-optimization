# Package loading and helper functions

load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

load_or_install("lpSolve")
load_or_install("ggplot2")

safe_scale_to_int <- function(x, scale = 100L) {
  as.integer(round(scale * x))
}

check_required_columns <- function(df, cols, df_name) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing columns in %s: %s",
      df_name, paste(missing_cols, collapse = ", ")
    ))
  }
}

make_type_key <- function(subject, medium) {
  paste(subject, medium, sep = "__")
}

euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

movement_indicator <- function(destination_school, current_school) {
  as.integer(destination_school != current_school)
}

assign_weights <- function(schools, scenario = "S3") {
  scenario <- toupper(scenario)

  if (scenario == "S1") {
    schools$w_j <- 1L
  } else if (scenario == "S2") {
    schools$w_j <- ifelse(schools$underserved == 1L, 2L, 1L)
  } else if (scenario == "S3") {
    schools$w_j <- ifelse(schools$underserved == 1L, 3L, 1L)
  } else if (scenario == "S4") {
    schools$w_j <- ifelse(schools$underserved == 1L, 5L, 1L)
  } else {
    stop("Unknown scenario. Use one of: 'S1', 'S2', 'S3', 'S4'.")
  }

  schools$weight_scenario <- scenario
  schools
}
