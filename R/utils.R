#' @title Package Utilities and Helper Functions
#' @description Common utility functions used throughout the package
#' @name utils

# Suppress R CMD check notes about global variables
utils::globalVariables(c(".", "Cost", "Department", "Total_Cost", "employee_id",
                         "project_id", "left_company", "department", "tenure_years",
                         "job_satisfaction", "performance_rating", "is_active",
                         "n", "dplyr"))

#' Validate DataFrame Structure
#'
#' @description Validates that a dataframe meets expected criteria
#' @param df DataFrame to validate
#' @param required_cols Character vector of required column names
#' @param min_rows Minimum number of rows required
#' @return Logical indicating if validation passed
#' @export
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5])
#' validate_dataframe(df, c("a", "b"), min_rows = 3)
validate_dataframe <- function(df, required_cols, min_rows = 1) {

  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  if (nrow(df) < min_rows) {
    stop(paste("DataFrame must have at least", min_rows, "rows"))
  }

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  return(TRUE)
}

#' Setup Logger
#'
#' @description Creates a simple logging function
#' @param name Logger name
#' @return List with logging functions
#' @export
#' @examples
#' logger <- setup_logger("MyApp")
#' logger$info("This is an info message")
setup_logger <- function(name = "GlobalTechAnalytics") {
  list(
    info = function(msg) cat("[INFO]", Sys.time(), "-", name, ":", msg, "\n"),
    warning = function(msg) cat("[WARN]", Sys.time(), "-", name, ":", msg, "\n"),
    error = function(msg) cat("[ERROR]", Sys.time(), "-", name, ":", msg, "\n")
  )
}

#' Calculate Business Days
#'
#' @description Calculate business days between two dates
#' @param start_date Start date
#' @param end_date End date
#' @return Number of business days
#' @export
#' @examples
#' start_date <- as.Date("2023-01-02")
#' end_date <- as.Date("2023-01-06")
#' calculate_business_days(start_date, end_date)
calculate_business_days <- function(start_date, end_date) {
  if (!inherits(start_date, "Date") || !inherits(end_date, "Date")) {
    stop("Both dates must be Date objects")
  }

  if (start_date >= end_date) {
    return(0)
  }

  # Calculate all days between dates (exclusive of start_date, inclusive of end_date)
  all_days <- seq(from = start_date + 1, to = end_date, by = "day")

  # Filter out weekends
  weekdays_only <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]

  return(length(weekdays_only))
}

#' Format Currency
#'
#' @description Format numbers as currency
#' @param x Numeric vector
#' @param currency Currency symbol
#' @return Character vector with formatted currency
#' @export
#' @examples
#' format_currency(1000)
#' format_currency(c(1000, 2500), "EUR ")
format_currency <- function(x, currency = "EUR ") {
  paste0(currency, scales::comma(round(x, 0)))
}

#' Calculate Percentile
#'
#' @description Calculate percentile rank within a group
#' @param x Numeric vector
#' @return Numeric vector of percentile ranks (0-100)
#' @export
#' @examples
#' values <- c(10, 20, 30, 40, 50)
#' calculate_percentile(values)
calculate_percentile <- function(x) {
  round(ecdf(x)(x) * 100, 0)
}
