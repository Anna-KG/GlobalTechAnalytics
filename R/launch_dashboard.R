#' @name launch_dashboard
#' @title Launch GlobalTech Analytics Dashboard

globalVariables(c(
  "age", "attrition_risk_score", "avg_performance", "avg_risk",
  "avg_satisfaction", "base_salary", "budget_variance_pct",
  "client_satisfaction", "delivered_on_time", "last_promotion_months_ago",
  "skill_count", "skills", "status", "total_cost", "total_count",
  "total_employees", "training_hours_last_year", "work_arrangement",
  "left_company", "department", "tenure_years", "job_satisfaction",
  "performance_rating", "employee_id", "host"
))

#' Launch GlobalTech Analytics Dashboard
#'
#' @description Launch the interactive dashboard
#' @param port Port number for the dashboard (default: 3838)
#' @param host Host address (default: "127.0.0.1")
#' @param sample_data Whether to use sample data (default: FALSE)
#' @export
launch_dashboard <- function(port = 3838, host = "127.0.0.1", sample_data = FALSE) {

  # Check required packages
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("Package 'shinydashboard' is required but not installed.")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required but not installed.")
  }

  # Load required libraries
  library(shiny)
  library(shinydashboard)
  library(DT)

  # Check if Shiny app exists
  app_dir <- system.file("shiny", package = "GlobalTechAnalytics")
  if (!dir.exists(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Shiny dashboard not found. Please ensure the package is properly installed.")
  }

  # Generate sample data if needed
  if (sample_data) {
    data_dir <- file.path(app_dir, "data")
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE)
    }

    if (!file.exists(file.path(data_dir, "employee_data.csv"))) {
      sample_data_obj <- generate_globaltech_data(
        n_employees = 1000,
        n_projects = 50,
        output_dir = data_dir
      )
    }
  }

  # Launch the Shiny app
  shiny::runApp(app_dir, port = port, host = host, launch.browser = TRUE)
}

#' Get Dashboard URL
#'
#' @param port Port number
#' @param host Host address
#' @return Character string with dashboard URL
#' @export
get_dashboard_url <- function(port = 3838, host = "127.0.0.1") {
  paste0("http://", host, ":", port)
}
