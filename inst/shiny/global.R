# Global variables and setup for Shiny dashboard
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)

# Set options
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size

# Global constants
REPLACEMENT_COST_PER_EMPLOYEE <- 75000
TARGET_ATTRITION_RATE <- 0.12
TARGET_SATISFACTION <- 3.5
TARGET_DELIVERY_RATE <- 0.85

# Color palettes
RISK_COLORS <- c("Low" = "#34dbcb", "Medium" = "#98db34", "High" = "#db3498")
DEPT_COLORS <- c("#0f3a57", "#175782", "#1f74ad", "#2691d9", "#52a7e0", "#7dbde8")

# Helper function for dashboard
format_percentage <- function(x) {
  paste0(round(x * 100, 1), "%")
}

# Check for sample data and generate if needed
ensure_sample_data <- function() {
  data_dir <- file.path(system.file("shiny", package = "GlobalTechAnalytics"), "data")

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  if (!file.exists(file.path(data_dir, "employee_data.csv"))) {
    sample_data <- GlobalTechAnalytics::generate_globaltech_data(14940, 523, output_dir = data_dir)
  }
}

# Ensure sample data exists when app loads
ensure_sample_data()
