# GlobalTech Analytics Dashboard - main Shiny ppplication

rm(list = intersect(c("generate_globaltech_data", "GlobalTechDataGenerator"), ls()))

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(GlobalTechAnalytics)
})

# Source the UI and Server
source("ui.R")
source("server.R")
source("global.R")

# Run the application
shinyApp(ui = ui, server = server)
