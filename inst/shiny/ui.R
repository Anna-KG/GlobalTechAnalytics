library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Resource Analytics"
  ),

  dashboardSidebar(
    # Main navigation menu ----
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Executive Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Attrition Analysis", tabName = "attrition", icon = icon("user-times")),
      menuItem("Resource Optimization", tabName = "resources", icon = icon("cogs")),
      menuItem("Project Analytics", tabName = "projects", icon = icon("project-diagram"))
    ),

    # Filter section ----
    br(),
    div(
      class = "filter-section",
      h4("Filters", style = "color: #485785; font-weight: 600; margin-bottom: 15px; text-align: left; font-size: 18px;"),

      # Department filter dropdown ----
      selectInput(
        inputId = "dept_filter",
        label = "Department:",
        choices = TRUE,
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),

      # Risk threshold slider ----
      sliderInput(
        inputId = "risk_threshold",
        label = "Threshold (for attrition analysis):",
        min = 0,
        max = 1,
        value = 0.7,
        step = 0.1
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Import Nunito font */
        @import url('https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700&display=swap');

        /* Soft Morph theme color variables */
        :root {
          --morph-bg: #e8f0fe;
          --morph-surface: #f4f8fc;
          --morph-primary: #4285f4;
          --morph-secondary: #6c63ff;
          --morph-text: #2c3e50;
          --morph-text-light: #5a6c7d;
          --morph-success: #34a853;
          --morph-warning: #fbbc04;
          --morph-danger: #ea4335;
          --morph-info: #4285f4;
        }

        /* Larger font sizes */
        body {
          font-family: 'Nunito', sans-serif !important;
          background: var(--morph-bg) !important;
          color: var(--morph-text) !important;
          font-size: 16px !important;
        }

        /* Header */
        .main-header .navbar {
          background: var(--morph-primary) !important;
          border: none !important;
        }

        .main-header .navbar-brand,
        .main-header .navbar .nav-link {
          color: white !important;
          font-weight: 600 !important;
          font-size: 18px !important;
        }

        /* Sidebar toggle button positioning */
        .main-header .navbar .sidebar-toggle {
          float: left !important;
          margin-right: 15px !important;
          margin-left: 0 !important;
        }

        /* Sidebar */
        .main-sidebar {
          background: linear-gradient(180deg, var(--morph-text) 0%, var(--morph-primary) 100%) !important;
        }

        .sidebar-menu > li > a {
          color: rgba(255,255,255,0.9) !important;
          border-radius: 10px !important;
          margin: 3px 8px !important;
          transition: all 0.3s ease !important;
          font-size: 16px !important;
        }

        .sidebar-menu > li:hover > a,
        .sidebar-menu > li.active > a {
          background: rgba(255,255,255,0.15) !important;
          color: white !important;
        }

        /* Content wrapper */
        .content-wrapper {
          background: var(--morph-bg) !important;
          min-height: 100vh !important;
        }

        /* Filter section - no shadows, clean look */
        .filter-section {
          background: var(--morph-surface) !important;
          border-radius: 15px !important;
          padding: 20px !important;
          margin: 15px !important;
          border: 2px solid rgba(66, 133, 244, 0.1) !important;
        }

        /* Boxes - clean flat design */
        .box {
          border-radius: 15px !important;
          border: 2px solid rgba(66, 133, 244, 0.1) !important;
          background: var(--morph-surface) !important;
          transition: all 0.3s ease !important;
        }

        .box-header {
          background: var(--morph-surface) !important;
          border-radius: 15px 15px 0 0 !important;
          border-bottom: 2px solid rgba(66, 133, 244, 0.1) !important;
          padding: 20px !important;
        }

        .box-header h3 {
          font-weight: 600 !important;
          color: var(--morph-text) !important;
          font-size: 22px !important;
        }

        .box-body {
          padding: 20px !important;
        }

        /* Value boxes - soft colors */
        .small-box {
          border-radius: 15px !important;
          border: none !important;
          overflow: hidden !important;
          transition: all 0.3s ease !important;
        }

        /* Soft value box colors */
        .small-box.bg-navy,
        .small-box.bg-blue {
          background: var(--morph-info) !important;
        }

        .small-box.bg-red {
          background: var(--morph-danger) !important;
        }

        .small-box.bg-orange,
        .small-box.bg-yellow {
          background: var(--morph-warning) !important;
        }

        .small-box.bg-green {
          background: var(--morph-success) !important;
        }

        .small-box .inner h3 {
          color: white !important;
          font-weight: 700 !important;
          font-size: 36px !important;
        }

        .small-box .inner p {
          color: white !important;
          font-weight: 600 !important;
          font-size: 18px !important;
        }

        .small-box .icon {
          opacity: 0.3 !important;
          color: white !important;
        }

        /* Info boxes */
        .info-box {
          border-radius: 15px !important;
          border: 2px solid rgba(66, 133, 244, 0.1) !important;
          background: var(--morph-surface) !important;
          transition: all 0.3s ease !important;
        }

        .info-box-icon {
          border-radius: 15px 0 0 15px !important;
          font-size: 24px !important;
        }

        /* Soft info box icon colors */
        .bg-red .info-box-icon {
          background: var(--morph-danger) !important;
        }

        .bg-orange .info-box-icon {
          background: var(--morph-warning) !important;
        }

        .bg-blue .info-box-icon {
          background: var(--morph-info) !important;
        }

        .info-box-content {
          color: var(--morph-text) !important;
          padding: 15px !important;
        }

        .info-box-number {
          color: var(--morph-text) !important;
          font-weight: 700 !important;
          font-size: 28px !important;
        }

        .info-box-text {
          color: var(--morph-text-light) !important;
          font-size: 16px !important;
          font-weight: 600 !important;
        }

        /* Form controls */
        .form-control,
        .selectize-input {
          border-radius: 10px !important;
          border: 2px solid rgba(66, 133, 244, 0.2) !important;
          background-color: var(--morph-surface) !important;
          padding: 12px 16px !important;
          transition: all 0.3s ease !important;
          color: var(--morph-text) !important;
          font-size: 16px !important;
        }

        .form-control:focus,
        .selectize-input.focus {
          border-color: var(--morph-primary) !important;
          background-color: white !important;
          outline: none !important;
        }

        /* Labels */
        .control-label {
          color: var(--morph-text) !important;
          font-weight: 600 !important;
          font-size: 16px !important;
        }

        /* Slider styling */
        .irs--shiny .irs-bar {
          background: var(--morph-primary) !important;
          border-radius: 5px !important;
        }

        .irs--shiny .irs-handle {
          background: white !important;
          border: 3px solid var(--morph-primary) !important;
          border-radius: 50% !important;
        }

        .irs--shiny .irs-line {
          background: rgba(66, 133, 244, 0.2) !important;
          border-radius: 5px !important;
        }

        /* DataTables */
        .dataTables_wrapper {
          font-family: 'Nunito', sans-serif !important;
          color: var(--morph-text) !important;
          font-size: 16px !important;
        }

        .dataTables_wrapper .dataTables_length select,
        .dataTables_wrapper .dataTables_filter input {
          border-radius: 8px !important;
          border: 2px solid rgba(66, 133, 244, 0.2) !important;
          background-color: var(--morph-surface) !important;
          padding: 8px 12px !important;
          color: var(--morph-text) !important;
          font-size: 14px !important;
        }

        .dataTables_wrapper .dataTables_paginate .paginate_button {
          border-radius: 8px !important;
          margin: 0 2px !important;
          border: 1px solid rgba(66, 133, 244, 0.2) !important;
          background: var(--morph-surface) !important;
          color: var(--morph-text) !important;
          font-size: 14px !important;
        }

        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: var(--morph-primary) !important;
          color: white !important;
          border-color: var(--morph-primary) !important;
        }

        /* Table styling */
        .table {
          border-radius: 10px !important;
          overflow: hidden !important;
          font-size: 16px !important;
        }

        .table thead th {
          background: var(--morph-primary) !important;
          color: white !important;
          border: none !important;
          font-weight: 600 !important;
          padding: 15px !important;
          font-size: 16px !important;
        }

        .table tbody tr {
          background-color: var(--morph-surface) !important;
          color: var(--morph-text) !important;
        }

        .table tbody tr:nth-child(even) {
          background-color: rgba(244, 248, 252, 0.8) !important;
        }

        .table tbody tr:hover {
          background-color: rgba(66, 133, 244, 0.1) !important;
          transform: translateY(-1px) !important;
          transition: all 0.3s ease !important;
        }

        .table tbody td {
          padding: 12px 15px !important;
          font-size: 15px !important;
        }

        /* Plotly charts */
        .plotly {
          border-radius: 10px !important;
          overflow: hidden !important;
        }

        /* Hover animations - subtle */
        .small-box:hover,
        .info-box:hover,
        .box:hover {
          transform: translateY(-2px) !important;
        }

        /* Responsive design */
        @media (max-width: 768px) {
          .small-box, .info-box, .box {
            margin-bottom: 15px !important;
          }

          .filter-section {
            margin: 10px !important;
            padding: 15px !important;
          }

          .small-box .inner h3 {
            font-size: 28px !important;
          }

          .small-box .inner p {
            font-size: 16px !important;
          }
        }

        /* Custom scrollbar */
        ::-webkit-scrollbar {
          width: 8px !important;
          height: 8px !important;
        }

        ::-webkit-scrollbar-track {
          background: var(--morph-bg) !important;
          border-radius: 4px !important;
        }

        ::-webkit-scrollbar-thumb {
          background: var(--morph-primary) !important;
          border-radius: 4px !important;
        }

        ::-webkit-scrollbar-thumb:hover {
          background: var(--morph-secondary) !important;
        }

        /* Content headers larger fonts */
        .content-header h1 {
          font-size: 28px !important;
          font-weight: 600 !important;
        }

        /* Remove any remaining shadows */
        * {
          box-shadow: none !important;
          -webkit-box-shadow: none !important;
          -moz-box-shadow: none !important;
        }
      "))
    ),

    # Main content tabs: ----
    tabItems(
      ## Executive Overview tab ----
      tabItem(
        tabName = "overview",
        ### Key metrics row ----
        fluidRow(
          valueBoxOutput("total_employees", width = 3),
          valueBoxOutput("attrition_rate", width = 3),
          valueBoxOutput("avg_satisfaction", width = 3),
          valueBoxOutput("project_success_rate", width = 3)
        ),
        ### Charts row ----
        fluidRow(
          box(
            title = "Department Distribution",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dept_chart")
          ),
          box(
            title = "Risk Distribution",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("risk_distribution")
          )
        ),
        ### KPI table row ----
        fluidRow(
          box(
            title = "Key Performance Indicators",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("kpi_table")
          )
        ),
        ## Executive summary row ----
        fluidRow(
          box(
            title = "Executive Summary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("executive_summary")
          )
        )
      ),

      # Attrition Analysis tab ----
      tabItem(
        tabName = "attrition",
        # Attrition metrics row ----
        fluidRow(
          infoBoxOutput("high_risk_count", width = 4),
          infoBoxOutput("estimated_cost", width = 4),
          infoBoxOutput("avg_tenure_leaving", width = 4)
        ),
        # Risk analysis charts row ----
        fluidRow(
          box(
            title = "Risk Factors Analysis",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("risk_factors_chart")
          ),
          box(
            title = "Department Risk Comparison",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dept_risk_chart")
          )
        ),
        # High-risk employees table row ----
        fluidRow(
          box(
            title = "High-Risk Employees",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("high_risk_table")
          )
        )
      ),

      # Resource Optimization tab ----
      tabItem(
        tabName = "resources",
        ## Resource metrics row ----
        fluidRow(
          valueBoxOutput("total_capacity", width = 3),
          valueBoxOutput("utilization_rate", width = 3),
          valueBoxOutput("skills_coverage", width = 3),
          valueBoxOutput("cost_efficiency", width = 3)
        ),
        ## Resource analysis row ----
        fluidRow(
          box(
            title = "Resource Utilization by Department",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("utilization_chart")
          ),
          box(
            title = "Top Skills",
            solidHeader = TRUE,
            width = 4,
            DT::dataTableOutput("skills_table")
          )
        ),
        ## Optimization recommendations row ----
        fluidRow(
          box(
            title = "Optimization Recommendations",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("optimization_table")
          )
        )
      ),

      # Project Analytics tab ----
      tabItem(
        tabName = "projects",
        ## Project metrics row ----
        fluidRow(
          infoBoxOutput("total_projects", width = 3),
          infoBoxOutput("active_projects", width = 3),
          infoBoxOutput("on_time_delivery", width = 3),
          infoBoxOutput("avg_budget_variance", width = 3)
        ),
        ## Project analysis charts row ----
        fluidRow(
          box(
            title = "Project Success by Type",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("project_success_chart")
          ),
          box(
            title = "Budget vs Timeline Performance",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("budget_timeline_chart")
          )
        ),
        ## Project portfolio table row ----
        fluidRow(
          box(
            title = "Project Portfolio Overview",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("projects_table")
          )
        )
      )
    )
  )
)
