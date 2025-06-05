library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {

  # Load and prepare data ----
  hr_data <- reactive({
    set.seed(123)
    n <- 15647

    departments <- c("Engineering", "Product", "Data Science", "Design", "QA", "Others")

    data.frame(
      employee_id = paste0("EMP", sprintf("%05d", 1:n)),
      first_name = paste("Employee", 1:n),
      department = sample(departments, n, replace = TRUE,
                          prob = c(0.35, 0.25, 0.15, 0.1, 0.1, 0.05)),
      job_title = sample(c("Senior Engineer", "Data Scientist", "Product Manager",
                           "Designer", "QA Engineer", "Software Engineer"), n, replace = TRUE),
      tenure_years = round(runif(n, 0.5, 8), 1),
      job_satisfaction = round(runif(n, 1, 5), 1),
      performance_rating = round(runif(n, 2, 5), 1),
      attrition_risk_score = runif(n, 0, 1),
      salary = round(runif(n, 40000, 150000), 0),
      skills = sample(c("Python", "R", "JavaScript", "SQL", "React", "Java"), n, replace = TRUE),
      recommended_action = sample(c("Immediate 1:1 meeting", "Monitor and maintain",
                                    "Career development", "Retention program"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })

  # Project data ----
  project_data <- reactive({
    set.seed(456)
    n_projects <- 357

    data.frame(
      project_id = paste0("PROJ", sprintf("%03d", 1:n_projects)),
      project_name = paste("Project", LETTERS[1:n_projects]),
      project_type = sample(c("Analytics Dashboard", "Data Pipeline", "Mobile App",
                              "Web Application", "Infrastructure", "Security Enhancement"),
                            n_projects, replace = TRUE),
      status = sample(c("Completed", "Active", "Planning"), n_projects, replace = TRUE,
                      prob = c(0.6, 0.3, 0.1)),
      budget = round(runif(n_projects, 50000, 2000000), 0),
      start_date = sample(seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "day"), n_projects),
      planned_end_date = sample(seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day"), n_projects),
      delivered_on_time = sample(c("Yes", "No"), n_projects, replace = TRUE, prob = c(0.56, 0.44)),
      client_satisfaction = round(runif(n_projects, 2.5, 5), 1),
      stringsAsFactors = FALSE
    )
  })

  # Initialize department filter choices ----
  observe({
    req(hr_data())
    choices <- sort(unique(hr_data()$department))
    updateSelectInput(session, "dept_filter",
                      choices = choices,
                      selected = choices) # all by default
  })


  # Reactive to filter HR data based on department selection ----
  filtered_hr_data <- reactive({
    req(hr_data(), input$dept_filter)

    data <- hr_data()

    # Filter by department if specific departments are selected ----
    if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
      data <- data %>%
        filter(department %in% input$dept_filter)
    }

    # Do NOT filter by risk threshold for general metrics
    data
  })

  # Separate reactive for high-risk analysis only ----
  high_risk_data <- reactive({
    req(filtered_hr_data(), input$risk_threshold)

    filtered_hr_data() %>%
      filter(attrition_risk_score >= input$risk_threshold)
  })

  # Value boxes for overview tab - use filtered_hr_data
  output$total_employees <- renderValueBox({
    valueBox(
      value = nrow(filtered_hr_data()),
      subtitle = "Active Employees",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$attrition_rate <- renderValueBox({
    # Use risk threshold to calculate rate ----
    high_risk_count <- sum(filtered_hr_data()$attrition_risk_score >= input$risk_threshold)
    total_count <- nrow(filtered_hr_data())
    rate <- if(total_count > 0) round((high_risk_count / total_count) * 100, 1) else 0

    valueBox(
      value = paste0(rate, "%"),
      subtitle = "High Risk Rate",
      icon = icon("user-times"),
      color = "aqua"
    )
  })

  output$avg_satisfaction <- renderValueBox({
    avg_sat <- round(mean(filtered_hr_data()$job_satisfaction, na.rm = TRUE), 1)

    valueBox(
      value = avg_sat,
      subtitle = "Avg Job Satisfaction",
      icon = icon("smile"),
      color = "teal"
    )
  })

  output$project_success_rate <- renderValueBox({
    on_time_projects <- sum(project_data()$delivered_on_time == "Yes", na.rm = TRUE)
    total_projects <- nrow(project_data())
    rate <- if(total_projects > 0) round((on_time_projects / total_projects) * 100, 1) else 0

    valueBox(
      value = paste0(rate, "%"),
      subtitle = "On-Time Delivery",
      icon = icon("check-circle"),
      color = "light-blue"
    )
  })

  # Department distribution chart - uses filtered_hr_data ----
  output$dept_chart <- renderPlotly({
    dept_summary <- filtered_hr_data() %>%
      count(department) %>%
      arrange(desc(n))

    p <- ggplot(dept_summary, aes(x = reorder(department, n), y = n)) +
      geom_col(fill = "#4285f4", alpha = 0.8) +
      coord_flip() +
      labs(title = "Active Employees by Department",
           x = "Department",
           y = "Employee Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # Risk distribution chart ----
  output$risk_distribution <- renderPlotly({
    risk_data <- filtered_hr_data() %>%
      mutate(
        risk_category = case_when(
          attrition_risk_score >= 0.7 ~ "High Risk",
          attrition_risk_score >= 0.4 ~ "Medium Risk",
          TRUE ~ "Low Risk"
        )
      ) %>%
      count(risk_category) %>%
      mutate(risk_category = factor(risk_category, levels = c("High Risk", "Medium Risk", "Low Risk")))

    colors <- c("High Risk" = "#ffafcc", "Medium Risk" = "#cdb4db", "Low Risk" = "#90e0ef")

    p <- ggplot(risk_data, aes(x = risk_category, y = n, fill = risk_category)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = colors) +
      labs(title = "Employee Risk Distribution",
           x = "Risk Category",
           y = "Number of Employees") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.position = "none",
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # KPI table ----
  output$kpi_table <- renderDT({
    kpi_data <- data.frame(
      Metric = c("Total Active Employees", "Average Tenure (Years)", "High-Risk Employees",
                 "Average Performance Rating", "Total Annual Salary Cost", "Skills Coverage"),
      Value = c(
        nrow(filtered_hr_data()),
        round(mean(filtered_hr_data()$tenure_years, na.rm = TRUE), 1),
        sum(filtered_hr_data()$attrition_risk_score >= input$risk_threshold, na.rm = TRUE),
        round(mean(filtered_hr_data()$performance_rating, na.rm = TRUE), 1),
        paste("EUR", format(sum(filtered_hr_data()$salary, na.rm = TRUE), big.mark = ",")),
        paste(length(unique(filtered_hr_data()$skills)), "unique skills")
      )
    )

    datatable(kpi_data,
              options = list(
                dom = 't',
                pageLength = 10,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = 1)
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle(columns = c(1, 2),
                  backgroundColor = "#f4f8fc",
                  color = "#2c3e50")
  })

  # Executive summary ----
  output$executive_summary <- renderText({
    total_emp <- nrow(filtered_hr_data())
    high_risk <- sum(filtered_hr_data()$attrition_risk_score >= input$risk_threshold, na.rm = TRUE)
    risk_rate <- if(total_emp > 0) round((high_risk / total_emp) * 100, 1) else 0

    paste0("SUMMARY:\n",
           "• Total Active Workforce: ", total_emp, " employees\n",
           "• High-Risk Rate (≥", input$risk_threshold, "): ", risk_rate, "%\n",
           "• High-Risk Employees: ", high_risk, " requiring immediate attention\n",
           "• Departments shown: ", paste(input$dept_filter, collapse = ", "))
  })

  # Attrition Analysis Tab outputs - uses high_risk_data ----
  output$high_risk_count <- renderInfoBox({
    count <- nrow(high_risk_data())

    infoBox(
      title = "HIGH-RISK EMPLOYEES",
      value = count,
      subtitle = paste("Risk ≥", input$risk_threshold),
      icon = icon("exclamation-triangle"),
      color = "blue"
    )
  })

  output$estimated_cost <- renderInfoBox({
    cost <- sum(high_risk_data()$salary * 1.1, na.rm = TRUE) # % of salary as replacement cost

    infoBox(
      title = "ESTIMATED REPLACEMENT COST",
      value = paste("EUR", format(round(cost), big.mark = ",")),
      subtitle = "If all high-risk employees leave",
      icon = icon("euro-sign"),
      color = "blue"
    )
  })

  output$avg_tenure_leaving <- renderInfoBox({
    avg_tenure <- if(nrow(high_risk_data()) > 0) {
      round(mean(high_risk_data()$tenure_years, na.rm = TRUE), 1)
    } else {
      0
    }

    infoBox(
      title = "AVG TENURE OF HIGH-RISK",
      value = paste(avg_tenure, "years"),
      subtitle = "Average tenure of high-risk employees",
      icon = icon("clock"),
      color = "blue"
    )
  })

  # Risk chart - uses filtered_hr_data ----
  output$risk_factors_chart <- renderPlotly({
    risk_data <- filtered_hr_data() %>%
      mutate(
        satisfaction_level = case_when(
          job_satisfaction >= 4 ~ "High",
          job_satisfaction >= 3 ~ "Medium",
          TRUE ~ "Low"
        ),
        performance_level = case_when(
          performance_rating >= 4 ~ "High",
          performance_rating >= 3 ~ "Average",
          TRUE ~ "Below Avg"
        )
      ) %>%
      group_by(satisfaction_level, performance_level) %>%
      summarise(avg_risk = mean(attrition_risk_score, na.rm = TRUE), .groups = 'drop')

    p <- ggplot(risk_data, aes(x = satisfaction_level, y = performance_level, size = avg_risk, color = avg_risk)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(5, 20)) +
      scale_color_gradient(low = "#90e0ef", high = "#ffafcc", name = "Avg Risk") +
      labs(title = "Risk by Satisfaction vs Performance",
           x = "Job Satisfaction Level",
           y = "Performance Level") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.text = element_text(color = "#5a6c7d"),
        legend.title = element_text(color = "#2c3e50"),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # Department risk comparison chart - uses filtered_hr_data ----
  output$dept_risk_chart <- renderPlotly({
    dept_risk <- filtered_hr_data() %>%
      group_by(department) %>%
      summarise(
        avg_risk = mean(attrition_risk_score, na.rm = TRUE),
        risk_pct = round(avg_risk * 100, 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_risk))

    p <- ggplot(dept_risk, aes(x = reorder(department, avg_risk), y = risk_pct, fill = avg_risk)) +
      geom_col(alpha = 0.8) +
      scale_fill_gradient(low = "#90e0ef", high = "#ffafcc", name = "Risk Score") +
      coord_flip() +
      labs(title = "Average Attrition Risk by Department",
           x = "Department",
           y = "Average Risk Score (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.text = element_text(color = "#5a6c7d"),
        legend.title = element_text(color = "#2c3e50"),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # High-risk employees table - uses high_risk_data ----
  output$high_risk_table <- renderDT({
    high_risk_table_data <- high_risk_data() %>%
      select(employee_id, first_name, department, job_title, tenure_years,
             job_satisfaction, performance_rating, attrition_risk_score, recommended_action) %>%
      arrange(desc(attrition_risk_score))

    datatable(high_risk_table_data,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = c(4, 5, 6, 7))
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatRound(columns = c('tenure_years', 'job_satisfaction', 'performance_rating', 'attrition_risk_score'), digits = 2) %>%
      formatStyle('attrition_risk_score',
                  backgroundColor = styleInterval(c(0.5, 0.7), c('#90e0ef', '#fefae0', '#ffafcc')),
                  color = 'white')
  })

  # Resource Optimization Tab outputs - uses filtered_hr_data ----
  output$total_capacity <- renderValueBox({
    valueBox(
      value = nrow(filtered_hr_data()),
      subtitle = "Total FTE Capacity",
      icon = icon("users"),
      color = "navy"
    )
  })

  output$utilization_rate <- renderValueBox({
    # Simulate utilization rate ----
    util_rate <- round(runif(1, 75, 90), 1)

    valueBox(
      value = paste0(util_rate, "%"),
      subtitle = "Average Utilization",
      icon = icon("chart-line"),
      color = "navy"
    )
  })

  output$skills_coverage <- renderValueBox({
    unique_skills <- length(unique(filtered_hr_data()$skills))

    valueBox(
      value = unique_skills,
      subtitle = "Unique Skills",
      icon = icon("cogs"),
      color = "navy"
    )
  })

  output$cost_efficiency <- renderValueBox({
    avg_cost <- round(mean(filtered_hr_data()$salary, na.rm = TRUE))

    valueBox(
      value = paste("EUR", format(avg_cost, big.mark = ",")),
      subtitle = "Avg Cost per Employee",
      icon = icon("euro-sign"),
      color = "navy"
    )
  })

  # Resource utilization chart ----
  output$utilization_chart <- renderPlotly({
    # Simulate utilization data by department ----
    util_data <- filtered_hr_data() %>%
      group_by(department) %>%
      summarise(
        employees = n(),
        utilization = round(runif(1, 60, 85), 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(utilization))

    p <- ggplot(util_data, aes(x = reorder(department, utilization), y = utilization, fill = utilization)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = 80, linetype = "dashed", color = "navy", size = 1) +
      scale_fill_gradient(low = "#ffafcc", high = "#90e0ef", name = "Utilization") +
      coord_flip() +
      labs(title = "Resource Utilization by Department",
           x = "Department",
           y = "Utilization Rate (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.text = element_text(color = "#5a6c7d"),
        legend.title = element_text(color = "#2c3e50"),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # Skills table ----
  output$skills_table <- renderDT({
    skills_data <- filtered_hr_data() %>%
      count(skills, sort = TRUE) %>%
      mutate(percent_workforce = round((n / nrow(filtered_hr_data())) * 100, 1)) %>%
      rename(Skill = skills, Count = n, `% of Workforce` = percent_workforce)

    datatable(skills_data,
              options = list(
                pageLength = 15,
                dom = 't',
                columnDefs = list(
                  list(className = 'dt-center', targets = c(1, 2))
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe')
  })

  # Optimization recommendations table ----
  output$optimization_table <- renderDT({
    opt_data <- filtered_hr_data() %>%
      group_by(department) %>%
      summarise(
        employees = n(),
        avg_satisfaction = round(mean(job_satisfaction, na.rm = TRUE), 2),
        avg_performance = round(mean(performance_rating, na.rm = TRUE), 2),
        high_risk_count = sum(attrition_risk_score >= input$risk_threshold, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        priority = case_when(
          high_risk_count > 10 ~ "High",
          high_risk_count > 5 ~ "Medium",
          TRUE ~ "Low"
        ),
        recommendation = case_when(
          high_risk_count > 10 ~ "Implement retention program",
          avg_satisfaction < 3 ~ "Focus on satisfaction improvement",
          TRUE ~ "Monitor and maintain"
        ),
        estimated_impact = case_when(
          high_risk_count > 10 ~ "EUR 500K-1M annual savings",
          high_risk_count > 5 ~ "EUR 250K-500K annual savings",
          TRUE ~ "EUR 50K-100K annual savings"
        )
      )

    datatable(opt_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = c(1, 2, 3, 4))
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle('priority',
                  backgroundColor = styleEqual(c('High', 'Medium', 'Low'),
                                               c('#ffafcc', '#fefae0', '#90e0ef')),
                  color = 'white')
  })

  # Project Analytics Tab outputs ----
  output$total_projects <- renderInfoBox({
    infoBox(
      title = "TOTAL PROJECTS",
      value = nrow(project_data()),
      subtitle = "All projects in portfolio",
      icon = icon("project-diagram"),
      color = "blue"
    )
  })

  output$active_projects <- renderInfoBox({
    active_count <- sum(project_data()$status == "Active")

    infoBox(
      title = "ACTIVE PROJECTS",
      value = active_count,
      subtitle = "Currently in progress",
      icon = icon("play"),
      color = "aqua"
    )
  })

  output$on_time_delivery <- renderInfoBox({
    on_time_count <- sum(project_data()$delivered_on_time == "Yes", na.rm = TRUE)
    total_completed <- sum(project_data()$status == "Completed")
    rate <- if(total_completed > 0) round((on_time_count / total_completed) * 100, 1) else 0

    infoBox(
      title = "ON-TIME DELIVERY",
      value = paste0(rate, "%"),
      subtitle = "Completed projects delivered on time",
      icon = icon("clock"),
      color = "teal"
    )
  })

  output$avg_budget_variance <- renderInfoBox({
    # Simulate budget variance ----
    variance <- round(runif(1, 8, 15), 1)

    infoBox(
      title = "AVG BUDGET VARIANCE",
      value = paste0(variance, "%"),
      subtitle = "Average deviation from budget",
      icon = icon("euro"),
      color = "light-blue"
    )
  })

  # Project success chart ----
  output$project_success_chart <- renderPlotly({
    project_success <- project_data() %>%
      group_by(project_type) %>%
      summarise(
        total_projects = n(),
        on_time_count = sum(delivered_on_time == "Yes", na.rm = TRUE),
        on_time_rate = round((on_time_count / total_projects) * 100, 1),
        avg_satisfaction = round(mean(client_satisfaction, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(on_time_rate))

    p <- ggplot(project_success, aes(x = reorder(project_type, on_time_rate), y = on_time_rate, fill = avg_satisfaction)) +
      geom_col(alpha = 0.8) +
      scale_fill_gradient(low = "#ffafcc", high = "#90e0ef", name = "Avg Client\nSatisfaction") +
      coord_flip() +
      labs(title = "Project Success Rate by Type",
           x = "Project Type",
           y = "On-Time Delivery Rate (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.text = element_text(color = "#5a6c7d"),
        legend.title = element_text(color = "#2c3e50"),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # Budget vs timeline performance chart ----
  output$budget_timeline_chart <- renderPlotly({
    # Simulate project performance data ----
    set.seed(789)
    proj_perf <- project_data() %>%
      mutate(
        duration_days = as.numeric(planned_end_date - start_date),
        budget_variance = round(runif(nrow(project_data()), -20, 40), 1),
        project_size = case_when(
          budget > 1000000 ~ "Enterprise",
          budget > 500000 ~ "Large",
          budget > 100000 ~ "Medium",
          TRUE ~ "Small"
        )
      )

    p <- ggplot(proj_perf, aes(x = duration_days, y = budget_variance,
                               color = project_size, size = budget)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#2c3e50") +
      scale_color_manual(values = c("Enterprise" = "#ffafcc", "Large" = "#cdb4db",
                                    "Medium" = "#a2d2ff", "Small" = "#90e0ef"),
                         name = "Project Size") +
      scale_size_continuous(range = c(3, 12), name = "Budget ($)") +
      labs(title = "Budget Variance vs Project Duration",
           x = "Project Duration (Days)",
           y = "Budget Variance (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#2c3e50", size = 16, face = "bold"),
        axis.text = element_text(color = "#5a6c7d", size = 12),
        axis.title = element_text(color = "#2c3e50", face = "bold", size = 14),
        legend.text = element_text(color = "#5a6c7d"),
        legend.title = element_text(color = "#2c3e50"),
        panel.grid = element_line(color = "#e8f0fe"),
        plot.background = element_rect(fill = "#f4f8fc", color = NA),
        panel.background = element_rect(fill = "#f4f8fc", color = NA)
      )

    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        font = list(family = "Nunito", color = "#2c3e50"),
        paper_bgcolor = "#f4f8fc",
        plot_bgcolor = "#f4f8fc"
      )
  })

  # Project portfolio table ----
  output$projects_table <- renderDT({
    proj_table_data <- project_data() %>%
      select(project_id, project_name, project_type, status, budget,
             start_date, planned_end_date, delivered_on_time, client_satisfaction) %>%
      arrange(desc(start_date))

    datatable(proj_table_data,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = c(3, 7, 8))
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatCurrency('budget', currency = "EUR", interval = 3, mark = ",") %>%
      formatRound('client_satisfaction', digits = 1) %>%
      formatStyle('status',
                  backgroundColor = styleEqual(c('Completed', 'Active', 'Planning'),
                                               c('navy', '#f49cbb', '#0096c7')),
                  color = 'white') %>%
      formatStyle('delivered_on_time',
                  backgroundColor = styleEqual(c('Yes', 'No'),
                                               c('#90e0ef', '#ffafcc')),
                  color = 'white')
  })
}
