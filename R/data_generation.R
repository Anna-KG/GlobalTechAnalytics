#' @title Data Generation for GlobalTech Analytics
#' @description Generate employee and project data for testing and demos
#' @name data_generation
#' @importFrom R6 R6Class
#' @importFrom dplyr %>% mutate select filter group_by summarise left_join n
#' @importFrom lubridate today days years
#' @importFrom purrr map_chr map_dbl pmap_dbl
#' @importFrom tibble tibble
#' @importFrom readr write_csv

#'
#' @description R6 class for generating synthetic employee and project data
#' @export
#' @examples
#' \dontrun{
#' # Initialize the data generator
#' generator <- GlobalTechDataGenerator$new()
#' employees <- generator$generate_employee_data(100)
#' projects <- generator$generate_project_data(10, employees)
#' }
GlobalTechDataGenerator <- R6::R6Class(
  "GlobalTechDataGenerator",

  public = list(

    #' @description Initialize the data generator
    #' @param seed Random seed for reproducibility
    initialize = function(seed = 42) {
      set.seed(seed)
      private$setup_company_structure()
      private$setup_skill_matrices()
    },

    #' Generate comprehensive employee dataset
    #' @param n_employees Number of employees to generate
    #' @return tibble with employee data
    generate_employee_data = function(n_employees = 15000) {

      # Initialize data containers
      employees <- tibble::tibble(
        employee_id = sprintf("EMP%06d", 1:n_employees),
        first_name = paste("Employee", 1:n_employees),
        last_name = paste("Surname", 1:n_employees)
      )

      # Department assignment based on company distribution
      employees$department <- sample(
        names(private$dept_distribution),
        n_employees,
        replace = TRUE,
        prob = private$dept_distribution
      )

      # Generate age with realistic distribution
      employees$age <- pmax(22, pmin(65,
                                     round(rnorm(n_employees, mean = 35, sd = 8))
      ))

      # Tenure influenced by age and department
      employees$tenure_years <- purrr::pmap_dbl(
        list(employees$age, employees$department),
        private$calculate_tenure
      )

      # Location assignment
      employees$location <- sample(
        private$locations,
        n_employees,
        replace = TRUE
      )

      # Salary calculation
      employees$base_salary <- purrr::pmap_dbl(
        list(employees$department, employees$tenure_years,
             employees$location, employees$age),
        private$calculate_salary
      )

      # Performance and satisfaction metrics
      employees$performance_rating <- pmax(1, pmin(5,rbeta(n_employees, 8, 2) * 4 + 1
      ))

      employees$job_satisfaction <- pmax(1, pmin(5,rnorm(n_employees, mean = 3.2, sd = 0.8)
      ))

      # Skills assignment based on department
      employees$skills <- purrr::map_chr(
        employees$department,
        private$assign_skills
      )

      # Work arrangement (post-pandemic reality)
      employees$work_arrangement <- sample(
        c("Remote", "Hybrid", "Office"),
        n_employees,
        replace = TRUE,
        prob = c(0.25, 0.35, 0.40)
      )

      # Career progression metrics
      employees$last_promotion_months_ago <- rexp(n_employees, rate = 1/18)
      employees$training_hours_last_year <- rgamma(n_employees, shape = 2, scale = 15)

      # Manager assignment (hierarchical structure)
      employees$manager_id <- private$assign_managers(employees)

      # Job titles based on department and seniority
      employees$job_title <- purrr::pmap_chr(
        list(employees$department, employees$tenure_years, employees$performance_rating),
        private$generate_job_title
      )

      # Calculate attrition risk
      employees$attrition_risk_score <- purrr::pmap_dbl(
        list(employees$department, employees$tenure_years, employees$job_satisfaction,
             employees$performance_rating, employees$last_promotion_months_ago,
             employees$work_arrangement),
        private$calculate_attrition_risk
      )

      # Historical attrition (18.5% annual rate)
      employees$left_company <- rbinom(n_employees, 1, 0.185) == 1

      # Termination details for those who left
      termination_data <- private$generate_termination_data(employees)
      employees <- employees %>%
        dplyr::left_join(termination_data, by = "employee_id")

      # Important dates
      employees$hire_date <- lubridate::today() - lubridate::days(round(employees$tenure_years * 365))
      employees$last_review_date <- lubridate::today() - lubridate::days(sample(30:365, n_employees, replace = TRUE))

      # Round numeric columns for readability
      employees <- employees %>%
        dplyr::mutate(
          tenure_years = round(tenure_years, 1),
          base_salary = round(base_salary, 0),
          performance_rating = round(performance_rating, 2),
          job_satisfaction = round(job_satisfaction, 2),
          last_promotion_months_ago = round(last_promotion_months_ago, 1),
          training_hours_last_year = round(training_hours_last_year, 1),
          attrition_risk_score = round(attrition_risk_score, 3)
        )
      return(employees)
    },

    #' Generate realistic project dataset
    #' @param n_projects Number of projects to generate
    #' @param employee_df Employee dataframe for team assignments (optional)
    #' @return tibble with project data
    generate_project_data = function(n_projects = 500, employee_df = NULL) {

      projects <- tibble::tibble(
        project_id = sprintf("PROJ%04d", 1:n_projects),
        project_type = sample(
          private$project_types,
          n_projects,
          replace = TRUE
        )
      )

      # Project names
      project_adjectives <- c("Alpha", "Beta", "Gamma", "Delta", "Enterprise",
                              "Pro", "Core", "Advanced", "Next", "Smart")
      projects$project_name <- paste(
        projects$project_type, "-",
        sample(project_adjectives, n_projects, replace = TRUE)
      )

      # Client type
      projects$client_type <- sample(
        c("Internal", "External"),
        n_projects,
        replace = TRUE,
        prob = c(0.4, 0.6)
      )

      # Timeline generation
      projects$start_date <- lubridate::today() - lubridate::days(sample(1:730, n_projects, replace = TRUE))

      # Duration based on project type complexity
      duration_days <- purrr::map_dbl(projects$project_type, private$get_project_duration)
      projects$planned_end_date <- projects$start_date + lubridate::days(duration_days)

      # Budget calculation
      projects$budget <- purrr::map_dbl(projects$project_type, private$calculate_project_budget)

      # Required skills
      projects$required_skills <- purrr::map_chr(projects$project_type, private$get_project_skills)

      # Team size based on budget and complexity
      projects$team_size <- pmax(3, round(projects$budget / 150000 * runif(n_projects, 0.8, 1.2)))

      # Project status based on timeline
      projects$status <- private$determine_project_status(projects)

      # Success metrics for completed projects
      success_metrics <- private$calculate_success_metrics(projects)
      projects <- projects %>%
        dplyr::left_join(success_metrics, by = "project_id")

      # Complexity and priority
      projects$complexity_score <- round(runif(n_projects, 1, 10), 2)
      projects$priority <- sample(1:10, n_projects, replace = TRUE)

      # Round budget
      projects$budget <- round(projects$budget, 0)

      return(projects)
    },

    #' Save all generated data to CSV files
    #' @param employee_df Employee data dataframe
    #' @param project_df Project data dataframe
    #' @param output_dir Output directory path
    save_data = function(employee_df, project_df, output_dir = "data") {

      # Create directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      # Save datasets
      readr::write_csv(employee_df, file.path(output_dir, "employee_data.csv"))
      readr::write_csv(project_df, file.path(output_dir, "project_data.csv"))

      # Print summary statistics
      cat("Data Generation Summary")
      cat("Employees:", nrow(employee_df), "\n")
      cat("Projects:", nrow(project_df), "\n")
      cat("Attrition Rate:", round(mean(employee_df$left_company) * 100, 1), "%\n")
      cat("Avg Employee Age:", round(mean(employee_df$age), 1), "years\n")
      cat("Avg Tenure:", round(mean(employee_df$tenure_years), 1), "years\n")
    }
  ),

  private = list(

    # Company structure
    dept_distribution = NULL,
    locations = NULL,
    project_types = NULL,
    skills_by_dept = NULL,

    #' Setup company organizational structure
    setup_company_structure = function() {
      private$dept_distribution <- c(
        "Engineering" = 0.40,
        "Data Science" = 0.15,
        "Product" = 0.20,
        "Design" = 0.10,
        "QA" = 0.10,
        "Others" = 0.05
      )

      private$locations <- c(
        "Paris", "Lisbon", "Brussels", "Perth",
        "London", "Berlin", "Amsterdam", "Dublin",
        "Bangalore", "Singapore", "Sydney", "Toronto"
      )

      private$project_types <- c(
        "Cloud Migration", "AI/ML Platform", "Mobile App", "Web Application",
        "Data Pipeline", "Infrastructure", "Analytics Dashboard", "API Development",
        "Security Enhancement", "Legacy Modernization"
      )
    },

    #' Setup skill matrices by department
    setup_skill_matrices = function() {
      private$skills_by_dept <- list(
        "Engineering" = c("Python", "Java", "JavaScript", "React", "Node.js", "AWS", "Docker", "Kubernetes"),
        "Data Science" = c("Python", "R", "SQL", "Machine Learning", "TensorFlow", "PyTorch", "Statistics"),
        "Product" = c("Product Strategy", "Agile", "Roadmapping", "Analytics", "User Research", "SQL"),
        "Design" = c("UI/UX Design", "Figma", "Adobe Creative", "Prototyping", "User Research"),
        "QA" = c("Test Automation", "Selenium", "API Testing", "Performance Testing", "Security Testing"),
        "Others" = c("Project Management", "Scrum Master", "Business Analysis", "Sales", "Marketing")
      )
    },

    calculate_tenure = function(age, department) {
      max_tenure <- age - 22
      if (department == "Data Science") {
        tenure <- rexp(1, rate = 1/2.5)
      } else {
        tenure <- rexp(1, rate = 1/4.0)
      }
      return(max(0.5, min(tenure, max_tenure)))
    },

    calculate_salary = function(dept, tenure, location, age) {
      base_salaries <- c(
        "Engineering" = 95000, "Data Science" = 105000, "Product" = 90000,
        "Design" = 80000, "QA" = 75000, "Others" = 70000
      )

      location_multipliers <- c(
        "Lisbon" = 1.4, "Paris" = 1.3, "Perth" = 1.2,
        "Brussels" = 1.1, "London" = 1.15, "Dublin" = 1.1,
        "Berlin" = 1.05, "Amsterdam" = 1.1, "Toronto" = 1.05,
        "Bangalore" = 0.4, "Singapore" = 0.9, "Sydney" = 1.1
      )

      base <- base_salaries[dept]
      location_mult <- ifelse(location %in% names(location_multipliers),
                              location_multipliers[location], 1.0)
      tenure_mult <- 1 + (tenure * 0.05)
      age_mult <- 1 + ((age - 25) * 0.02)

      return(base * location_mult * tenure_mult * age_mult * runif(1, 0.9, 1.1))
    },

    assign_skills = function(department) {
      dept_skills <- private$skills_by_dept[[department]]
      n_skills <- sample(2:min(5, length(dept_skills)), 1)
      selected_skills <- sample(dept_skills, n_skills)
      return(paste(selected_skills, collapse = ", "))
    },

    assign_managers = function(employees) {
      n_employees <- nrow(employees)
      manager_ids <- character(n_employees)
      manager_ids[1:min(50, n_employees)] <- NA
      if (n_employees > 50) {
        manager_pool <- employees$employee_id[1:min(1000, n_employees)]
        manager_ids[51:n_employees] <- sample(manager_pool, n_employees - 50, replace = TRUE)
      }
      return(manager_ids)
    },

    generate_job_title = function(dept, tenure, performance) {
      titles <- list(
        "Engineering" = c("Software Engineer", "Senior Software Engineer", "Staff Engineer", "Principal Engineer"),
        "Data Science" = c("Data Scientist", "Senior Data Scientist", "Staff Data Scientist", "Principal Data Scientist"),
        "Product" = c("Product Manager", "Senior Product Manager", "Principal Product Manager", "Director of Product"),
        "Design" = c("UX Designer", "Senior UX Designer", "Staff Designer", "Design Director"),
        "QA" = c("QA Engineer", "Senior QA Engineer", "QA Lead", "QA Manager"),
        "Others" = c("Specialist", "Senior Specialist", "Manager", "Senior Manager")
      )

      dept_titles <- titles[[dept]]
      if (tenure < 2) level <- 1
      else if (tenure < 5 && performance > 3.5) level <- 2
      else if (tenure < 8 && performance > 3.0) level <- 3
      else level <- 4

      level <- min(level, length(dept_titles))
      return(dept_titles[level])
    },

    calculate_attrition_risk = function(dept, tenure, satisfaction, performance,
                                        last_promotion, work_arrangement) {
      risk_score <- 0.0
      dept_risk <- c("Data Science" = 0.3, "Engineering" = 0.2, "Product" = 0.15,
                     "Design" = 0.1, "QA" = 0.1, "Others" = 0.05)
      risk_score <- risk_score + dept_risk[dept]

      if (tenure < 1) risk_score <- risk_score + 0.25
      else if (tenure > 8) risk_score <- risk_score + 0.15

      risk_score <- risk_score + max(0, (3.5 - satisfaction) * 0.2)
      if (performance > 4.0) risk_score <- risk_score + 0.1
      if (last_promotion > 24) risk_score <- risk_score + 0.15
      if (work_arrangement == "Office") risk_score <- risk_score + 0.05

      return(min(1.0, risk_score))
    },

    generate_termination_data = function(employees) {
      termination_reasons <- c(
        "Voluntary - Better Opportunity", "Voluntary - Career Growth",
        "Voluntary - Compensation", "Voluntary - Work-Life Balance",
        "Involuntary - Performance", "Involuntary - Restructuring"
      )

      employees %>%
        dplyr::filter(left_company) %>%
        dplyr::mutate(
          termination_date = lubridate::today() - lubridate::days(sample(1:365, dplyr::n(), replace = TRUE)),
          termination_reason = sample(termination_reasons, dplyr::n(), replace = TRUE,
                                      prob = c(0.35, 0.25, 0.15, 0.15, 0.07, 0.03))
        ) %>%
        dplyr::select(employee_id, termination_date, termination_reason)
    },

    get_project_duration = function(project_type) {
      duration_ranges <- c(
        "Cloud Migration" = 120, "AI/ML Platform" = 180, "Mobile App" = 90,
        "Web Application" = 100, "Data Pipeline" = 70, "Infrastructure" = 110,
        "Analytics Dashboard" = 60, "API Development" = 80,
        "Security Enhancement" = 95, "Legacy Modernization" = 150
      )
      base_duration <- duration_ranges[project_type]
      return(round(rgamma(1, shape = 2, scale = base_duration/2)))
    },

    calculate_project_budget = function(project_type) {
      base_budgets <- c(
        "Cloud Migration" = 850000, "AI/ML Platform" = 1200000, "Mobile App" = 450000,
        "Web Application" = 600000, "Data Pipeline" = 400000, "Infrastructure" = 750000,
        "Analytics Dashboard" = 300000, "API Development" = 350000,
        "Security Enhancement" = 500000, "Legacy Modernization" = 900000
      )
      base_budget <- base_budgets[project_type]
      return(base_budget * runif(1, 0.7, 1.8))
    },

    get_project_skills = function(project_type) {
      skill_mapping <- list(
        "Cloud Migration" = c("AWS", "Docker", "Kubernetes", "Python"),
        "AI/ML Platform" = c("Python", "Machine Learning", "TensorFlow", "Statistics"),
        "Mobile App" = c("React", "JavaScript", "UI/UX Design"),
        "Web Application" = c("JavaScript", "React", "Node.js", "Python"),
        "Data Pipeline" = c("Python", "SQL", "AWS"),
        "Infrastructure" = c("AWS", "Docker", "Kubernetes"),
        "Analytics Dashboard" = c("Python", "SQL", "Analytics"),
        "API Development" = c("Python", "Node.js", "API Testing"),
        "Security Enhancement" = c("Security Testing", "Python"),
        "Legacy Modernization" = c("Java", "Python", "AWS")
      )
      skills <- skill_mapping[[project_type]]
      return(paste(skills, collapse = ", "))
    },

    determine_project_status = function(projects) {
      status <- character(nrow(projects))
      for (i in 1:nrow(projects)) {
        if (projects$planned_end_date[i] < lubridate::today()) {
          status[i] <- sample(c("Completed", "Cancelled"), 1, prob = c(0.85, 0.15))
        } else {
          status[i] <- sample(c("Active", "On Hold"), 1, prob = c(0.9, 0.1))
        }
      }
      return(status)
    },

    calculate_success_metrics = function(projects) {
      completed_projects <- projects %>%
        dplyr::filter(status == "Completed")

      if (nrow(completed_projects) == 0) {
        return(tibble::tibble(project_id = character(0)))
      }

      completed_projects %>%
        dplyr::mutate(
          delivered_on_time = rbinom(dplyr::n(), 1, 0.62) == 1,
          budget_variance_pct = ifelse(delivered_on_time,
                                       rnorm(dplyr::n(), 0.02, 0.08),
                                       rnorm(dplyr::n(), 0.1, 0.15)),
          client_satisfaction = pmax(1, pmin(5, rnorm(dplyr::n(), 3.5, 1.0)))
        ) %>%
        dplyr::select(project_id, delivered_on_time, budget_variance_pct, client_satisfaction)
    }
  )
)

#' Generate GlobalTech sample data
#'
#' @description Main function to generate all synthetic data
#' @param n_employees Number of employees to generate
#' @param n_projects Number of projects to generate
#' @param output_dir Output directory for saved files (optional)
#' @return List containing generated datasets
#' @export
#' @examples
#' \dontrun{
#' data <- generate_globaltech_data(1000, 50)
#' }
generate_globaltech_data <- function(n_employees = 15000, n_projects = 500,
                                     output_dir = NULL) {

  # Initialize generator
  generator <- GlobalTechDataGenerator$new()

  # Generate core datasets
  employees <- generator$generate_employee_data(n_employees)
  projects <- generator$generate_project_data(n_projects, employees)

  # Save data if output directory specified
  if (!is.null(output_dir)) {
    generator$save_data(employees, projects, output_dir)
  }

  return(list(
    employees = employees,
    projects = projects
  ))
}

