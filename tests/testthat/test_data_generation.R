# Test file for data generation functions

test_that("GlobalTechDataGenerator creates employee data", {

  # Create generator
  generator <- GlobalTechDataGenerator$new()

  # Generate small dataset for testing
  employees <- generator$generate_employee_data(50)

  # Check basic structure
  expect_true(is.data.frame(employees))
  expect_equal(nrow(employees), 50)

  # Check required columns exist
  required_cols <- c("employee_id", "department", "age", "tenure_years",
                     "job_satisfaction", "performance_rating", "attrition_risk_score")
  expect_true(all(required_cols %in% names(employees)))

  # Check data quality
  expect_true(all(employees$age >= 22 & employees$age <= 65))
  expect_true(all(employees$tenure_years >= 0))
  expect_true(all(employees$job_satisfaction >= 1 & employees$job_satisfaction <= 5))
  expect_true(all(employees$performance_rating >= 1 & employees$performance_rating <= 5))
  expect_true(all(employees$attrition_risk_score >= 0 & employees$attrition_risk_score <= 1))

  # Check for unique employee IDs
  expect_equal(length(unique(employees$employee_id)), nrow(employees))
})

test_that("GlobalTechDataGenerator creates project data", {

  # Create generator
  generator <- GlobalTechDataGenerator$new()

  # Generate datasets
  employees <- generator$generate_employee_data(20)
  projects <- generator$generate_project_data(10, employees)

  # Check basic structure
  expect_true(is.data.frame(projects))
  expect_equal(nrow(projects), 10)

  # Check required columns
  required_cols <- c("project_id", "project_type", "budget", "start_date",
                     "planned_end_date", "status")
  expect_true(all(required_cols %in% names(projects)))

  # Check data quality
  expect_true(all(projects$budget > 0))
  expect_true(all(projects$start_date <= projects$planned_end_date))
  expect_true(all(projects$status %in% c("Active", "Completed", "On Hold", "Cancelled")))

  # Check for unique project IDs
  expect_equal(length(unique(projects$project_id)), nrow(projects))
})

test_that("generate_globaltech_data function works correctly", {

  # Test main generation function
  data <- generate_globaltech_data(n_employees = 25, n_projects = 5)

  # Check structure
  expect_true(is.list(data))
  expect_true("employees" %in% names(data))
  expect_true("projects" %in% names(data))

  # Check data sizes
  expect_equal(nrow(data$employees), 25)
  expect_equal(nrow(data$projects), 5)

  # Check data validity using validation function
  expect_true(validate_dataframe(
    data$employees,
    c("employee_id", "department", "job_satisfaction"),
    min_rows = 25
  ))

  expect_true(validate_dataframe(
    data$projects,
    c("project_id", "project_type", "budget"),
    min_rows = 5
  ))
})
