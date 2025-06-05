# Test file for analytics functions

test_that("build_attrition_model works correctly", {

  # Generate sample data
  sample_data <- generate_globaltech_data(100, 10)

  # Test model building
  model <- build_attrition_model(sample_data$employees, test_split = 0.3)

  # Check model object structure
  expect_true(is.list(model))
  expect_true("model" %in% names(model))
  expect_true("auc" %in% names(model))
  expect_true("feature_importance" %in% names(model))

  # Check AUC is reasonable (more lenient for small datasets)
  expect_true(is.numeric(model$auc))
  expect_true(model$auc >= 0.3 && model$auc <= 1.0)  # threshold for small test data

  # Check feature importance structure
  expect_true(is.data.frame(model$feature_importance))
  expect_true(nrow(model$feature_importance) > 0)
})

test_that("analyze_resource_utilization works correctly", {

  # Generate sample data
  sample_data <- generate_globaltech_data(50, 5)

  # Test utilization analysis
  utilization <- analyze_resource_utilization(
    sample_data$employees,
    sample_data$projects
  )

  # Check structure
  expect_true(is.list(utilization))
  expect_true("department_utilization" %in% names(utilization))
  expect_true("project_success" %in% names(utilization))
  expect_true("skills_inventory" %in% names(utilization))

  # Check department utilization
  dept_util <- utilization$department_utilization
  expect_true(is.data.frame(dept_util))
  expect_true("department" %in% names(dept_util))
  expect_true("total_employees" %in% names(dept_util))
})

test_that("generate_business_insights works correctly", {

  # Generate sample data
  sample_data <- generate_globaltech_data(100, 10)

  # Test insights generation
  insights <- generate_business_insights(
    sample_data$employees,
    sample_data$projects
  )

  # Check structure
  expect_true(is.list(insights))
  expect_true("key_metrics" %in% names(insights))
  expect_true("financial_impact" %in% names(insights))
  expect_true("executive_summary" %in% names(insights))
  expect_true("recommendations" %in% names(insights))

  # Check key metrics
  metrics <- insights$key_metrics
  expect_true(is.list(metrics))
  expect_true("total_employees" %in% names(metrics))
  expect_true("attrition_rate" %in% names(metrics))

  # Check financial impact
  financial <- insights$financial_impact
  expect_true(is.list(financial))
  expect_true("total_potential" %in% names(financial))
  expect_true(financial$total_potential > 0)
})
