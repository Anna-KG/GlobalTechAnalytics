#' @title Analytics and Predictive Models
#' @description Core analytics functions for workforce insights
#' @aliases analytics
#' @docType package
#' @importFrom stats predict glm binomial ecdf
NULL

globalVariables(c(
  "age", "attrition_risk_score", "avg_performance", "avg_risk",
  "avg_satisfaction", "base_salary", "budget_variance_pct",
  "client_satisfaction", "delivered_on_time", "last_promotion_months_ago",
  "skill_count", "skills", "status", "total_cost", "total_count",
  "total_employees", "training_hours_last_year", "work_arrangement",
  "left_company", "department", "tenure_years", "job_satisfaction",
  "performance_rating", "employee_id"
))

#' Build Attrition Prediction Model
#'
#' @description Train a machine learning model to predict employee attrition
#' @param model_type Type of model ("rf" for Random Forest, "glm" for logistic regression)
#' @param employee_data DataFrame containing employee data
#' @param test_split Proportion of data to use for testing (default: 0.2)
#' @return List containing trained model and evaluation metrics
#' @importFrom randomForest randomForest importance
#' @importFrom pROC roc auc
#' @importFrom caret createDataPartition
#' @importFrom dplyr %>% select mutate filter group_by summarise arrange desc case_when
#' @importFrom tidyr drop_na separate_rows
#' @importFrom scales comma
#' @export
#' @examples
#' \dontrun{
#' # Example usage
#' model_result <- build_attrition_model(employee_data, model_type = "rf")
#' }
build_attrition_model <- function(employee_data, model_type = "rf", test_split = 0.2) {

  # Validate input data
  required_cols <- c("left_company", "department", "tenure_years", "job_satisfaction",
                     "performance_rating", "age", "base_salary")
  validate_dataframe(employee_data, required_cols)

  # Prepare modeling dataset
  model_data <- employee_data %>%
    dplyr::select(
      left_company, department, tenure_years, job_satisfaction,
      performance_rating, age, base_salary, work_arrangement,
      last_promotion_months_ago, training_hours_last_year
    ) %>%
    dplyr::mutate(
      department = as.factor(department),
      work_arrangement = as.factor(work_arrangement),
      left_company = as.factor(ifelse(left_company, "Yes", "No")),
      # Create derived features
      tenure_category = cut(tenure_years,
                            breaks = c(0, 1, 3, 5, 10, Inf),
                            labels = c("New", "Junior", "Mid", "Senior", "Expert")),
      satisfaction_level = cut(job_satisfaction,
                               breaks = c(0, 2.5, 3.5, 5),
                               labels = c("Low", "Medium", "High")),
      performance_level = cut(performance_rating,
                              breaks = c(0, 3, 4, 5),
                              labels = c("Below", "Average", "High"))
    ) %>%
    tidyr::drop_na()

  # Split data
  set.seed(42)
  train_idx <- caret::createDataPartition(model_data$left_company, p = 1 - test_split, list = FALSE)
  train_data <- model_data[train_idx, ]
  test_data <- model_data[-train_idx, ]

  # Train model
  if (model_type == "rf") {

    # Random Forest
    model <- randomForest::randomForest(
      left_company ~ .,
      data = train_data,
      ntree = 500,
      mtry = sqrt(ncol(train_data) - 1),
      importance = TRUE
    )

    # Predictions
    train_pred <- predict(model, train_data, type = "prob")[, "Yes"]
    test_pred <- predict(model, test_data, type = "prob")[, "Yes"]

  } else if (model_type == "glm") {

    # Logistic Regression
    model <- glm(
      left_company ~ .,
      data = train_data,
      family = binomial()
    )

    # Predictions
    train_pred <- predict(model, train_data, type = "response")
    test_pred <- predict(model, test_data, type = "response")

  } else {
    stop("model_type must be 'rf' or 'glm'")
  }

  # Evaluate model
  test_actual <- ifelse(test_data$left_company == "Yes", 1, 0)

  # ROC and AUC
  roc_result <- pROC::roc(test_actual, test_pred, quiet = TRUE)
  auc_score <- pROC::auc(roc_result)

  # Feature importance
  if (model_type == "rf") {
    importance_scores <- randomForest::importance(model)[, "MeanDecreaseGini"]
    feature_importance <- data.frame(
      feature = names(importance_scores),
      importance = as.numeric(importance_scores)
    ) %>%
      dplyr::arrange(dplyr::desc(importance))
  } else {
    coefs <- summary(model)$coefficients
    feature_importance <- data.frame(
      feature = rownames(coefs)[-1],  # Exclude intercept
      importance = abs(coefs[-1, "z value"])
    ) %>%
      dplyr::arrange(dplyr::desc(importance))
  }

  return(list(
    model = model,
    auc = auc_score,
    roc = roc_result,
    feature_importance = feature_importance,
    predictions = list(
      train = train_pred,
      test = test_pred,
      test_actual = test_actual
    ),
    data_split = list(
      train = train_data,
      test = test_data
    )
  ))
}

#' Predict Employee Attrition Risk
#'
#' @description Predict attrition risk for specific employees
#' @param model Trained attrition model (from build_attrition_model)
#' @param employee_data DataFrame containing employee data
#' @export
predict_attrition_risk <- function(model, employee_data) {

  # Prepare data in same format as training
  pred_data <- employee_data %>%
    dplyr::select(
      employee_id, department, tenure_years, job_satisfaction,
      performance_rating, age, base_salary, work_arrangement,
      last_promotion_months_ago, training_hours_last_year
    ) %>%
    dplyr::mutate(
      department = as.factor(department),
      work_arrangement = as.factor(work_arrangement),
      tenure_category = cut(tenure_years,
                            breaks = c(0, 1, 3, 5, 10, Inf),
                            labels = c("New", "Junior", "Mid", "Senior", "Expert")),
      satisfaction_level = cut(job_satisfaction,
                               breaks = c(0, 2.5, 3.5, 5),
                               labels = c("Low", "Medium", "High")),
      performance_level = cut(performance_rating,
                              breaks = c(0, 3, 4, 5),
                              labels = c("Below", "Average", "High"))
    ) %>%
    tidyr::drop_na()

  # Make predictions
  if (inherits(model$model, "randomForest")) {
    risk_scores <- predict(model$model, pred_data, type = "prob")[, "Yes"]
  } else {
    risk_scores <- predict(model$model, pred_data, type = "response")
  }

  # Return results
  results <- pred_data %>%
    dplyr::select(employee_id) %>%
    dplyr::mutate(
      attrition_risk = round(risk_scores, 3),
      risk_level = dplyr::case_when(
        attrition_risk >= 0.7 ~ "High",
        attrition_risk >= 0.4 ~ "Medium",
        TRUE ~ "Low"
      ),
      recommended_action = dplyr::case_when(
        attrition_risk >= 0.7 ~ "Immediate action required",
        attrition_risk >= 0.4 ~ "Monitor",
        TRUE ~ "Standard management"
      )
    )

  return(results)
}

#' Analyze Resource Utilization
#'
#' @description Analyze current resource allocation and utilization
#' @param employee_data DataFrame containing employee data
#' @param project_data DataFrame containing project data
#' @return List containing utilization analysis
#' @export
analyze_resource_utilization <- function(employee_data, project_data) {

  # Department-level analysis
  dept_utilization <- employee_data %>%
    dplyr::filter(!left_company) %>%
    dplyr::group_by(department) %>%
    dplyr::summarise(
      total_employees = dplyr::n(),
      avg_performance = mean(performance_rating, na.rm = TRUE),
      avg_satisfaction = mean(job_satisfaction, na.rm = TRUE),
      avg_tenure = mean(tenure_years, na.rm = TRUE),
      total_cost = sum(base_salary, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cost_per_employee = total_cost / total_employees,
      efficiency_score = (avg_performance * 0.6) + (avg_satisfaction * 0.4)
    )

  # Project success analysis
  project_success <- project_data %>%
    dplyr::filter(status == "Completed") %>%
    dplyr::summarise(
      total_projects = dplyr::n(),
      success_rate = mean(delivered_on_time, na.rm = TRUE),
      avg_budget_variance = mean(abs(budget_variance_pct), na.rm = TRUE),
      avg_client_satisfaction = mean(client_satisfaction, na.rm = TRUE)
    )

  # Skills gap analysis
  all_skills <- employee_data %>%
    dplyr::filter(!left_company) %>%
    dplyr::select(department, skills) %>%
    tidyr::separate_rows(skills, sep = ", ") %>%
    dplyr::count(department, skills, name = "skill_count") %>%
    dplyr::group_by(skills) %>%
    dplyr::summarise(
      total_count = sum(skill_count),
      departments = dplyr::n_distinct(department),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_count))

  return(list(
    department_utilization = dept_utilization,
    project_success = project_success,
    skills_inventory = all_skills
  ))
}

#' Generate Business Insights
#'
#' @description Generate executive-level business insights from analytics
#' @param attrition_model Optional attrition model for risk analysis
#' @param employee_data DataFrame containing employee data
#' @param project_data DataFrame containing project data
#' @return List containing key business insights
#' @export
generate_business_insights <- function(employee_data, project_data, attrition_model = NULL) {

  # Key metrics
  total_employees <- sum(!employee_data$left_company)
  attrition_rate <- mean(employee_data$left_company) * 100
  avg_satisfaction <- mean(employee_data$job_satisfaction[!employee_data$left_company], na.rm = TRUE)

  # High-risk employees
  high_risk_count <- sum(employee_data$attrition_risk_score > 0.7 & !employee_data$left_company, na.rm = TRUE)
  estimated_replacement_cost <- high_risk_count * 75000  # avg replacement cost

  # Project performance
  completed_projects <- project_data %>% dplyr::filter(status == "Completed")
  on_time_rate <- mean(completed_projects$delivered_on_time, na.rm = TRUE) * 100

  # Department insights
  dept_risks <- employee_data %>%
    dplyr::filter(!left_company) %>%
    dplyr::group_by(department) %>%
    dplyr::summarise(
      employees = dplyr::n(),
      avg_risk = mean(attrition_risk_score, na.rm = TRUE),
      high_risk_count = sum(attrition_risk_score > 0.7, na.rm = TRUE),
      avg_satisfaction = mean(job_satisfaction, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(avg_risk))

  # Financial impact estimates
  annual_salary_cost <- sum(employee_data$base_salary[!employee_data$left_company], na.rm = TRUE)

  potential_savings <- list(
    attrition_reduction = estimated_replacement_cost * 0.5, # 50% reduction assumption
    efficiency_improvement = annual_salary_cost * 0.05, # 5% efficiency gain
    total_potential = (estimated_replacement_cost * 0.5) + (annual_salary_cost * 0.05)
  )

  # Executive summary ----
  executive_summary <- paste(
    sprintf("Analysis of %s employees reveals %d high-risk individuals",
            scales::comma(total_employees), high_risk_count),
    sprintf("representing $%s in potential replacement costs.",
            scales::comma(estimated_replacement_cost)),
    sprintf("Current attrition rate of %.1f%% exceeds industry benchmark.", attrition_rate),
    sprintf("Project delivery success rate of %.1f%% indicates optimization opportunity.", on_time_rate),
    sprintf("Targeted interventions could save $%s annually.",
            scales::comma(potential_savings$total_potential)),
    sep = " "
  )

  # Recommendations
  recommendations <- list(
    immediate = c(
      "Implement retention program for high-risk employees",
      sprintf("Address satisfaction issues in %s department", dept_risks$department[1]),
      "Optimize resource allocation to improve utilization"
    ),
    strategic = c(
      "Develop predictive workforce planning capabilities",
      "Enhance skills-based project staffing",
      "Implement continuous feedback and engagement monitoring"
    )
  )

  return(list(
    key_metrics = list(
      total_employees = total_employees,
      attrition_rate = round(attrition_rate, 1),
      avg_satisfaction = round(avg_satisfaction, 2),
      high_risk_count = high_risk_count,
      on_time_delivery_rate = round(on_time_rate, 1)
    ),
    financial_impact = potential_savings,
    department_risks = dept_risks,
    executive_summary = executive_summary,
    recommendations = recommendations
  ))
}
