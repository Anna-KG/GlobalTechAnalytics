# Test file for utility functions

test_that("validate_dataframe works correctly", {

  # Create test dataframe
  test_df <- data.frame(
    a = 1:5,
    b = letters[1:5],
    c = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  # Test successful validation
  expect_true(validate_dataframe(test_df, c("a", "b"), min_rows = 3))
  expect_true(validate_dataframe(test_df, c("a", "b", "c"), min_rows = 5))

  # Test validation failures
  expect_error(validate_dataframe(test_df, c("a", "d")))  # missing column
  expect_error(validate_dataframe(test_df, c("a", "b"), min_rows = 10))  # too few rows
  expect_error(validate_dataframe("not_a_df", c("a", "b")))  # not a dataframe
})

test_that("setup_logger works correctly", {

  # Test logger creation
  logger <- setup_logger("TestLogger")

  # Check logger structure
  expect_true(is.list(logger))
  expect_true("info" %in% names(logger))
  expect_true("warning" %in% names(logger))
  expect_true("error" %in% names(logger))

  # Check that logger functions exist and are callable
  expect_true(is.function(logger$info))
  expect_true(is.function(logger$warning))
  expect_true(is.function(logger$error))
})

test_that("calculate_business_days works correctly", {

  # Test business days calculation
  start_date <- as.Date("2023-01-02")  # Monday
  end_date <- as.Date("2023-01-06")    # Friday

  # Should be 4 business days (Tue, Wed, Thu, Fri)
  expect_equal(calculate_business_days(start_date, end_date), 4)

  # Test with weekend
  start_date2 <- as.Date("2023-01-06")  # Friday
  end_date2 <- as.Date("2023-01-10")    # Tuesday

  # Should be 2 business days (Monday, Tuesday)
  expect_equal(calculate_business_days(start_date2, end_date2), 2)

  # Test error handling
  expect_error(calculate_business_days("2023-01-01", end_date))
  expect_error(calculate_business_days(start_date, "2023-01-10"))
})

test_that("format_currency works correctly", {

  # Test currency formatting
  expect_equal(format_currency(1000), "EUR 1,000")
  expect_equal(format_currency(1234567), "EUR 1,234,567")
  expect_equal(format_currency(1000, "EUR "), "EUR 1,000")

  # Test with vector
  amounts <- c(1000, 2500, 3750)
  expected <- c("EUR 1,000", "EUR 2,500", "EUR 3,750")
  expect_equal(format_currency(amounts), expected)
})

test_that("calculate_percentile works correctly", {

  # Test percentile calculation
  test_values <- c(10, 20, 30, 40, 50)
  percentiles <- calculate_percentile(test_values)

  # Check that percentiles are in correct range
  expect_true(all(percentiles >= 0 & percentiles <= 100))
  expect_equal(length(percentiles), length(test_values))

  # Test specific case
  simple_values <- c(1, 2, 3, 4, 5)
  simple_percentiles <- calculate_percentile(simple_values)
  expect_equal(simple_percentiles[1], 20)  # first value should be 20th percentile
  expect_equal(simple_percentiles[5], 100) # last value should be 100th percentile
})
