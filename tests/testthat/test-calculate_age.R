test_that("calculate_age works with different date formats", {
  # Test M/D/YYYY format
  age1 <- calculate_age("01/01/2020", reference_date = as.Date("2023-01-01"))
  expect_true(is.numeric(age1))
  expect_true(age1 > 2.9 && age1 < 3.1)  # Approximately 3 years
  
  # Test with Date object
  age2 <- calculate_age(as.Date("2020-01-01"), reference_date = as.Date("2023-01-01"))
  expect_true(is.numeric(age2))
  
  # Test with NA
  age3 <- calculate_age(NA)
  expect_true(is.na(age3))
  
  # Test with empty string
  age4 <- calculate_age("")
  expect_true(is.na(age4))
})
