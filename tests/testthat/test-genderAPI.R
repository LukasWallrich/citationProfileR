
#Test to make sure every name is associated with a probability/ checks size and shape
#and whether cache feature is working by checking if when cache = TRUE the duration is less than when cache = FALSE

#system.time({}) function taken from https://www.codingem.com/how-to-measure-execution-time-in-r/

test_that("guessGender", {
  false_refs <- guess_gender("Rithika", "US", cache = FALSE)
  false_time <- system.time({guess_gender("Rithika", "US", cache = FALSE)})

  true_refs <- guess_gender("Rithika", "US", cache = TRUE)
  true_time <- system.time({guess_gender("Rithika", "US", cache = TRUE)})

  expect_equal(ncol(true_refs), 6)
  expect_equal(nrow(true_refs), 1)

  #time when cache = TRUE should be less than when cache = FALSE
  expect_true(false_time[3] > true_time[3])
})


#Test to make sure the accuracy score is in the range of 0 <= accuracy <= 100

#any() function taken from https://www.tutorialspoint.com/check-if-any-value-in-an-r-vector-is-greater-than-or-less-than-a-certain-value#:~:text=To%20check%20if%20any%20value%20in%20an%20R,use%20the%20command%20given%20below%20%E2%88%92%20any%20%28V%3E100%29
#expect_false from https://www.rdocumentation.org/packages/testthat/versions/0.11.0/topics/expect_true

test_that("guessGender", {
  refs <- guess_gender("Rithika", "US", cache = FALSE)

  expect_false(any(refs[4] < 0), any(refs[4] > 100))
})


#Test to make sure that the country code impacts the result

test_that("guessGender", {
  us_result <- guess_gender("Rithika", "US", cache = FALSE)
  uk_result <- guess_gender("Rithika", "UK", cache = FALSE)

  comparison <- us_result == uk_result

  #this statement checks to see if results from changing countrycode affects the rest
  #both results should be different in at least one value
  expect_false(all(comparison) == TRUE)

})
