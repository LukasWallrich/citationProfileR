

test_that("location_cols", {

  #load a sample data set
  locations <- get_location(sample_data_frame)
  #most basic test - test that the function adds 2 more cols

  # Most basic test - correct number of references returned
  expect_equal(ncol(locations), ncol(sample_data_frame)+2)
})
