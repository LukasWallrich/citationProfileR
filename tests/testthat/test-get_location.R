#load a sample data set
file_path <- system.file("test-data", "test_data_crossref_results.csv", package = "CitationProfileR")
f <- read.csv(file_path)
locations <- get_location(f)
#most basic test - test that the function adds 2 more cols

test_that("location_cols", {
  # Most basic test - correct number of references returned
  expect_equal(ncol(locations), ncol(sample_data_frame)+2)
})

