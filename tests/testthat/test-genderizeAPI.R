
#f <- system.file("test-data", "Wallrich_et al_2020.pdf", package = "CitationProfileR")

df = c("Natalie", "Liam", "Eamon")
cr_code = "FR"

refs <- guessGender(df, countryCode = cr_code)

#Test to make sure every name is associated with a probablity/ checks size and shape
test_that("guessGender", {
  # Most basic test - correct number of references returned
  expect_equal(ncol(refs), 5) %>%
  expect_equal(nrow(refs), 3)

})

#Test to make sure the probability is in the range of 0 <= p <= 1

#Test to make sure a gender is output maybe check if there is a string variable?

#rows and columns; first column is name, second column is gender, third column is probablity
