
df = c("Natalie", "Liam", "Eamon")
cr_code = "FR"

#f <- system.file("test-data", "Wallrich_et al_2020.pdf", package = "CitationProfileR")

refs <- guessGender(df, countryCode = cr_code)

test_that("guessGender", {
  # Most basic test - correct number of references returned
  expect_equal(nrow(refs), 3)
})
