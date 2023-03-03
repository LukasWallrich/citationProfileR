
#f <- system.file("test-data", "Wallrich_et al_2020.pdf", package = "CitationProfileR")

df = c("Natalie", "Liam", "Eamon")
cr_code = "FR"

refs <- guessGender(df, countryCode = cr_code)

#Test to make sure every name is associated with a probablity/ checks size and shape
test_that("guessGender", {
  # Most basic test - correct number of references returned
  expect_equal(ncol(refs), 4) %>%
  expect_equal(nrow(refs), 3)

})

#Test to make sure the probability is in the range of 0 <= p <= 1

#any() function taken from https://www.tutorialspoint.com/check-if-any-value-in-an-r-vector-is-greater-than-or-less-than-a-certain-value#:~:text=To%20check%20if%20any%20value%20in%20an%20R,use%20the%20command%20given%20below%20%E2%88%92%20any%20%28V%3E100%29
#expect_false from https://www.rdocumentation.org/packages/testthat/versions/0.11.0/topics/expect_true
test_that("guessGender", {
  expect_false(any(refs[4] < 0), any(refs[4] > 1))
  #for (i in refs[4])
  #{
    #expect_false(any(i < 0), any(i > 1))
  #}
})

#Test to make sure gender data type is a character -- data type is a list so nevermind

#test_that("guessGender", {
  #expect_true(is.character(refs[2]))
#})

