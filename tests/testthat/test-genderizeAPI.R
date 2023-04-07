
df = c("Natalie")
key = "ucThdyFEbbTRNp2EhSx4UUn3kMKMThqthcnZ"

refs <- guess_gender(df, key)

#Test to make sure every name is associated with a probability/ checks size and shape

test_that("guessGender", {
  expect_equal(ncol(refs), 5)
  expect_equal(nrow(refs), 1)
})


#Test to make sure the probabilities p are in the range of 0 <= p <= 1

#any() function taken from https://www.tutorialspoint.com/check-if-any-value-in-an-r-vector-is-greater-than-or-less-than-a-certain-value#:~:text=To%20check%20if%20any%20value%20in%20an%20R,use%20the%20command%20given%20below%20%E2%88%92%20any%20%28V%3E100%29
#expect_false from https://www.rdocumentation.org/packages/testthat/versions/0.11.0/topics/expect_true

test_that("guessGender", {
  expect_false(any(refs[4] < 0), any(refs[4] > 100))
})


