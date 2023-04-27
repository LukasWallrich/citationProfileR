
#for some reason this is not working
f <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
df <- readr::read_csv(f)

# ensure that for each GROBID detected citation, get author info got more info or at
test_that ("return_answer_for_all", {

  #how many unique entries (index) are there in the parameter dataframe original <- df %>%
  original <- df %>%
  dplyr::group_by (index) %>%
  dplyr::summarize(count = dplyr::n())

  info_gathered <- get_author_info(df)

  info_gathered_unique <- info_gathered %>%
    dplyr::group_by(index) %>%
    dplyr::summarize(n = dplyr::n())

  unique_entries_count <- nrow(original)
  info_gathered_unique_entries <- nrow(info_gathered_unique)

  expect_equal(unique_entries_count, info_gathered_unique_entries)
})

#ensure that there are no mismatched when joining by having entries that have NAs
test_that ("no_unidentifiable_entries", {

  info_gathered <- get_author_info(df)

  #check if any entry that was collected by get_author_info is na
  index_na <- info_gathered %>%
    dplyr::filter(is.na(index))

  expect_equal(nrow(index_na), 0)
})


