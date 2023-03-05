library(rcrossref)

#' get_author_info
#'
#' @param df data frame containing author, title and date
#'
#' @return dataframe with the first and last name of the cited authors
#' @export
#'
#' @examples
get_author_info <-function(df){
  # tester data and selecting important things
  df <- readr::read_csv("R/test_citations_table.csv")
  df <- df %>%
    dplyr::select('AUTHOR', 'TITLE', 'DATE', 'YEAR')

  #practice examples
  practice_author <- df$AUTHOR[6]
  practice_title <- df$TITLE[6]
  practice_date <- df$DATE[6]

  # splitting the last name and initial to get only last name
  practice_author <- strsplit(practice_author, split = ",")[[1]] #https://www.programmingr.com/tutorial/how-to-use-strsplit-in-r/ (list in list problem)
  practice_author <- practice_author[1]

  #relevance/score is the score of the  match from the paper it returns
  #add the year of publication
 info <- rcrossref::cr_works(flq = c(query.author = practice_author, query.bibliographic = practice_title), limit = 3,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))

 #data frame with info
  data_returned <- info$data
}
