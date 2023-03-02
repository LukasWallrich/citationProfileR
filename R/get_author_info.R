library(rcrossref)

#' get_author_info
#'
#' @param df: data frame containing author, title and date
#'
#' @return dataframe with the first and last name of the cited authors
#' @export
#'
#' @examples
get_author_info <-function(df){
  df <- read_csv("R/test_citations_table.csv")

  df <- df %>%
    select(AUTHOR, TITLE, DATE, YEAR)


  practice_author <- df$AUTHOR[6]
  practice_title <- df$TITLE[6]
  practice_date <- df$DATE[6]

  practice_author <- strsplit(practice_author, split = ",")[[1]] #https://www.programmingr.com/tutorial/how-to-use-strsplit-in-r/ (list in list problem)
  practice_author <- practice_author[1]

 info <- cr_works(flq = c(query.author = practice_author, query.bibliographic = practice_title))

 res_json <- cr_works_(flq = c(query.author = practice_author, query.bibliographic = practice_title))
 unname(vapply(res_json, class, ""))
 lol <- jsonlite::fromJSON(res_json[[1]])

 hehe <- info[[data]]

}

