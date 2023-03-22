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
    dplyr::select('AUTHOR', 'TITLE', 'DATE', 'YEAR', 'DOI')
  #get the list of papers that have DOI's present
  list_doi <- df %>%
    select(DOI) %>%
    filter(!is.na(DOI))

  #get the list of papers that do not have DOIs
  na_list_doi <- df %>%
    filter(is.na(DOI))

#get the info on the papers with DOIs from crossref
info_dois <- rcrossref::cr_works(dois = pull(list_doi, DOI))

#practice test cases for papers that have no DOI present
no_author_title <- na_list_doi$TITLE[12]
no_author_author <- na_list_doi$AUTHOR[12]
no_title_author <- na_list_doi$AUTHOR[7]
no_title_title <- na_list_doi$TITLE[7]

# splitting the last name and initial to get only last name
no_title_author <- strsplit(no_title_author, split = ", ") #https://www.programmingr.com/tutorial/how-to-use-strsplit-in-r/ (list in list problem)
practice_author <- matrix(unlist(no_title_author),ncol=2,byrow=T)
only_last_name <- practice_author[1, 1]
only_initial <- practice_author[1,2]

#test cases for the different available author/title info for each paper
                  #both title and author have values
test_na_doi <-   if (!is.na(no_title_author) & !is.na(no_title_title)){
    rcrossref::cr_works(flq = c(query.author = no_author_author, query.bibliographic = no_title_title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
    #when the entry has a title and NA author
  } else if(!is.na(no_title_title) & is.na(no_title_author)){
    rcrossref::cr_works(flq = c(query.bibliographic = no_title_title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
    #when the entry has NA-value title and non-NA value author
  } else if(is.na(no_title_title) & !is.na(no_title_author)){
    NA
  }#we only get the most relevant match based on rcrossref's `relevance` sorting. We do not check that the titles match each other.

 #data frame with info
  if(!is.na(test_na_doi)){
    data_returned <- test_na_doi$data
  }else{
    data_returned <- test_na_doi
  }
}
