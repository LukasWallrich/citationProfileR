library(rcrossref)
library(tidyverse)

#' get_author_info
#'
#' @param df data frame containing author, title and date
#'
#' @return dataframe with the first and last name of the cited authors
#' @export
#'
#' @examples

#issues to work on:

# figure out nrow/ncol situation -- switch to nrow. might need to calculate nrow before we put it inthe for loop and then put the number itself.

# Might need to get rid of the variable "author" in the finished df

#

get_author_info <-function(df){
  # tester data and selecting important things
  df <- readr::read_csv("R/test_citations_table.csv")
  df <- df %>%
    dplyr::select('AUTHOR', 'TITLE', 'DATE', 'YEAR', 'DOI')

  #get the list of papers that have DOI's present
  list_doi <- df %>%
    select(DOI) %>%
    filter(!is.na(DOI))


  #create an empty df where we will put the info from crossref
  doi_finished <- data.frame(matrix(nrow = 0, ncol = 6))
  doi_column_names <- c("Title", "Year", "Given", "Family", "Sequence", "DOI")
  colnames(doi_finished) = doi_column_names

  for(entry in 1:nrow(list_doi)){
    #pull the respective DOI
    entry_doi <- list_doi$DOI[entry]

    print("we are on entry")
    print(entry)

    #get the info on the papers with DOIs from crossref
    info_dois <- rcrossref::cr_works(dois = entry_doi)


  #if there are citations that have DOIs present turn it into a data frame
  if(!is.null(info_dois)){ ## THIS MIGHT NEED TO GO ON LINE 28 before the for loop

    #get API payload
    doi_data_returned <- info_dois$data
    print("data returned is")
    print(doi_data_returned)

    print("The dims are")
    print(dim(doi_data_returned))

    #get the title, date, affiliation info
    title <- doi_data_returned$title
    date <- doi_data_returned$created
    authors <- doi_data_returned$author

    # if the rcrossref returned no results, just add no results matched to that entry
    if(nrow(doi_data_returned)==0){ #is.null(dim(data_returned))
      print("I am getting into the null dim if statement")
      first_name <- "No result matched"
      doi_finished[nrow(doi_finished) + 1,] = c(title, date, first_name, NA, NA, entry_doi)
      #move on to new entry
      next
    }

    crossref_info <- doi_data_returned[[30]][[1]]
    #create a duplicate row for each author of the paper
    for(contributor in 1:nrow(crossref_info)){
      doi_finished[nrow(doi_finished) + 1,] = c(title, date, crossref_info[contributor,1],crossref_info[contributor,2], crossref_info[contributor,3], entry_doi)
    }
  }else{
    #no information was gathered return NAs
    first_name <- "inconclusive"
    doi_finished[nrow(doi_finished) + 1,] = c(title, date, first_name, NA, NA, entry_doi)
  }
  print("Last updated finished is")
  print(doi_finished)

}

#---------------------------




  #get the list of papers that do not have DOIs
  na_list_doi <- df %>%
    filter(is.na(DOI))

#practice test cases for papers that have no DOI present
# no_author_title <- na_list_doi$TITLE[12]
# no_author_author <- na_list_doi$AUTHOR[12]
# no_title_author <- na_list_doi$AUTHOR[7]
# no_title_title <- na_list_doi$TITLE[7]
#
# # splitting the last name and initial to get only last name
# no_title_author <- strsplit(no_title_author, split = ", ") #https://www.programmingr.com/tutorial/how-to-use-strsplit-in-r/ (list in list problem)
# practice_author <- matrix(unlist(no_title_author),ncol=2,byrow=T)
# only_last_name <- practice_author[1, 1]
# only_initial <- practice_author[1,2]
finished <- data.frame(matrix(nrow = 0, ncol = 6))
column_names <- c("Author","Title", "Year", "Given", "Family", "Sequence")
colnames(finished) = column_names

  # data.frame(Author= c(),
  #                      Title = c(),
  #                      Year = c(),
  #                      given = c(),
  #                      family = c(),
  #                      sequence = c())


for(entry in 1:ncol(na_list_doi)){
  #pull the author, title, date entries from the NA-DOI data
  author <- na_list_doi$AUTHOR[entry]
  title <- na_list_doi$TITLE[entry]
  date <- na_list_doi$YEAR[entry]

  print("we are on entry")
  print(entry)
  #test cases for the different available author/title info for each paper
  #both title and author have values
  test_na_doi <-   if (!is.na(author) & !is.na(title)){
    rcrossref::cr_works(flq = c(query.author = author, query.bibliographic = title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
    #when the entry has a title and NA author
  } else if(!is.na(title) & is.na(author)){
    rcrossref::cr_works(flq = c(query.bibliographic = title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
    #when the entry has NA-value title and non-NA value author
  } else if(is.na(title) & !is.na(author)){
    NA
  }#we only get the most relevant match based on rcrossref's `relevance` sorting. We do not check that the titles match each other.

  print("test na doi is ")
  print(test_na_doi)




  #if we pulled info from crossref
  if(!is.null(test_na_doi)){

    #get API payload
    data_returned <- test_na_doi$data
    print("data returned is")
    print(data_returned)

    print("The dims are")
    print(dim(test_na_doi))


    # if the rcrossref returned no results, just add no results matched to that entry
    if(nrow(data_returned)==0){ #is.null(dim(data_returned))
      print("I am getting into the null dim if statement")
      first_name <- "No result matched"
      finished[nrow(finished) + 1,] = c(author, title, date, first_name, NA, NA)
      #move on to new entry
      next
    }

    crossref_info <- data_returned[[5]][[1]]
    #create a duplicate row for each author of the paper
    for(contributor in 1:nrow(crossref_info)){
      finished[nrow(finished) + 1,] = c(author, title, date, crossref_info[contributor,1],crossref_info[contributor,2], crossref_info[contributor,3])
    }
  }else{
    #no information was gathered return NAs
    first_name <- "inconclusive"
    finished[nrow(finished) + 1,] = c(author, title, date, first_name, NA, NA)
  }
  print("Last updated finished is")
  print(finished)

}
#doi_no_doi_data <- rbind(finished, info_dois) #this is for when we get the list of names for the doi-present entries
return(finished)
}

