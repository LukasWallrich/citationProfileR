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
    filter(!is.na(DOI))


  #get the info on the papers with DOIs from crossref
  info_dois <- rcrossref::cr_works(dois = pull(list_doi, DOI))
  info_df_dois <- info_dois$data

#--------
  doi_finished <- data.frame(matrix(nrow = 0, ncol = 6))
  doi_column_names <- c("Title", "Year", "Given", "Family", "Sequence", "DOI")
  colnames(doi_finished) = doi_column_names


  # need to duplicate the rows for papers with multiple author entries	  for(entry in 1:nrow(list_doi)){
  #this code is copied from non-DOI code below	    #pull the respective DOI
  #if we pulled info from crossref	    entry_doi <- list_doi$DOI[entry]
  if(!is.null(test_na_doi)){
    print("we are on entry")
    print(entry)

    #get the info on the papers with DOIs from crossref
    info_dois <- rcrossref::cr_works(dois = entry_doi)

  }

  ## questions: From doing this, we are getting the right authors but will the additional authors also be correct? How do DOI work?
  ## in some there is affiliation.info_df_dois[[30]][[3]] in other there is not [[30]][[5]]

#---------------------------


  #get the list of papers that do not have DOIs
  na_list_doi <- df %>%
    filter(is.na(DOI))


  finished <- data.frame(matrix(nrow = 0, ncol = 6))
  column_names <- c("Author","Title", "Year", "Given", "Family", "Sequence")
  colnames(finished) = column_names


  for(entry in 1:nrow(na_list_doi)){
    #pull the author, title, date entries from the NA-DOI data
    author <- na_list_doi$AUTHOR[entry]
    title <- na_list_doi$TITLE[entry]
    date <- na_list_doi$YEAR[entry]

    print("we are on entry")
    print(entry)

    print("author is ")
    print(author)

    #test cases for the different available author/title info for each paper
    #both title and author have values
    date_title <- if(!is.na(date) & !is.na(title)){
      paste0(title, " ", date)
    } else{
        title
      }

    test_na_doi <-   if (!is.na(author) & !is.na(title)){
      rcrossref::cr_works(flq = c(query.author = author, query.bibliographic = date_title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
      #when the entry has a title and NA author
    }
    else if(!is.na(title) & is.na(author)){
      rcrossref::cr_works(flq = c(query.bibliographic = date_title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
      #when the entry has NA-value title and non-NA value author
    }
    else if(is.na(title) & !is.na(author)){
      NULL
    }#we only get the most relevant match based on rcrossref's `relevance` sorting. We do not check that the titles match each other.

    print("test na doi is ")
    print(test_na_doi)


    #if we pulled info from crossref
    if(!is.null(test_na_doi)){

      #get API payload
      data_returned <- test_na_doi$data
      print("data returned is")
      print(data_returned)

      print("The author data is")
      #print(data_returned[[5]][[1]])

      print("The dims are")
      print(dim(data_returned))

      print("The nrow is")
      print(nrow(data_returned))



      # if the rcrossref returned no results, just add no results matched to that entry
      if(is.null(data_returned) | nrow(data_returned)== 0){ #is.null(dim(data_returned)). # we have to check for both cases, if we get a null return or if we get a return but it is empty
        print("I am getting into the null dim if statement")
        first_name <- "No result matched"
        finished[nrow(finished) + 1,] = c(author, title, date, first_name, NA, NA)
        #move on to new entry
        next
      }


      crossref_info <- data_returned[[5]][[1]] #pull(data_returned, author) this gives error need to fix
      #create a duplicate row for each author of the paper
      for(contributor in 1:nrow(crossref_info)){
        finished[nrow(finished) + 1,] = c(author, title, date, crossref_info[contributor,1],crossref_info[contributor,2], crossref_info[contributor,3])
      }
    }
    else{
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

