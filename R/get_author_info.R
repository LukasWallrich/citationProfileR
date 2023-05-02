#' get_author_info
#'
#' @param df data frame containing author, title and date
#'
#' @return dataframe with the first and last name of the cited authors
#' @export
#'
#' @examples
get_author_info <-function(df){
  #binding global variables for this function
  Found_author <- NULL
  #selecting the important things
  df <- df %>%
    dplyr::select('AUTHOR', 'TITLE', 'DATE', 'YEAR', 'DOI', 'index')

  #return(1)
  #get the list of papers that have DOI's present
  list_doi <- df %>%
    dplyr::filter(!is.na(df$DOI))


  #get the info on the papers with DOIs from crossref
  info_dois <- rcrossref::cr_works(dois = dplyr::pull(list_doi, "DOI"))
  info_df_dois <- info_dois$data


  info_df_dois <- info_df_dois %>%
    dplyr::select('doi', 'title', 'author') %>%
    tidyr::unnest(author) %>%
    dplyr::rename(DOI = "doi")

  # making the joining column lower to be able to match the info throughoutly and correctly
  info_df_dois$DOI <- tolower(info_df_dois$DOI)
  list_doi$DOI <- tolower(list_doi$DOI)

  #this for some reason is not filling in for all the secondary authors, just the main author. filter doi 10.1348/014466610X524263 on view to see what I mean
all_info_doi <- list_doi %>%
  dplyr::left_join(info_df_dois, by = "DOI",multiple = "all")
all_info_doi <- all_info_doi %>%
  dplyr::rename(OG_Author = "AUTHOR", OG_Title = "TITLE", Date = "DATE", Year = "YEAR", OG_doi = "DOI")

#---------------------------

  #get the list of papers that do not have DOIs
  na_list_doi <- df %>%
    dplyr::filter(is.na(df$DOI))

  #dataframe that will have all the previous information and a column for the author info from crossref(found_author)
  finished <- data.frame(matrix(nrow = 0, ncol = 7))
  column_names <- c("OG_Author","OG_Title","Date", "Year","OG_doi","index", "Found_author")
  colnames(finished) = column_names

  # This is how you create dataframes that you can nest (and unnest) in a dataframe
  not_found_df <- tibble::tibble(data = list(tibble::tibble(given ="No result matched" , family = "No result matched")))
  inconclusive_df <- tibble::tibble(data = list(tibble::tibble(given ="Inconclusive" , family = "Inconclusive")))

for(entry in 1:nrow(na_list_doi)){
  #pull the specific entries info from the original data to create a new table that contains both old and new info
  author <- na_list_doi$AUTHOR[entry]
  title <- na_list_doi$TITLE[entry]
  date <- na_list_doi$DATE[entry]
  year <- na_list_doi$YEAR[entry]
  og_doi <- na_list_doi$DOI[entry]
  index <- na_list_doi$index[entry]

  #test cases for the different available author/title info for each paper
  test_na_doi <-   if (!is.na(author) & !is.na(title)){   #both title and author have values
    rcrossref::cr_works(flq = c(query.author = author, query.bibliographic = title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
  }
  else if(!is.na(title) & is.na(author)){ #when the entry has a title and NA author
    rcrossref::cr_works(flq = c(query.bibliographic = title), limit = 1,sort='relevance', select = c('DOI', 'title', 'author', 'created', 'published-print', 'published-online', 'publisher-location'))
  }
  else if(is.na(title) & !is.na(author)){ #when the entry has NA-value title and non-NA value author
    NULL
  }else{NULL}


  #if the crossref return is not null
  if(!is.null(test_na_doi)){

    #get API payload
    data_returned <- test_na_doi$data

    # if the rcrossref returned no results, just add no results matched to that entry
    if(is.null(data_returned) | nrow(data_returned)== 0){ #is.null(dim(data_returned)). # we have to check for both cases, if we get a null return or if we get a return but it is empty
      finished[nrow(finished) + 1,] = c(author, title, date, year, og_doi, index, not_found_df)
      #move on to new entry
      next
    }
    else{
    # Add a new row with the entry information and add the nested author table from crossref into the Foune_author column
    finished[nrow(finished) + 1,] <- c(author, title, date, year, og_doi, index , NA)
    finished[nrow(finished), ]$Found_author <-  data_returned$author
    }
  }
  else{
    #no information was gathered return NAs
    finished[nrow(finished) + 1,] = c(author, title, date, year, og_doi,index, inconclusive_df)
  }
}

  #unnest the dataframe we got from crossref
  all_info_non_doi <-  finished %>%
    tidyr::unnest(Found_author)

    all_info_non_doi$Date <- as.numeric(all_info_non_doi$Date)
    all_info_non_doi$Year <- as.numeric(all_info_non_doi$Year)
    all_info_non_doi$index <- as.numeric(all_info_non_doi$index)

    # extract 4 consecutive numbers to a year with regex
    #

    all_info_doi$Date <- as.numeric(all_info_doi$Date)
    all_info_doi$Year <- as.numeric(all_info_doi$Year)
    all_info_doi$index <- as.numeric(all_info_doi$index)

  #bind both doi and non doi results together
  full_results <- dplyr::bind_rows(all_info_doi, all_info_non_doi)


return(full_results)
}
