#' First Name Check
#'
#' @param df A data frame that has the extracted citations from the pdf using GROBID
#'
#' @return a list with two data frames: The first index is entries that have abbreviated authors and the second has author's first name
#' @export
#'
#' @examples
#' file_path <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
#' sample_data_frame <- read.csv(file_path)
#' first_name_check(sample_data_frame)

first_name_check <- function(df){
  split_trans <- function(x){
    # removing the spaces so they do not count on vector length
    x <- gsub(" ", "",x)
    # change all symbols that arw not commas or alphanumeric with comma (if last names have hyphen they will be separated but I dont think there are names that are just one letter then hypen then it is good)
    x <- gsub("[^[:alnum:],]", ",", x)
    #get rid of repeated commas if there are symbols infront of each other
    x <- gsub(",+", ",", x)
    # split the author
    split <- strsplit(x, ",")

    #function that gives us the character length
    char_length <- function(split){nchar(split)}

    #get character length for each vector
    purrr::map(split, char_length)
  }

  #gets the length of each split section (split using commas)
  split <- purrr::map(df$AUTHOR, split_trans)

  # returns True if there are 1 length values in the author entries
  if_abbrev <- function(x){
    #if the author is NA then return true bc its like an abreviation, we should try to find first name
    if(is.na(x)){
      return(TRUE)
    }
    #find ones in list and see how many are there in the list
    amount_of_ones <- c(1) %in% x
    print(amount_of_ones)
    #print("amount of ones is ", amount_of_ones)
    ones_present <- length(amount_of_ones)
    print(ones_present)

    # if there are no abbreviations (ones_present !-0) or none in amoun
    if(ones_present == 0 || amount_of_ones == FALSE ){
      FALSE
    }
    else{
      TRUE
    }
  }

  # vector containing TRUE if there is an abbreviated author
  abbrev_bool <- purrr::map(split, if_abbrev)

  #getting the indices of where there is abbreviated authors
  abbrev_indices <- which(abbrev_bool == TRUE)

  #have first name supposedly
  full_names_indeces <- which(abbrev_bool == FALSE)

  #getting dataframe
  abbrev_df <- df[abbrev_indices, ]

  full_df <- df[full_names_indeces,]

  return(list(abbrev_df, full_df))
}

#full_name_paper <- readr::read_csv("R/Johanna_Moderation_not_abbrev")

#lol <- first_name_check(full_name_paper)
#should it be better if I actually make another column that says if we have to
#look for the name in cross ref or just extract it somehow
