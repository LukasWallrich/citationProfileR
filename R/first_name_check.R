
first_name_check <- function(df){

  df <- readr::read_csv("R/test_citations_table.csv")

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
    map(split, char_length)
  }

  #gets the length of each split section (split using commas)
  split <- map(df$AUTHOR, split_trans)

  # returns True if there are 1 length values in the author entries
  if_abbrev <- function(x){
    #if the author is NA then return true bc its like an abreviation, we should try to find first name
    if(is.na(x)){
      return(TRUE)
    }
    #find ones in list and see how many are there in the list
    amount_of_ones <- c(1) %in% x
    ones_present <- length(amount_of_ones)

    #if zero 1 found, return false
    if(ones_present == 0 ){
      FALSE
    }
    else{
      TRUE
    }
  }

  # vector containing TRUE if there is an abbreviated author
  abbrev_bool <- map(split, if_abbrev)

  #getting the indices of where there is abbreviated authors
  abbrev_indices <- which(abbrev_bool == TRUE)

  #have first name supposedly
  full_names_indeces <- which(abbrev_bool == FALSE)

  #getting dataframe
  abbrev_df <- df[abbrev_indices, ]

  full_df <- df[full_names_indeces,]

}
