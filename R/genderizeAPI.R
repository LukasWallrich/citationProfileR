# the following code is adapted from
# https://github.com/coccopuffs/GenderGuesser/blob/master/data-raw/getCountryAndLanguageCodes.R
# and
# https://github.com/coccopuffs/GenderGuesser/blob/master/R/guessGender.R
# but uses the gender-api from https://gender-api.com/ as the means to predict gender instead


#' Get an element from a list
#'
#' Helper function that returns NA instead of NULL when a missing list element
#' is requested, otherwise returns the element itself.
#' @keywords internal
getListElement <- function(listName, elementName) {
  if (!is.null(listName[[elementName]])) {
    listElement <- listName[[elementName]]
  } else {
    listElement <- NA
  }
  return(listElement)
}

# API functions -----------------------------------------------------------

#' Look up a vector of names on https://gender-api.com/.
#'
#' @inheritParams guessGender
#' @keywords internal
lookupNameVectorGenderize <- function(nameVector, key = "ucThdyFEbbTRNp2EhSx4UUn3kMKMThqthcnZ") {

  # Construct the query
  query <- paste("name=", nameVector, sep = "", collapse = "&")
  query <- paste(query, "&key=", "ucThdyFEbbTRNp2EhSx4UUn3kMKMThqthcnZ", sep = "")

  cat(query)

  # Run it!
  queryResult <- httr::GET("https://gender-api.com/get?", query = query, httr::config(ssl_verifypeer = FALSE))

  if (httr::status_code(queryResult) == 200) {
    responseFromJSON <- jsonlite::fromJSON(httr::content(queryResult, as = "text"))
    responseDF <- data.frame(name = getListElement(responseFromJSON, "name"),
                             gender = getListElement(responseFromJSON, "gender"),
                             samples = getListElement(responseFromJSON, "samples"),
                             accuracy = getListElement(responseFromJSON, "accuracy"),
                             duration = getListElement(responseFromJSON, "duration"),
                             stringsAsFactors = FALSE)

  } else {
    cat(paste("\n!!!! http returned status code:",
              httr::status_code(queryResult),
              "!!!! message:",
              httr::http_status(queryResult)$message,
              "!!!! error:",
              httr::content(queryResult)$error,
              sep="\n"))
    if (httr::status_code(queryResult) == 429){
      cat('\n!!!! number of available requests exhaused')
    }
    responseDF <- NULL
  }
  return(responseDF)
}

#' Guess names' genders
#'
#' This function uses the https://gender-api.com/ API to supply estimates of the gender for one name.
#' @param name A name to look up.
#' @param apiKey An optional API key for https://gender-api.com/.
#' @export
#' @examples
#'guessGender("Natalie")
guessGender <- function(name, apiKey = NA) {
  # Run the queries
  responseDF <- lookupNameVectorGenderize(name, apiKey)
  return(responseDF)
}
