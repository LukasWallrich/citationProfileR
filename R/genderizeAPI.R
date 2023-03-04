# the following code copied from
# https://github.com/coccopuffs/GenderGuesser/blob/master/data-raw/getCountryAndLanguageCodes.R
# and
# https://github.com/coccopuffs/GenderGuesser/blob/master/R/guessGender.R

genderizeCountries <- readr::read_csv("country_codes.csv", show_col_types = FALSE)$"Code"

# Code for using the genderize.io API to guess names' genders.

# Helper functions --------------------------------------------------------

#' Check country code.
#' Ensures that any code specified is recognized by genderize.io.
#' @keywords internal
checkLanguageCountryCodes <- function(countryCode) {
  checkCodeInVector <- function(code, codeVector) {
    return(match(tolower(code), tolower(codeVector), nomatch = 0) > 0)
  }
  if (!is.na(countryCode)) {
    if (!checkCodeInVector(countryCode, genderizeCountries)) {
      stop("Country code not in list")
    }
  }
}

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

#' Look up a vector of names on genderize.io.
#'
#' This function actually implements the genderize.io API. Can only query 10
#' names at a time.
#' @inheritParams guessGender
#' @keywords internal
lookupNameVectorGenderize <- function(nameVector,
                                      countryCode = NA, apiKey = "7dffb3b373a9efd349a01c214e59e00b") {
  # Make sure that no more than 10 names were passed
  checkLanguageCountryCodes(countryCode)

  # Construct the query
  query <- paste("name[", seq_along(nameVector), "]=", nameVector,
                 sep = "",
                 collapse = "&")
  if (!is.na(countryCode)) {
    query <- paste(query, "&country_id=", countryCode, sep = "")
  }
  cat("mahesh")

  query <- paste(query, "&apikey=", "7dffb3b373a9efd349a01c214e59e00b", sep = "")

  cat(query)

  # Run it!
  # XXX - setting ssl_verifypeer to FALSE is probably really bad. Whatev.
  queryResult <- httr::GET("https://api.genderize.io", query = query,
                           httr::config(ssl_verifypeer = FALSE))
  if (httr::status_code(queryResult) == 200) {
    responseFromJSON <- jsonlite::fromJSON(httr::content(queryResult, as = "text"))
    # Make sure this is a data.frame with the correct columns. I bet fromJSON
    # can do this for me but I don't know how. This code works whether fromJSON
    # returned a list (the response to one name) or a data.frame (the response
    # to several).
    responseDF <- data.frame(name = getListElement(responseFromJSON, "name"),
                             gender = getListElement(responseFromJSON, "gender"),
                             country_id = getListElement(responseFromJSON, "country_id"),
                             # language_id = getListElement(responseFromJSON, "language_id"),
                             probability = getListElement(responseFromJSON, "probability"),
                             count = getListElement(responseFromJSON, "count"),
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
#' This function uses the genderize.io API to supply estimates of the gender one
#' or more names.
#' @param nameVector A vector containing one or more names to look up.
#' @param countryCode An optional ISO 3166-1 alpha-2 country code.
#' @param languageCode An optional ISO 639-1 language code. Only one of
#'   countryCode or languageCode can be specified.
#' @param apiKey An optional API key for genderize.io.
#' @export
#' @examples
#' guessGender(c("Natalie", "Liam", "Eamon"), countryCode = "US")
guessGender <- function(nameVector,
                        countryCode = NA, apiKey = NA) {
  checkLanguageCountryCodes(countryCode)

  # genderize.io only handles 10 names at a time. Create a list of vectors, each
  # with no more than 10 names.
  queryList <- list()
  while(length(nameVector) > 10) {
    queryList[[length(queryList)+1]] <- nameVector[1:10]
    nameVector <- nameVector[11:length(nameVector)]
  }
  queryList[[length(queryList)+1]] <- nameVector

  # Run the queries
  responseList <- list()
  for (i in seq_along(queryList)) {
    responseDF <- lookupNameVectorGenderize(queryList[[i]], countryCode, apiKey)
    if (is.null(responseDF)) {
      break
    } else {
      responseList[[length(responseList)+1]] <- responseDF
    }
  }

  final_output <- responseDF %>%
    dplyr::select("name", "gender", "country_id", "probability")

  return(final_output)
}
