# the following code is adapted from https://github.com/coccopuffs/GenderGuesser/blob/master/R/guessGender.R
# but uses the gender-api from https://gender-api.com/ as the means to predict gender instead


#' Users can replace the default API key with their own for the gender-api from https://gender-api.com/
#'
#' @param key A user's API key for https://gender-api.com/.
#'
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' #replace_key(key)
#' }
replace_key <- function(key) {
  writeLines(key, con = "./api_keys")
}

#' Guess a names' gender
#'
#' This function uses the https://gender-api.com/ API to supply estimates of the gender for one name.
#' @param name A name to look up.
#' @param key An optional API key for https://gender-api.com/.
#' @param cache (logical) local cache to avoid similar request combinations repeating.
#' @param countrycode The country code that the user inputs.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' guess_gender("Rithika", "US")
#' }

guess_gender <- function(name, countrycode = countrycode, key = NA, cache = FALSE) {

  if(is.na(key)) {
    key = readLines("./api_keys")[1]
  }

  if (cache == TRUE) {
    #if query in cache, first retrieve information from cache
    cache_entry <- rlang::env_get(guess_gender_cache_env, name, countrycode)

    #check if cache entry is empty
    if(!is.null(cache_entry)) {
      #if the cache entry isn't empty, we return the result that's already there
      return(cache_entry)
    }
   }

  else {
    #get res from api but first we have to check if we predict the gender keeping the countrycode in mind or not
    if (!is.na(countrycode)) {
      query <- paste("name=", name, collapse = "&", "&country=", countrycode, "&key=", key, sep = "")
    }
    else {
      query <- paste("name=", name, collapse = "&", "&key=", key, sep = "")
    }

    queryResult <- httr::GET("https://gender-api.com/get?", query = query, httr::config(ssl_verifypeer = FALSE))

    #retrieve the information from api results when query status is clear
    if (httr::status_code(queryResult) == 200) {
      responseFromJSON <- jsonlite::fromJSON(httr::content(queryResult, as = "text"))
      responseDF <- data.frame(name = responseFromJSON[["name"]],
                               gender = responseFromJSON[["gender"]],
                               countrycode,
                               samples = responseFromJSON[["samples"]],
                               accuracy = responseFromJSON[["accuracy"]],
                               duration = responseFromJSON[["duration"]],
                               stringsAsFactors = FALSE)

    #when query isn't able to be processed we return a warning or stop message
    } else {
      warning(paste("\n!!!! http returned status code:",
                    httr::status_code(queryResult),
                    "!!!! message:",
                    httr::http_status(queryResult)$message,
                    "!!!! error:",
                    httr::content(queryResult)$error,
                    sep="\n"))
      if (httr::status_code(queryResult) == 429) {
        stop("The number of available requests are exhausted")
      }
      responseDF <- NULL
    }
  }

    #store res in cache only if the prediction isn't null
  if (!is.null(responseDF)) {
    rlang::env_cache(guess_gender_cache_env, nm = name, default = responseDF)
  }

  return(responseDF)
}
