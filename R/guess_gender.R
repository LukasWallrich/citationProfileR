# the following code is adapted from https://github.com/coccopuffs/GenderGuesser/blob/master/R/guessGender.R
# but uses the gender-api from https://gender-api.com/ as the means to predict gender instead

#' Guess a names' gender
#'
#' This function uses the https://gender-api.com/ API to supply estimates of the gender for one name.
#' @param name A name to look up.
#' @param key An optional API key for https://gender-api.com/.
#' @param cache (logical) local cache to avoid similar request combinations repeating
#' @keywords internal
#' @export
#' @examples
#'guess_gender("Natalie", key = "ucThdyFEbbTRNp2EhSx4UUn3kMKMThqthcnZ")
guess_gender <- function(name, key = "ucThdyFEbbTRNp2EhSx4UUn3kMKMThqthcnZ", cache = FALSE) {

  if (cache == TRUE) {
    req <- paste("name=", name, collapse = "&", "&key=", key, sep = "")
    #guess_gender_cache_env our environment
    rlang::env_cache(env = guess_gender_cache_env,
                     nm = req,
                     default = httr::GET("https://gender-api.com/get?", query = req, httr::config(ssl_verifypeer = FALSE)))
    #currently a NULL value
    queryResult <- guess_gender_cache_env$default
  }
  else {
    query <- paste("name=", name, collapse = "&", "&key=", key, sep = "")
    queryResult <- httr::GET("https://gender-api.com/get?", query = query, httr::config(ssl_verifypeer = FALSE))
  }


  if (httr::status_code(queryResult) == 200) {
    responseFromJSON <- jsonlite::fromJSON(httr::content(queryResult, as = "text"))
    responseDF <- data.frame(name = responseFromJSON[["name"]],
                             gender = responseFromJSON[["gender"]],
                             samples = responseFromJSON[["samples"]],
                             accuracy = responseFromJSON[["accuracy"]],
                             duration = responseFromJSON[["duration"]],
                             stringsAsFactors = FALSE)

  } else {
    warning(paste("\n!!!! http returned status code:",
                  httr::status_code(queryResult),
                  "!!!! message:",
                  httr::http_status(queryResult)$message,
                  "!!!! error:",
                  httr::content(queryResult)$error,
                  sep="\n"))
    if (httr::status_code(queryResult) == 429){
      stop("The number of available requests are exhausted")
    }
    responseDF <- NULL
  }

  return(responseDF)
}
