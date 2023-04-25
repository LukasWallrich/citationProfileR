
#' get_location
#'
#' @return
#' @export
#'
#' @examples

# load library(tmaptools)
#load library(tidyverse), library(countrycode), library(purrr)

#affiliation name must be string
get_location <- function(df_with_affiliation){

  #load in the sample result from the crossref_api()
  crossref_data <- read_csv("R/crossref_results.csv") %>%
    select(-`...1`)
  #add an empty column country_code where the for loop will add values
  crossref_data <- cbind(crossref_data, country_code=NA)

  for(entry in 1:nrow(crossref_data)){
    #pull the specific affiliation one by one
    affiliation <- crossref_data$affiliation.name[entry]

    print(c("we are on entry", entry, affiliation))

    #if the affiliation entry is not NA
    if(!is.na(affiliation)){
      #use the `countrycode` package to see if the country name is returned
      countrycode_pkg_return <- countrycode::codelist$country.name.en[stringr::str_detect(affiliation, stringr::regex(countrycode::codelist$country.name.en.regex, ignore_case = TRUE))]

      print(c("Got location code", countrycode_pkg_return))

        #if `countrycode` package does not return any information, use OSM
        if(is_empty(countrycode_pkg_return)){
          #get the information about the location using OSM
          OSM_affiliation_returned <- geocode_OSM(affiliation)

          #print(c("OSM affiliation returned is"), OSM_affiliation_returned$coords["x"])
          #if OSM does not return a geolocation, try to split according to commas, then
          if(is.null(OSM_affiliation_returned)){
            #move onto the next entry
            #TO DO
            print("No OSM data returned")
            crossref_data$country_code[entry] <-  NA
            next

          }else{
            #if the OSM returns a geolocation, get the country code using OSM
            location_list <- rev_geocode_OSM(OSM_affiliation_returned$coords["x"], OSM_affiliation_returned$coords["y"])

            #Get the country code of the affiliation
            list_country_code <- location_list[[1]]$country_code

            #add this country code to the respective column
            crossref_data[nrow(crossref_data), ]$country_code <-  list_country_code
          }
        }else{
          #print(c("I already got the country code from `countrycode`"), countrycode_pkg_return)
          #put the info returned by the `countrycode` pkg in the df
          crossref_data$country_code[entry] <-  countrycode_pkg_return
          next
        }

    }else{     #if the entry is NA
      crossref_data$country_code[entry] <-  NA
      next
    }
  return(crossref_data)
  }#for loop

#TO DO:
#1. need to save the entries I get from the for loop
#2. code it to not stop when there's an error in download.file()

}
loc <- geocode_OSM("University of Osnabrück, Germany")
location_list <- rev_geocode_OSM(loc$coords["x"], loc$coords["y"])



location_list[[1]]$country_code
