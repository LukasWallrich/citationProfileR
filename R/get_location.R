#' get_location
#'
#' @return
#' @export
#'
#' @examples

# load library(tmaptools)
#load library(tidyverse), library(countrycode), library(purrr)

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

          #if OSM does not return a geolocation, try to split according to commas
          if(is.null(OSM_affiliation_returned)){
            #cases to test -- row 36 Centre for Social Investigation, Nuffield College, University of Oxford, Oxford, UK

            print("No OSM data returned, now checking the comma splitting")

            #if the affiliation has any commas, proceed to comma splitting
            if(str_count(affiliation, ",") > 0){
              #count the number of commas present in a affiliation phrase
              num_commas <- str_count(affiliation, ",")

              #find the location of the last comma
              locations_all_commas <- str_locate_all(pattern = ",", affiliation)
              last_comma_location <- max(locations_all_commas[[1]])
              #if the phrase has a comma in the end and nothing after, get rid of the comma, update affiliation
              affiliation <- ifelse(last_comma_location == nchar(affiliation),
                                    substr(affiliation, 1, nchar(affiliation)-1),
                                    affiliation)

              #if n commmas are present, I will choose the phrase after n commas.
              last_phrase <- substr(affiliation, last_comma_location+1, nchar(affiliation))
              #get the OSM location
              OSM_last_phrase <- geocode_OSM(last_phrase)
              #if OSM location exists for the last phrase, get the lcoation name
              if(!is.null(OSM_last_phrase)){
                #get the location list
                last_phrase_location <- rev_geocode_OSM(OSM_last_phrase$coords["x"], OSM_last_phrase$coords["y"])
                #get the location code
                last_phrase_country_code <- last_phrase_location[[1]]$country_code
                crossref_data$country_code[entry] <-  toupper(last_phrase_country_code)
                next
              }else{#if after comma splitting OSM does not return anything
                #put NA in the entry
                crossref_data$country_code[entry] <-  NA
                next
              }

            }else{# if the affiliation has no commas and OSM does not return anything
              #put NA into this entry and move to the next one
              crossref_data$country_code[entry] <-  NA
              next
            }

          }else{
            #if the OSM returns a geolocation, get the country code using OSM
            print("I am in the location list place")
            location_list <- rev_geocode_OSM(OSM_affiliation_returned$coords["x"], OSM_affiliation_returned$coords["y"])
            print("I got the location list")
            #Get the country code of the affiliation
            list_country_code <- location_list[[1]]$country_code
            print(c("I got the list_country_code", list_country_code))
            #add this country code to the respective column
            crossref_data$country_code[entry] <-  toupper(list_country_code)
            print(crossref_data$country_code[entry] )
            next
          }
        }else{
          #countrycode returns country names, turn it into 2 letter country codes
          countrycode_pkg_return <- toupper(countrycode(countrycode_pkg_return, origin = 'country.name', destination = 'iso2c'))
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


}#function
