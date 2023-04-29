#' get_location
#'
#' @param data_with_affiliations Data frame containing affiliations. If using Shiny App, the data will come from `crossref_api()` function
#' @param affiliations_col_name The name of the affiliations column.
#'
#' @return Dataframe with country names and country codes based on the inputted affiliation
#' @importFrom tmaptools geocode_OSM
#' @importFrom stringr str_count
#' @importFrom stringr str_locate_all
#' @importFrom tmaptools rev_geocode_OSM
#' @importFrom countrycode countrycode
#'
#' @export
#'
#' @examples
#' file_path <- system.file("test-data", "test_data_crossref_results.csv", package = "CitationProfileR")
#' sample_data_frame <- read.csv(file_path)
#' get_location(sample_data_frame)


# load library(tmaptools)
#load library(tidyverse), library(countrycode), library(purrr)

#RUN THIS IF YOU WANT TO LOAD A SAMPLE DF
# data_with_affiliations <- read_csv("inst/test-data/test_data_crossref_results.csv")

get_location <- function(data_with_affiliations, affiliations_col_name){

  #If both country_code and country_name exist, do not create a new column name that
  if("country_code" %in% colnames(data_with_affiliations) & "country_name" %in% colnames(data_with_affiliations)){
    data_with_affiliations$country_code
  }else if(!"country_code" %in% colnames(data_with_affiliations) & !"country_name" %in% colnames(data_with_affiliations)){
    #if neither cols exist, create new cols

    #add an empty column country_code
    data_with_affiliations <- cbind(data_with_affiliations, country_code=NA)

    #add an empty column country_name
    data_with_affiliations <- cbind(data_with_affiliations, country_name=NA)

  }else if("country_code" %in% colnames(data_with_affiliations) & !"country_name" %in% colnames(data_with_affiliations)){
    #if country_name does not exist, create a new col
    data_with_affiliations <- cbind(data_with_affiliations, country_name=NA)

  }else if(!"country_code" %in% colnames(data_with_affiliations) & "country_name" %in% colnames(data_with_affiliations)){
    #if country_code does not exist, create a new col
    data_with_affiliations <- cbind(data_with_affiliations, country_code=NA)
  }


  for(entry in 1:nrow(data_with_affiliations)){
    #pull the specific affiliation one by one
    affiliation <- data_with_affiliations$affiliations_col_name[entry]

    #if the affiliation entry is not NA
    if(!is.na(affiliation)){
      #use the `countrycode` package to see if the country name is returned
      #countrycode_pkg_return is a country name
      countrycode_pkg_return <- countrycode::codelist$country.name.en[stringr::str_detect(affiliation, stringr::regex(countrycode::codelist$country.name.en.regex, ignore_case = TRUE))]

        #if `countrycode` package does not return any information, use OSM
        if(rlang::is_empty(countrycode_pkg_return)){
          #get the information about the location using OSM
          OSM_affiliation_returned <- tmaptools::geocode_OSM(affiliation)

          #if OSM does not return a geolocation, try to split according to commas
          if(is.null(OSM_affiliation_returned)){
            #cases to test -- row 36 Centre for Social Investigation, Nuffield College, University of Oxford, Oxford, UK

            #if the affiliation has any commas, proceed to comma splitting
            if(stringr::str_count(affiliation, ",") > 0){
              #count the number of commas present in a affiliation phrase
              num_commas <- stringr::str_count(affiliation, ",")

              #find the location of the last comma
              locations_all_commas <- stringr::str_locate_all(pattern = ",", affiliation)
              last_comma_location <- max(locations_all_commas[[1]])
              #if the phrase has a comma in the end and nothing after, get rid of the comma, update affiliation
              affiliation <- ifelse(last_comma_location == nchar(affiliation),
                                    substr(affiliation, 1, nchar(affiliation)-1),
                                    affiliation)

              #if n commmas are present, I will choose the phrase after n commas.
              last_phrase <- substr(affiliation, last_comma_location+1, nchar(affiliation))
              #get the OSM location
              OSM_last_phrase <- tmaptools::geocode_OSM(last_phrase)
              #if OSM location exists for the last phrase, get the lcoation name
              if(!is.null(OSM_last_phrase)){
                #get the location list
                last_phrase_location <- tmaptools::rev_geocode_OSM(OSM_last_phrase$coords["x"], OSM_last_phrase$coords["y"])
                #get the location code
                last_phrase_country_code <- last_phrase_location[[1]]$country_code

                #if the country_code entry is NA, put the value in
                if(is.na(data_with_affiliations$country_code[entry])){
                  data_with_affiliations$country_code[entry] <-  toupper(last_phrase_country_code)
                }else{#if the entry is not NA, do not change the entry
                  data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
                }

                #get the country name from last_phrase_location
                last_phrase_country_name <- last_phrase_location[[1]]$country
                #if country_name entry is NA, enter the value
                if(is.na(data_with_affiliations$country_name[entry])){
                  data_with_affiliations$country_name[entry] <- last_phrase_country_name
                }else{#if country_name entry is not NA, keep the same value
                  data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
                }
                next
              }else{#if after comma splitting OSM does not return anything
                #put NA in the entry if the entry was already NA
                if(is.na(data_with_affiliations$country_code[entry])){
                  data_with_affiliations$country_code[entry] <-  NA
                }else{# if the entry was not NA, put the same value
                  data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
                }
                #put NA in country_name if the entry was already NA
                if(is.na(data_with_affiliations$country_name[entry])){
                  #Put NA in country_name[entry]
                  data_with_affiliations$country_name[entry] <-  NA
                }else{#if the entry had a value, keep the value
                  data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
                }
                next
              }

            }else{# if the affiliation has no commas and OSM does not return anything
              #put NA in the entry if the entry was already NA
              if(is.na(data_with_affiliations$country_code[entry])){
                data_with_affiliations$country_code[entry] <-  NA
              }else{# if the entry was not NA, put the same value
                data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
              }
              #put NA in country_name if the entry was already NA
              if(is.na(data_with_affiliations$country_name[entry])){
                #Put NA in country_name[entry]
                data_with_affiliations$country_name[entry] <-  NA
              }else{#if the entry had a value, keep the value
                data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
              }
              next
            }

          }else{
            #if the OSM returns a geolocation, get the country code using OSM
            location_list <- tmaptools::rev_geocode_OSM(OSM_affiliation_returned$coords["x"], OSM_affiliation_returned$coords["y"])
            #Get the country code of the affiliation
            list_country_code <- location_list[[1]]$country_code
            #add this country code to the respective column
            #if the country_code entry is NA, put the value in
            if(is.na(data_with_affiliations$country_code[entry])){
              data_with_affiliations$country_code[entry] <-  toupper(list_country_code)
            }else{#if the entry is not NA, do not change the entry
              data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
            }

            #HERE put value into country_name from location_list
            list_country_name <- location_list[[1]]$country
            if(is.na(data_with_affiliations$country_name[entry])){
              data_with_affiliations$country_name[entry] <- list_country_name
            }else{#if country_name entry is not NA, keep the same value
              data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
            }
            next
          }
        }else{
          #HERE put country name from countrycode_pkg_return into the column
          #data_with_affiliations$country_name[entry] <-  countrycode_pkg_return
          if(is.na(data_with_affiliations$country_name[entry])){
            data_with_affiliations$country_name[entry] <- countrycode_pkg_return
          }else{#if country_name entry is not NA, keep the same value
            data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
          }

          #countrycode returns country names, turn it into 2 letter country codes
          countrycode_pkg_return <- toupper(countrycode::countrycode(countrycode_pkg_return, origin = 'country.name', destination = 'iso2c'))
          #put the info returned by the `countrycode` pkg in the df
          #data_with_affiliations$country_code[entry] <-  countrycode_pkg_return
          if(is.na(data_with_affiliations$country_code[entry])){
            data_with_affiliations$country_code[entry] <-  countrycode_pkg_return
          }else{#if the entry is not NA, do not change the entry
            data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
          }
          next
        }

    }else{     #if the entry is NA
      if(is.na(data_with_affiliations$country_code[entry])){
        data_with_affiliations$country_code[entry] <-  NA
      }else{# if the entry was not NA, put the same value
        data_with_affiliations$country_code[entry] <- data_with_affiliations$country_code[entry]
      }
      #put NA in country_name if the entry was already NA
      if(is.na(data_with_affiliations$country_name[entry])){
        #Put NA in country_name[entry]
        data_with_affiliations$country_name[entry] <-  NA
      }else{#if the entry had a value, keep the value
        data_with_affiliations$country_name[entry] <- data_with_affiliations$country_name[entry]
      }
      next
    }
  }#for loop


return(data_with_affiliations)
}#function
