#' get_location
#'
#' @param citation_data Data frame containing affiliations. If using Shiny App, the data will come from `crossref_api()` function
#' @param affiliation_col_name String name of the affiliations column. The default col name is "affiliation.name".
#'
#' @return Dataframe with country names and country codes based on the inputted affiliation
#' @export
#'
#' @examples
#' get_location(sample_data_frame)

get_location <- function(citation_data, affiliation_col_name = "affiliation.name"){

  #If country_code col does not exist, add it
  if(!"country_code" %in% colnames(citation_data)){
    #add an empty column country_code
    citation_data <- cbind(citation_data, country_code=NA)
  }
  #If country_name col does not exist, add it
  if(!"country_name"%in% colnames(citation_data)){
    #add an empty column country_name
    citation_data <- cbind(citation_data, country_name=NA)
  }


  #for loop
  for(entry in seq_len(nrow(citation_data))){

    Sys.sleep(.5)

    #pull the specific affiliation one by one
    affiliation <- citation_data[affiliation_col_name][entry,]

    #if country_code is not NA
    if(!is.na(citation_data$country_code[entry])){
      #check if country_name is NA
      #if country_name is not NA, skip to the next entry
      if(!is.na(citation_data$country_name[entry])){
        next
      }else{#if country name is NA
        entry_country_code <- citation_data$country_code[entry]
        #convert country_code to country_name
        entry_country_name <- countrycode::countrycode(entry_country_code, origin = 'iso2c', destination = 'country.name')
        #enter the country_name
        citation_data$country_name[entry] <-  entry_country_name
      }
    }else{# if the country_col is NA

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
                  #put the country code value
                  citation_data$country_code[entry] <-  toupper(last_phrase_country_code)


                  #get the country name from last_phrase_location
                  #last_phrase_country_name <- last_phrase_location[[1]]$country
                  last_phrase_country_name <- countrycode::countrycode(last_phrase_country_code, origin = 'iso2c', destination = 'country.name')

                  #if country_name entry is NA, enter the value
                  if(is.na(citation_data$country_name[entry])){
                    citation_data$country_name[entry] <- last_phrase_country_name
                  }else{#if country_name entry is not NA, keep the same value
                    citation_data$country_name[entry] <- citation_data$country_name[entry]
                  }
                  next
                }else{#if after comma splitting OSM does not return anything
                  #put NA in country_name if the entry was already NA
                  if(is.na(citation_data$country_name[entry])){
                    #Put NA in country_name[entry]
                    citation_data$country_name[entry] <-  NA
                  }else{#if the entry had a value, keep the value
                    citation_data$country_name[entry] <- citation_data$country_name[entry]
                  }
                  #put NA in country_code
                  citation_data$country_code[entry] <-  NA
                  next
                }

              }else{# if the affiliation has no commas and OSM does not return anything
                #put NA in country_name if the entry was already NA
                if(is.na(citation_data$country_name[entry])){
                  #Put NA in country_name[entry]
                  citation_data$country_name[entry] <-  NA
                }else{#if the entry had a value, keep the value
                  citation_data$country_name[entry] <- citation_data$country_name[entry]
                }
                #put NA in the country_code entry
                citation_data$country_code[entry] <-  NA
                next
              }

            }else{
              #if the OSM returns a geolocation, get the country code using OSM
              location_list <- tmaptools::rev_geocode_OSM(OSM_affiliation_returned$coords["x"], OSM_affiliation_returned$coords["y"])
              #Get the country code of the affiliation
              list_country_code <- location_list[[1]]$country_code
              #add this country code to the respective column
              citation_data$country_code[entry] <-  toupper(list_country_code)

              #HERE put value into country_name from location_list
              list_country_name <- countrycode::countrycode(list_country_code, origin = 'iso2c', destination = 'country.name')

              #list_country_name <- location_list[[1]]$country
              if(is.na(citation_data$country_name[entry])){
                citation_data$country_name[entry] <- list_country_name
              }else{#if country_name entry is not NA, keep the same value
                citation_data$country_name[entry] <- citation_data$country_name[entry]
              }
              next
            }
          }else{
            #HERE put country name from countrycode_pkg_return into the column
            #citation_data$country_name[entry] <-  countrycode_pkg_return
            if(is.na(citation_data$country_name[entry])){
              citation_data$country_name[entry] <- countrycode_pkg_return
            }else{#if country_name entry is not NA, keep the same value
              citation_data$country_name[entry] <- citation_data$country_name[entry]
            }

            #countrycode returns country names, turn it into 2 letter country codes
            countrycode_pkg_return <- toupper(countrycode::countrycode(countrycode_pkg_return, origin = 'country.name', destination = 'iso2c'))
            #put the info returned by the `countrycode` pkg in the df
            citation_data$country_code[entry] <-  countrycode_pkg_return
            next
          }

      }else{     #if the affiliation entry is NA
        next
      }

    }#else country_code NA
    next
  }#for loop


return(citation_data)
}#function
