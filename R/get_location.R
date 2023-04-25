
#' get_location
#'
#' @return
#' @export
#'
#' @examples

# load library(tmaptools)

#affiliation name must be string
get_location <- function(df_with_affiliation){

  #load in the sample result from the crossref_api()
  crossref_data <- read_csv("R/crossref_results.csv") %>%
    select(-`...1`)
  #add an empty column country_code where the for loop will add values
  crossref_data <- cbind(crossref_data, country_code=NA)

  for(entry in 1:nrow(crossref_data)){
    #pull the specific entries one by one
    author <- crossref_data$OG_Author[entry]
    title <- crossref_data$OG_Title[entry]
    date <- crossref_data$Date[entry]
    year <- crossref_data$Year[entry]
    og_doi <- crossref_data$OG_dio[entry]
    index <- crossref_data$index[entry]
    title2 <- crossref_data$title[entry]
    given <- crossref_data$given[entry]
    family <- crossref_data$family[entry]
    sequence <- crossref_data$sequence[entry]
    affiliation.name <- crossref_data$affiliation.name[entry]
    affiliation1.name <- crossref_data$affiliation1.name[entry]
    affiliation2.name <- crossref_data$affiliation2.name[entry]
    ORCID <- crossref_data$ORCID[entry]
    authenticated.orcid <- crossref_data$authenticated.orcid[entry]
    name <- crossref_data$name[entry]
    country_code <- crossref_data$country_code[entry]

    print(c("we are on entry", entry, affiliation))
    #if the affiliation entry is not NA
    location_data <- if(!is.na(affiliation)){
      #return the X Y coordinates of the affiliation
      data_returned <- geocode_OSM(affiliation)

      #if the location info does not exist
      if(is.null(data_returned) | nrow(data_returned)==0){
        #do not add anything to that location in df
        crossref_data[nrow(crossref_data) + 1,] = c(author, title, date, year, og_doi,
                                                    index, title2, given, family, sequence,
                                                    affiliation.name, affiliation1.name,
                                                    affiliation2.name, ORCID, authenticated.orcid,
                                                    name, country_code)

        # move to the next entry
        next
      }else{
        #get the information about the location
        location_list <- rev_geocode_OSM(data_returned$coords["x"], data_returned$coords["y"])
        #Get the country code of the affiliation
        list_country_code <- location_list[[1]]$country_code

        #add this country code to the respective column

      }

    }
    #if the entry is NA
    else if(is.na(affiliation)){
      NULL
    }else{NULL}

  }

#TO DO:
#1. need to save the entries I get from the for loop
#2. code it to not stop when there's an error in download.file()

}
loc <- geocode_OSM("University of Osnabrück, Germany")
location_list <- rev_geocode_OSM(loc$coords["x"], loc$coords["y"])



location_list[[1]]$country_code
