
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
  crossref_data <- read_csv("R/crossref_results.csv")

  for(entry in 1:nrow(crossref_data)){
    #pull the affiliation entries one by one
    affiliation <- crossref_data$affiliation.name[entry]
    print(c("we are on entry", entry, affiliation))
    #if the affiliation entry is not NA
    location_data <- if(!is.na(affiliation)){
      #return the X Y coordinates of the affiliation
      data_returned <- geocode_OSM(affiliation)

      #if the location info does not exist
      if(is.null(data_returned)){
        # move to the next entry
        next
      }else{
        #get the information about the location
        location_list <- rev_geocode_OSM(data_returned$coords["x"], data_returned$coords["y"])
        #Get the country code of the affiliation
        list_country_code <- location_list[[1]]$country_code
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
