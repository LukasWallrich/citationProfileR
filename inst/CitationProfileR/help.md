### How to Access

CitationProfileR Shiny dashboard can be accessed through downloading the package along with an external hosting on an html website that will be accessible through search engines.

The link for the hosted dashboard is: http://127.0.0.1:4955

A user can launch the Shiny dashboard by first finding the app.R script in which the respective file path is: citationProfileR/inst/CitationProfileR/app.R. Once opening the file, all one needs to do is click on the run app tab at the top of the file in Rstudio.


### How to Install

You can install the development version of CitationProfileR from GitHub with:
``` r
# install.packages("devtools")
devtools::install_github("LukasWallrich/citationProfileR")
```


### Examples

These are some basic examples for every function in our package. 

First, load `CitationProfileR` R package:

```{r}
library(CitationProfileR)
```

In order to use the `first_name_check` function, a user needs to upload a csv file to their Rstudio dashboard. After the csv file has been saved locally on one's file, they can call the function successfully. We already have some example csv files in the inst folder within the test-data sub folder that a user can access.

```{r}
file_path <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
sample_data_frame <- read.csv(file_path)
first_name_check(sample_data_frame)
```

Likewise, we follow the same procedure for the `get_author_info`implementation as we did for the `first_name_check` function. The example csv files within our package will also work with this implementation.

```{r}
file_path <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
sample_data_frame <- read.csv(file_path)
get_author_info(sample_data_frame)
```

For the `guess_gender`function, a user needs to replace the name parameter with one of their own in " " along with a country code of their choice also in " ." 

```{r}
#Standardized format for any use
#guess_gender(name, countrycode)

#Example of how to call the function using a name and country of their choice. In this case, the name is Rithika and the country is the United States where the associated code is the US.
guess_gender("Rithika", "US")
```

The `parse_pdf_refs` takes in a pdf uploaded into Rstudio, and there is also an example pdf available for a user to access in order to run the function

```{r}
file_path <- system.file("test-data", "Wallrich_et_al_2020.pdf", package = "CitationProfileR")
parse_pdf_refs(file_path)
```

The `get_location()` function takes in a data frame with affiliations and outputs the country names and country codes of where the affiliations are located. The function has a default affiliations column name set to "affiliation.name", but the user can set a different column name. The `sample_data_frame` dataframe is an example data object available in our package that the user can examine the function on.

```{r}
file_path <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
sample_data_frame <- read.csv(file_path)
get_location(sample_data_frame)
```

