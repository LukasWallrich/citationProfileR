# CitationProfileR

<img src="CitationProfileR_logo.png" align="right" style="height:150px; padding: 10px;" />

## About

[CitationProfileR](https://github.com/LukasWallrich/citationProfileR) is an R package and Shiny web app that allows users to upload a PDF or citation file and to get statistics on the gender and geographic distribution of the citations they include. These visualizations will be provided for download, and summarized and visualized in a form that is publication-ready.
The package uses data from various web service, like [Crossref API](https://www.crossref.org/), [GROBID API](https://grobid.readthedocs.io/en/latest/), [Gender-API](https://gender-api.com/en/), and [Open Street Map](https://www.openstreetmap.org/#map=4/38.01/-95.84), as well as the data extracted from the uploaded files.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START -->
| Contributions | Name |
| ----: | :---- |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Adriana Beltran Andrade](https://orcid.org/0009-0000-3015-9694) |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Lika Mikhelashvili](https://orcid.org/0009-0009-0295-5891) |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Mackie Zhou](https://orcid.org/0009-0004-8416-2660) |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Rithika Devarakonda](https://orcid.org/0009-0006-7869-9204) |
| [🔢](# "Content") [🧑‍🏫](# "Mentoring") | [Lukas Wallrich](https://orcid.org/0000-0003-2121-5177) |


<!-- ALL-CONTRIBUTORS-LIST:END -->


## Definitions

*Citations* - A reference to a source of information in a academic paper. Citations typically include information such as author names, article title, DOI, date of publication, etc.

*Diversity Statement* - A diversity statement of an academic journal is a statement that acknowledges the gender and/or racial imbalance within a scientific field. The diversity statement motivates researchers to pay particular attention to the gender and racial breakdown of the authors cited in their work. It recognizes existing biases and aims for greater inclusivity in the field.


## How to Access

CitationProfileR Shiny dashboard can be accessed through downloading the package along with an external hosting on an html website that will be accessible through search engines.

The link for the hosted dashboard is: http://127.0.0.1:4955

A user can launch the Shiny dashboard by first finding the app.R script in which the respective file path is: citationProfileR/inst/CitationProfileR/app.R. Once opening the file, all one needs to do is click on the run app tab at the top of the file in Rstudio.

## Dependencies

There are no special dependencies. All one needs is Rstudio downloaded and installed in the latest version.

## How to Install

You can install the development version of CitationProfileR from GitHub with:
``` r
# install.packages("devtools")
devtools::install_github("LukasWallrich/citationProfileR")
```

## Functions/Datasets Included

Our package includes the following functions, which allows the user to extract information from all authors included in the paper uploaded to our app along with returning the gender prediction per every name as well. Also, they can retrieve a diversity statement and see a bar plot with the count per gender in the web app as well.

- `first_name_check` takes in data frame of extracted citations returned from GROBID API and returns first name of every author

- `get_author_info` takes in data frame that contains every cited author's name, paper title, and date published and returns first and last name of all cited authors from Crossref API

- `guess_gender` takes in a cited author's name, geographic location based on country code, as well as if the user wants to use the cache feature which remembers previous predictions based on a name used in earlier iterations in order to return a data frame containing the author's name, location, and associated gender prediction and accuracy measure from GenderAPI

- `parse_pdf_refs` takes in a pdf uploaded from a user containing a works cited page and returns the isolated references of every cited author and their respective work from GROBID

- `get_location` takes in a data frame of all cited author's affiliations and uses Crossref API in order to return a data frame with all associated countries and country codes in the ISO 3166 standardized format for every given author

## Examples

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

The `get_location` function takes in a csv file uploaded to Rstudio, and this function can also run using the example csv file available in our package in order to return a data frame with all the country codes associated with every cited author.

```{r}
file_path <- system.file("test-data", "test_citations_table2.csv", package = "CitationProfileR")
sample_data_frame <- read.csv(file_path)
get_location(sample_data_frame)
```

## Data Sources

CitationProfileR source of data is any academic article in a pdf version that is uploaded to the Shiny UI by users of the package. After the pdf is uploaded, the `parse_pdf_refs()` function will parse the contents of the file and output a data frame with all the cited authors along with their affiliations and DOI if applicable. Then, the `guess_gender()` function takes in this data frame and outputs a new one including the predicted gender and probability of accuracy of every given name using the Gender-API.

## Data Collection and Update Process

The data does not need to be either manually or automatically updated as the user inputs the academic article on their own. 

## Repo Architecture

This repository follows the standard R package structure. The [R folder](https://github.com/LukasWallrich/citationProfileR/tree/main/R) contains the code to the functions available in CitationProfileR separated into different R scripts. The code for the Shiny UI dashboard is in the [inst folder](https://github.com/LukasWallrich/citationProfileR/tree/main/inst/CitationProfileR) in the repository. A user can access the final dashboard by using the link provided above or through accessing the cloned version of the repository contents on their local device.

## License

[MIT License](https://www.tldrlegal.com/license/mit-license?ref=fossa.com#fulltext). Copyright (c) 2023 CitationProfileR authors.

## How to Provide Feedback

Questions, bug reports, and feature requests can be submitted to this repo's [issue queue](https://github.com/LukasWallrich/citationProfileR).


## Have Questions?

Contact us at lmikhelashvili@smith.edu.

## TODO

We need to address whether a user needs Rstudio or if they can run our Shiny app and the package with just R. Also, we are unsure of what version of R one needs for our package so we don't want to make any assumptions at this point.
