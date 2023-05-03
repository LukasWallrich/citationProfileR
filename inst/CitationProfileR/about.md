### About CitationProfileR

[CitationProfileR](https://github.com/LukasWallrich/citationProfileR) is an R package and Shiny web app that allows users to upload a PDF or citation file and to get statistics on the gender and geographic distribution of the citations they include. These visualizations will be provided for download, and summarized and visualized in a form that is publication-ready.
The package uses data from various web service, like [Crossref API](https://www.crossref.org/), [GROBID API](https://grobid.readthedocs.io/en/latest/), [Gender-API](https://gender-api.com/en/), and [Open Street Map](https://www.openstreetmap.org/#map=4/38.01/-95.84), as well as the data extracted from the uploaded files.


### Dependencies

There are no special dependencies. All one needs is Rstudio downloaded and installed in the latest version.


### Functions/Datasets Included

Our package includes the following functions, which allows the user to extract information from all authors included in the paper uploaded to our app along with returning the gender prediction per every name as well. Also, they can retrieve a diversity statement and see a bar plot with the count per gender in the web app as well.

- `first_name_check` takes in data frame of extracted citations returned from GROBID API and returns first name of every author

- `get_author_info` takes in data frame that contains every cited author's name, paper title, and date published and returns first and last name of all cited authors from Crossref API

- `guess_gender` takes in a cited author's name, geographic location based on country code, as well as if the user wants to use the cache feature which remembers previous predictions based on a name used in earlier iterations in order to return a data frame containing the author's name, location, and associated gender prediction and accuracy measure from GenderAPI

- `parse_pdf_refs` takes in a pdf uploaded from a user containing a works cited page and returns the isolated references of every cited author and their respective work from GROBID

- `get_location` takes in a data frame of all cited author's affiliations and uses Crossref API in order to return a data frame with all associated countries and country codes in the ISO 3166 standardized format for every given author
