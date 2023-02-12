
# CitationProfileR

<!-- badges: start -->
<!-- badges: end -->


<red> <big> ** Beware: Work in progress - not yet functional!!** </big> </red>

CitationProfileR is an R package and web app that allows users to upload a PDF or citation file and to get statistics on the gender, ethnic (?) and geographic distribution of the citations they include. These will be provided for download, and summarised and visualised in a form that is publication-ready. The data is derived from various web services (e.g., Crossref, genderize.io, ORCID, Wikidata, ...) as well as data extraction from the uploaded files.

# Background

Academia is rife with inequalities. Across subjects, women and ethnic minorities are under-represented in higher ranks, and universities in the Global North (and particularly the Anglophone world) dominate. Given that the number of citations strongly shapes careers, inequities in citation patterns matter - yet they are currently hard to detect, for authors, reviewers and meta-science researchers and journal editors. Citation Diversity Statements are gaining traction (Zurn et al., 2020), yet the only tool to create them to date (cleanBib) lacks some features and is only accessible to users comfortable with Python code.


## Installation

You can install the development version of CitationProfileR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LukasWallrich/citationProfileR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CitationProfileR)
## basic example code
```

