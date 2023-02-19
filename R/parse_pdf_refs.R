library(tidyverse)
library(httr)
#giving 200 but content is returned in hexidecimal
parse_pdf_refs <- function(file) {
  #creating API url
  GROBID_URL <- 'https://cloud.science-miner.com/grobid'
  api <- '/api/processReferences'
  api_url <- paste0(GROBID_URL, api)

  #input is required, API call to GROBID process Reference(https://grobid.readthedocs.io/en/latest/Grobid-service/)
  GROBID <- httr::POST(api_url, accept("application/x-bibtex"),  body = list(input = upload_file(file)), encode = "multipart", verbose())

  # translate return from Raw to Characters
  output <- rawToChar(GROBID$content)

  f_bib <- paste0("/Users/adrianabeltran/Desktop/Academics/Spring23/Capstone/citationProfileR/extractedCitations", ".bib")
  writeLines(output, f_bib)

  #if no references are fine return that
  if (length(output) == 0 || (length(output) == 1 && output[1] == "")) {
    message("No references found for output" )
    return(data.frame())
  }

  res <- bib2df::bib2df(f_bib)
  return(res)
}

#testers to run just the function on top
#file <- "/Users/adrianabeltran/Downloads/brewerPaper.pdf"
file <- "/Users/adrianabeltran/Downloads/practicing-moderation-community-moderation-as-reflective-practice (1).pdf"
output <- parse_pdf_refs(file)

