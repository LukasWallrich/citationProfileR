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

  f_bib <- paste0("/Users/likamikhelashvili/Documents/Classes@Smith/SDS410/citationProfileR/extractedCitations/Test", ".bib")
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
file <- "/Users/likamikhelashvili/Documents/Classes@Smith/thesis/Text/doleac_2017.pdf"
output <- parse_pdf_refs(file)

f_bib <- paste0("/Users/adrianabeltran/Downloads/CitationTest", ".bib"). #add .bib extension to file name (do not need file_path_blah since we are asking for it just for rn bc we do not have a shiny upload)
bib <- parse_pdf_refs(f) #read the PDF
writeLines(bib, f_bib) # write what you find in bib onto f_bib file


f_bib <- paste0("~/citationProfileR/extractedCitations/", bib_name , ".bib")


# translating this
# def parse_pdf_refs(file):
#   import requests
# GROBID_URL = 'https://cloud.science-miner.com/grobid'
# url = '%s/api/processReferences' % GROBID_URL
# xml = requests.post(url, files={'input': open(file, 'rb')}, headers = {"Accept": "application/x-bibtex"}, data = {"consolidateCitations": "1"})
# return xml.text
