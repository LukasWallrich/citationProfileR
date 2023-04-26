
## @CitationProfileR team: Remove the showing_progress wrapper, not needed here


# GROBID is used to extract references from PDFs - usually, this should be done with a local installation,
# but that is difficult without a Crossref subscription - so we could use the web service which appears
# acceptable for the kind of load expected here.
#(https://github.com/kermitt2/grobid_client_python/issues/54#issuecomment-1272509755).
# This is a bit slow ... approx. 1 min per PDF.

# Note that this does not recognise text in PDF scans - these need to be OCR-ed before.
# ## Not sure how easy/possible it is to include the OCR step on shinyapps.io -
# can skip this for now, but then provide warning with instructions for OCR if grobid returns nothing
library(dplyr)
library(tidyverse)


pacman::p_load(sys)

fs <- list.files("./past_review_PDFs", pattern = ".pdf", full.names = TRUE)

# OCR them (this also compresses the ones already containing text, if that takes
# too long, could also first try GROBID and only OCR failures
#says remove
#showing_progress(fs, walk, \(x) normalizePath(x) %>% {system(glue::glue('ocrmypdf --skip-text "{.}" "{.}"'))})

# Request from GROBID
refs <- map(fs, \(f) {
  f_bib <- paste0(tools::file_path_sans_ext(f), ".bib"). #add .bib extension to file name (do not need file_path_blah since we are asking for it just for rn bc we do not have a shiny upload)
  bib <- parse_pdf_refs(f) #read the PDF
  writeLines(bib, f_bib) # write what you find in bib onto f_bib file
  #if no references are fine return that
  if (length(bib) == 0 || (length(bib) == 1 && bib[1] == "")) {
    message("No references found for ", f)
    return(data.frame())
  }


  res <- bib2df::bib2df(f_bib) %>%
    collapse_to_string(where(is.list))
  res$database <- paste0("references_", basename(f))
  res
}) %>% set_names(fs)

refs <- bind_rows(refs) %>% rename_with(tolower)

## PYTHON function - needs to be in a .py file and then be sourced with reticulate::source_python()
# or translate into R - no particular need to keep this in Python, I just found an easier example there

library(httr)

#giving 200
parse_pdf_refs <- function(file) {
  GROBID_URL <- 'https://cloud.science-miner.com/grobid'
  api <- '/api/processReferences'
  full_url <- paste0(GROBID_URL, url)
  #filePATH <- "/Users/adrianabeltran/Downloads/brewerPaper.pdf"
  filePATH <- "/Users/adrianabeltran/Downloads/practicing-moderation-community-moderation-as-reflective-practice (1).pdf"
  #input is required
  GROBID3 <- httr::POST(full, accept("application/x-bibtex"),  body = list(input = upload_file(filePATH)), encode = "multipart", verbose())
}


def parse_pdf_refs(file):
  import requests
GROBID_URL = 'https://cloud.science-miner.com/grobid'
url = '%s/api/processReferences' % GROBID_URL
xml = requests.post(url, files={'input': open(file, 'rb')}, headers = {"Accept": "application/x-bibtex"}, data = {"consolidateCitations": "1"})
return xml.text


