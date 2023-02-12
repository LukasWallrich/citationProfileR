
## @CitationProfileR team: Remove the showing_progress wrapper, not needed here


# GROBID is used to extract references from PDFs - usually, this should be done with a local installation,
# but that is difficult without a Crossref subscription - so we could use the web service which appears
# acceptable for the kind of load expected here.
#(https://github.com/kermitt2/grobid_client_python/issues/54#issuecomment-1272509755).
# This is a bit slow ... approx. 1 min per PDF.

# Note that this does not recognise text in PDF scans - these need to be OCR-ed before.
# ## Not sure how easy/possible it is to include the OCR step on shinyapps.io -
# can skip this for now, but then provide warning with instructions for OCR if grobid returns nothing

pacman::p_load(sys)

fs <- list.files("./past_review_PDFs", pattern = ".pdf", full.names = TRUE)

# OCR them (this also compresses the ones already containing text, if that takes
# too long, could also first try GROBID and only OCR failures
showing_progress(fs, walk, \(x) normalizePath(x) %>% {system(glue::glue('ocrmypdf --skip-text "{.}" "{.}"'))})

# Request from GROBID
refs <- map(fs, \(f) {
  f_bib <- paste0(tools::file_path_sans_ext(f), ".bib")
  bib <- parse_pdf_refs(f)
  writeLines(bib, f_bib)
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

def parse_pdf_refs(file):
  import requests
GROBID_URL = 'https://cloud.science-miner.com/grobid'
url = '%s/api/processReferences' % GROBID_URL
xml = requests.post(url, files={'input': open(file, 'rb')}, headers = {"Accept": "application/x-bibtex"}, data = {"consolidateCitations": "1"})
return xml.text


