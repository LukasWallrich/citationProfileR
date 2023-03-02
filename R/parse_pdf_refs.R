#' Parse PDF references
#'
#' Parse references in a PDF file using GROBID
#'
#' @param file path to the PDF file
#' @param save_to_file path to the output file
#' @param GROBID_URL GROBID URL
#' @return dataframe with references
#' @export
#' @examples
#' parse_pdf_refs("file.pdf")
#' parse_pdf_refs("file.pdf", "file.bib")
#' parse_pdf_refs("file.pdf", "file.bib", "https://kermitt2-grobid.hf.space")

# original grobid URL gave 'temporary' 302 redirect to url below - so might need to revert to 'https://cloud.science-miner.com/grobid'
parse_pdf_refs <- function(file, save_to_file = NULL, GROBID_URL = "https://kermitt2-grobid.hf.space") {

  #creating API url
  api <- '/api/processReferences'
  api_url <- paste0(GROBID_URL, api)

  #input is required, API call to GROBID process Reference(https://grobid.readthedocs.io/en/latest/Grobid-service/)
  GROBID <- httr::POST(api_url, accept("application/x-bibtex"),  body = list(input = httr::upload_file(file)), encode = "multipart")#, verbose())

  # translate return from Raw to Characters
  output <- rawToChar(GROBID$content)

  #use temporary file if user does not want to save info
  if (is.null(save_to_file)) {
    save_to_file <- tempfile()
  }

  writeLines(output, save_to_file)

  #if no references are found return message
  if (length(output) == 0 || (length(output) == 1 && output[1] == "")) {
    message("No references found for output" )
    return(data.frame())
  }

  res <- bib2df::bib2df(save_to_file) %>%
    bbr::collapse_to_string(where(is.list))
  return(res)
}
