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
#' file_path <- system.file("test-data", "Wallrich_et_al_2020.pdf", package = "CitationProfileR")
#' parse_pdf_refs(file_path)
#' \dontrun{
#'parse_pdf_refs(file_path, "file.bib")
#'parse_pdf_refs(file_path, "file.bib", "https://kermitt2-grobid.hf.space")
#'}

# original grobid URL gave 'temporary' 302 redirect to url below - so might need to revert to 'https://cloud.science-miner.com/grobid'
parse_pdf_refs <- function(file, save_to_file = NULL, GROBID_URL = "https://kermitt2-grobid.hf.space") {

  #creating API url
  api <- '/api/processReferences'
  api_url <- paste0(GROBID_URL, api)

  #input is required, API call to GROBID process Reference(https://grobid.readthedocs.io/en/latest/Grobid-service/)
  GROBID <- httr::POST(api_url, httr::accept("application/x-bibtex"),  body = list(input = httr::upload_file(file)), encode = "multipart")#, verbose())

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

  res <- bib2df::bib2df(save_to_file) |>
    bbr::collapse_to_string(tidyselect::where(is.list))

  res <- res |>
    dplyr::mutate(index = dplyr::row_number())

  return(res)
}

hahaha <- parse_pdf_refs("/Users/adrianabeltran/Downloads/Wallrich_et al_2020.pdf")
