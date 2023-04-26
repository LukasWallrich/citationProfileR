# Copyright (c) 2020 Metrum Research Group under the MIT licence
# Retrieved from https://github.com/metrumresearchgroup/bbr/blob/main/LICENSE.md
collapse_to_string <- function(.data, ..., .sep = ", ") {
  checkmate::assert_scalar(.sep)

  cols <- tidyselect::eval_select(rlang::expr(c(...)), .data)

  # subset to only list cols and warn if passed any columns that are not lists
  valid_cols <- purrr::map_lgl(cols, ~ inherits(.data[[.x]], "list"))
  if (any(!valid_cols)) {
    bad_cols <- names(valid_cols)[!valid_cols]
    warning(glue("collapse_to_string() only works on list columns. The following columns are not lists and will be ignored: {paste(bad_cols, collapse = ', ')}"))
  }
  cols <- cols[valid_cols]

  # collapse together any lists of vectors
  .data %>%
    purrr::modify_at(.at = cols, .f = function(x) {
      purrr::map_chr(x, .f = function(.vec) {
        if (inherits(.vec, c("character", "numeric", "logical"))) {
          .vec <- paste0(.vec, collapse = .sep)
        } else if (is.null(.vec)) {
          .vec <- NA_character_
        } else {
          .vec <- paste(
            capture.output(dput(.vec)),
            collapse = ""
          )
        }
        return(.vec)
      })
    })
}




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
    collapse_to_string(tidyselect::where(is.list))

  res <- res |>
    dplyr::mutate(index = dplyr::row_number())

  return(res)
}
