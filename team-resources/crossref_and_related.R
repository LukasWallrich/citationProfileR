
safe_format <- function(bibtex) {
  purrr::possibly(format, otherwise = NA)(bibtex, .bibstyle = "brief")
}

bibtex_to_apa <- function(bibtex, warn = FALSE) {
  if(!exists("cite_tmp")) cite_tmp <<- tempfile()
  fileConn<-file(cite_tmp)
  writeLines(bibtex, fileConn)
  close(fileConn)

  if (warn) {
    t <- bibtex::read.bib(cite_tmp)} else{
      t <- suppressMessages(bibtex::read.bib(cite_tmp))
  }
    out <- safe_format(t)


  if(length(out) == 0) return(NA)
  out
}


get_citation <- function(df) {

  if(nrow(df) > 1) {
    return(pmap_dfr(df, ~get_citation(tibble(...))))
  }

  if(is.null(df$doi) || is.na(df$doi)) {
    return(df)
  }

  bibtex <- cr_cn(dois = df$doi, format = "bibtex", style = "apa")

  if(!is.null(bibtex)) df$citation <-  bibtex_to_apa(bibtex)

  if(is.null(bibtex)) bibtex <- df$citation <- NA

  if(is.null(df$citation) || is.na(df$citation) || length(df$citation) == 0) {
    df$citation <- purrr::possibly(cr_cn, otherwise = NA)(dois = df$doi, format = "text", style = "apa")
  }
  df$bibtex <- bibtex
  df
}


apa_style <- tools::bibstyle("brief", sortKeys = function(refs) seq_along(refs),
                             extraInfo = function(paper) NULL,
                             .init = TRUE)

clean_text <- function(x) {
  stringr::str_squish(stringr::str_to_lower(str_replace_all(x, "<.*?>", " ") %>%
                                              str_replace_all("[&#]{1}[0-9a-z]*?;", " ") %>% str_replace_all("[[:punct:]]", " ") %>% stringi::stri_trans_general('Latin-ASCII')))
}

## Assess quality of match (compare title, first author, year) and merge data

match_crossref <- function(df, crr) {
  cr_match <- tibble()
  comp_res <- c()
  crr <- crr %>%
    mutate(across(any_of(c("created", "published.print", "published.online", "issued")),
                  ~ lubridate::year(lubridate::parse_date_time(.x, orders = c("%Y-%m-%d", "%Y", "%Y-%m"))),
                  .names = "year_{.col}"
    ))
  if ((clean_text(crr$title) == clean_text(df$title) ||
           safe_str_detect(clean_text(crr$title), clean_text(df$title)) ||
           safe_str_detect(clean_text(df$title), clean_text(crr$title))) %>%
      {!(is.na(.)) && .}) { # Treat NA (from missing Crossref title)
    comp_res <- c(comp_res, "title: Y")
  } else {
    comp_res <- c(comp_res, "title: N")
  }

  years <- as.numeric(crr[safe_str_detect(names(crr), "year_")])

  # Allow for +/- 1 year, nearly always just difference between online first and print
  if (df$year %in% c(years, years + 1, years - 1)) {
    comp_res <- c(comp_res, "year: Y")
  } else {
    comp_res <- c(comp_res, "year: N")
  }

  if (("author" %in% names(crr) &&
      (safe_str_detect(clean_text(df$first_author), clean_text(crr$author[[1]]$family[1]))||
       safe_str_detect(clean_text(df$authors_1_name), clean_text(crr$author[[1]]$family[1])))) %>%
      {!(is.na(.)) && .}) { # Treat NA (from missing Crossref family field - e.g., for institutional authors)
    comp_res <- c(comp_res, "1st: Y")
  } else {
    comp_res <- c(comp_res, "1st: N")
  }
  df$doi <- crr$doi
  df$doi_url <- crr$url
  df$abstract <- crr[["abstract"]]
  if(is.null(df[["abstract"]])) df$abstract <- NA_character_
  df$author <- crr[["author"]]
  df$journal <- crr[["container.title"]]
  df$type <- crr$type
  df$cr_match <- str_flatten(comp_res, " | ")
  df$cr_title <- crr$title
  df$cr_year <- do.call(pmin, c(crr[ safe_str_detect(names(crr), "year_")], na.rm=TRUE))

  df

}

## Search crossref for reference matching Scholar hit
### trying both title, author year and summary queries

extract_crossref <- function(df) {

  crr <- cr_works(flq = c(query.bibliographic =
                            paste(df$title, df$summary)), limit = 1)$data

  if (!is.null(crr) && "title" %in% colnames(crr)) {


  df_merged <- match_crossref(df, crr)

  if(length(str_extract_all(df_merged$cr_match, "Y")[[1]])==3 ||
     is.na(df$authors_1_name)) return(df_merged)

  }


  crr2 <- cr_works(flq = c(query.bibliographic =
                             paste(df$title, df$authors_1_name, df$year)), limit = 1)$data

  if (is.null(crr2) || !"title" %in% colnames(crr2) || crr2$doi == crr$doi) {
    if (exists("df_merged") && length(str_extract_all(df_merged$cr_match, "Y")[[1]]) == 2) {
      return(df_merged)
    } else {
      return(df)
    }
  }

  df_merged2 <- match_crossref(df, crr2)

  if(length(str_extract_all(df_merged2$cr_match, "Y")[[1]])==3) {
    return(df_merged2)
  }

  if (exists("df_merged") && length(str_extract_all(df_merged$cr_match, "Y")[[1]]) == 2) {
    if (length(str_extract_all(df_merged2$cr_match, "Y")[[1]]) == 2) {
      df_merged$notes <- paste(df_merged$notes, "| alt DOI:", df_merged2$doi_url)
    }
    return(df_merged)
  } else if  (length(str_extract_all(df_merged2$cr_match, "Y")[[1]]) == 2) {
    return(df_merged2)
  } else {
    df
  }
}

collapse_author_df <- function(df) {
  map_chr(df, \(x) {
    if (is.null(x)) return(NA_character_)
    if (!"given" %in% names(x)) x$given <- ""
    if ("name" %in% names(x) & !"family" %in% names(x)) x$family <- x$name
    x %>% mutate(name = paste(str_to_title(family), str_to_title(given))) %>%
      pull(name) %>% glue::glue_collapse(sep = ", ", last = " & ")
  })
}

# Crossref extraction sometimes times out on individual requests - should not lead to failure of 3000+
extract_crossref_try <- purrr::possibly(extract_crossref, tibble(cr_match = "error"))
