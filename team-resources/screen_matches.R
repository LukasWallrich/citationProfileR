library(magrittr)

screen_matches <- function(
  df, cols = names(df), suffixes, id = "id"
){

  input_data <- list(
    raw = NULL,
    wip = NULL
  )

  #### PROCESS DATA

    # make sure added data has a unique column called 'id'
  if (! id %in% names(df)) df[id] <- 1:nrow(df)
  if (! "result" %in% names(df))
  {df$result <- NA} else {
    if(all(!is.na(df$result))) stop("Nothing to be done here - all entries have a result")
      message("Only entries where result is NA will be displayed")
    }
  if (length(df[id]) != length(unique(df[id]))) stop("Ensure that id is a unique identifier in df.")
  if (length(unique(suffixes)) != 2) stop("Only two sources - i.e. suffixes - can be considered.")

  input_data$raw <- df

  if (!any(stringr::str_detect(cols, paste0("_", suffixes, collapse = "|"))))
    cols = c(id, as.vector(outer(cols, suffixes, paste, sep="_")))

    # make sure added data has a unique column called 'id'
    names(df) <- names(df) %>% stringr::str_replace_all(paste0("___", suffixes) %>% purrr::set_names(paste0("_", suffixes)))
    cols <- cols %>% stringr::str_replace_all(paste0("___", suffixes) %>% set_names(paste0("_", suffixes)))
    relevant_cols <- c(id, "result", cols %>% stringr::str_subset(paste0(paste0("___", suffixes), collapse = "|")))

    if(length(setdiff(cols, relevant_cols))>0)
      message("The following colums are ignored: ",
              paste(setdiff(cols, relevant_cols), sep = ", "),
              ". Only colums ending with the suffixes are considered.")

    df <- df %>% dplyr::select(dplyr::any_of(relevant_cols), result) %>%
      dplyr::filter(is.na(result)) %>%
      mutate(across(.fns = as.character)) %>%
       tidyr::pivot_longer(c(-id, -result)) %>%
       tidyr::separate(name, c("var", "suffix"), sep = "___") %>%
       tidyr::pivot_wider(names_from = var, values_from = value)

      input_data$wip <- df

    #### CREATE APP

    # create ui
    ui <- screen_matches_ui(suffixes)

      # start server
  server <- function(input, output, session){

    # reactive values
    data <- reactiveValues(
      raw = input_data$raw,
      wip = input_data$wip,
      done = input_data$wip[0,]
    )

    progress <- reactiveValues(
      entry = input_data$wip[[id]][1],
      done = NULL
    )

        # action buttons
    output$selector_bar <- renderUI({
      text_out <- HTML(
        paste0("Dataset with ",
               nrow(data$raw),
               " entries  |  ",
               nrow(data$wip)/2,
               " duplicates remaining. "
        )
      )
      div(
        div(class = "container",
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  flex-grow: 1;",
              renderText({text_out})
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 20px",
              renderText(" ")
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_skip",
                label = HTML("Skip<br><i>[space]</i>"),
                width = "100px",
                style = "background-color: #darkgrey;height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_match",
                label = HTML("Match<br><i>[1]</i>"),
                width = "100px",
                style = "background-color: #006400;height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_no_match_delete",
                label = HTML(glue::glue("No match -<br>delete {suffixes[2]} <i>[2]</i>")),
                width = "100px",
                style = "background-color: #A52A2A;height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_no_match_keep",
                label = HTML(glue::glue("No match -<br>keep both <i>[3]</i>")),
                width = "100px",
                style = "background-color: #8B008B; height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "undo",
                label = "Undo",
                icon = icon("backspace"),
                width = "100px",
                style = "background-color: #darkgrey; height: 45px; padding:1px;"
              )
            )
        )
      )
    }
    )

    # summaries
    output$hit <- renderTable({
      shiny::validate(
        need(nrow(data$wip) > 0, "")
      )
      format_overviews(
        x = data$wip %>%
          dplyr::filter(!!as.symbol(id) == progress$entry) %>%
          dplyr::select(-!!as.symbol(id), -result)
      )},
      width = NULL,
      striped = TRUE,
      sanitize.text.function=function(x){x},
      type = "html"
    )

        # Move through results
    show_next <- function() {
      data$done <- bind_rows(data$done, data$wip[data$wip[[id]] == progress$entry,])
      progress$done <- c(progress$entry, progress$done)
      data$wip <- data$wip[data$wip[[id]] != progress$entry,]
        if(nrow(data$wip) == 0){
          progress$entry <- NULL
          showModal(modalDialog(
            title = "Screening Complete - well done!",
            if (sum(is.na(isolate(data$raw$result)))>0) {glue::glue("Skipped {sum(is.na(isolate(data$raw$result)))}.
                                                                    You will need to relaunch the app to deal with them.")
              } else {""},
            "You can close the app now. Data will be returned to the workspace. If you have already assigned it to an object, it will be saved there. Otherwise, you have to save it with df <- .Last.value"))

        }else{
          progress$entry <- data$wip[[id]][1]
        }
    }

    # respond when buttons are triggered
    observeEvent(input$selected_skip, {
      data$raw$result[data$raw[[id]] == progress$entry] <- NA
      show_next()
    })

    observeEvent(input$selected_match, {
      data$raw$result[data$raw[[id]] == progress$entry] <- "match"
      show_next()
    })


    observeEvent(input$selected_no_match_delete, {
      data$raw$result[data$raw[[id]] == progress$entry] <- "no_match_delete"
      show_next()
    })

    observeEvent(input$selected_no_match_keep, {
      data$raw$result[data$raw[[id]] == progress$entry] <- "no_match_keep"
      show_next()
    })

    observeEvent(input$undo, {
      if(nrow(data$done)==0) {
        warning("Nothing to undo")
        return(NULL)}
      progress$entry <- progress$done[1]
      progress$done <- progress$done[-1]
      data$wip <- bind_rows(data$done[data$done[[id]] == progress$entry,], data$wip)
      data$done <- data$done[data$done[[id]] != progress$entry,]
    })

    observeEvent(input$keys, {
      switch (input$keys,
              "1" = click("selected_match"),
              "2" = click("selected_no_match_delete"),
              "3" = click("selected_no_match_keep"),
              "space" = click("selected_skip"),
              "backspace" = click("undo")
      )
    })



    onStop(function() {
      message("Returning results")
      stopApp(returnValue = isolate(data$raw))
    })

  } # end server



  app <- shinyApp(ui, server)
  runApp(app)

}

screen_matches_ui <- function(suffixes){

  # build user interface
  fluidPage(
    useKeys(),
    useShinyjs(),
    keysInput("keys", c("1", "2", "3", "space", "backspace")),
    tags$head(tags$style(HTML(glue::glue('

    .container {display: flex; width: 100%}

    #hit table {
                               width: 100%;
                             }
                             #hit td:nth-child(2) {
                               width: 40%;
                             }
                             #hit td:nth-child(3) {
                               width: 40%;
                             }', .open = "{{", .close = "}}")))),
    theme = shinythemes::shinytheme("cosmo"),
    titlePanel("mini-revtools | Screen matches"),

    fluidRow(
      uiOutput("selector_bar")
    ),
    fluidRow(
      column(
        width = 12,
        tableOutput("hit")
      )
    )
  )
}


format_overviews <- function(
  x
){

  cols <- setdiff(names(x), "suffix")
  labels <- tools::toTitleCase(setdiff(names(x), "suffix"))

  tibble(" " = paste("<b>", labels, "</b>"), !!(x[[1, "suffix"]]) := c(t(x[1,cols])), !!(x[[2, "suffix"]]) := c(t(x[2,cols])))

}
