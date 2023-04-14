library(shiny)
library(plotly)
library(CitationProfileR)
library(shinythemes)

options(shiny.maxRequestSize=30*1024^2)

# Define UI for data upload app ----
ui <- navbarPage(
  theme = shinytheme("united"),
  title = "CitationProfileR",
  id = "tabs",
  header = tagList(shinybusy::add_busy_spinner(spin = "circle")),

  ### tab one -- Home/About/Ethics
  tabPanel(
    title = "Home",

    h4("Gender Breakdown Graph"),
    # plotOutput("genderBarPlot"),
    plotOutput("testPlot"),

    shiny::navlistPanel(
      # "Penguins",
      tabPanel(title = "About",
               htmltools::includeMarkdown(path = "about.md")
      ),
      tabPanel(title = "Ethics",
               htmltools::includeMarkdown(path = "ethics.md")
      ),
      tabPanel(title = "Help & Use Cases",
               htmltools::includeMarkdown(path = "help.md")
      )
    )
  ),

  # ### tab two -- upload pdf manuscripts
  # tabPanel(
  #   title = "Upload",
  #   shiny::fluidRow(
  #     # style = "border: 1px solid black;",
  #     column(
  #       width = 12,
  #       h4("Upload your citation files"),
  #       shiny::fileInput(
  #         inputId = "file",
  #         label = "",
  #         multiple = TRUE,
  #         accept = c(".pdf")
  #       )
  #     )
  #   )
  #
  # ),
  #
  # ### tab three -- process data and download dataset
  # tabPanel(
  #   title = "Citation Data",
  #
  #   # download citation data
  #   shiny::fluidRow(
  #     # style = "border: 1px solid black;",
  #     column(
  #       width = 12,
  #       h4("Download Your Citation Data"),
  #       mainPanel(
  #         downloadButton(
  #           outputId = "downloadData",
  #           label = "Download CSV"
  #         )
  #       )
  #     )
  #   )
  # ),

  # ### tab four -- analysis report
  # tabPanel(
  #   title = "Analysis",
  #   shiny::fluidRow(
  #     column(
  #       width = 12,
  #       h4("Gender Breakdown Graph"),
  #       plotOutput("genderBarPlot")
  #     )
  #   ),
  #
  #   # download analysis
  #   shiny::fluidRow(
  #     column(
  #       width = 12,
  #       h4("Download Your Analysis Report"),
  #       mainPanel(
  #         downloadButton(
  #           outputId = "downloadData",
  #           label = "Download PDF"
  #         )
  #       )
  #     )
  #   )
  #
  # )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {

  rv <- shiny::reactiveValues()

  rv$df <- data.frame()
  rv$upload_df <- data.frame()
  rv$CiteSource <- data.frame()
  rv$unique <- data.frame()

  #### Upload files tab section ####
  #upload on click
  shiny::observeEvent(input$file,{
    shiny::validate(need(input$file != "", "Select your bibliographic file to upload..."))

    if (is.null(input$file)) {
      return(NULL)
    } else {
      #upload files one-by-one
      path_list <- input$file$datapath
      rv$upload_number <- 0
      rv$upload_number <- rv$upload_number + 1
      suggested_source <- stringr::str_replace_all(input$file$name, ".ris", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".bib", "")
      suggested_source <- stringr::str_replace_all(suggested_source, ".txt", "")
      upload_df <- read_citations(files=input$file$datapath,
                                  cite_sources = suggested_source,
                                  cite_labels = rep("", length(input$file$datapath)),
                                  cite_strings =rep("", length(input$file$datapath)))
      upload_length <- upload_df %>%
        dplyr::group_by(cite_source) %>%
        dplyr::count(name="records") %>%
        dplyr::rename(source = cite_source)

      #create a dataframe summarising inputs
      df <- data.frame('file' = input$file,
                       'suggested_source' = suggested_source,
                       'label' = rep("", length(input$file$datapath)),
                       'string' = rep("", length(input$file$datapath)))

      upload_df <- dplyr::left_join(upload_df, df, by=c("cite_source"="suggested_source")) %>%
        dplyr::select(-label, -string) %>%
        dplyr::select(cite_source, cite_label, cite_string, everything())

      # make sure required cols are present
      required_cols <- c("title", "doi", "label","isbn","source",
                         "year", "journal", "pages", "volume", "number",
                         "abstract")
      upload_df[required_cols[!(required_cols %in% colnames(upload_df))]] = NA

      df <- dplyr::left_join(upload_length, df, by=c("source" = "suggested_source")) %>%
        dplyr::select(file.name,records, source, label, string)

      rv$df <- dplyr::bind_rows(rv$df, df)
      rv$upload_df <- dplyr::bind_rows(rv$upload_df, upload_df)

    }
  })


  # # display summary input table - summary of files added
  output$tbl_out <- DT::renderDataTable({
    DT::datatable(rv$df,
                  editable = TRUE,
                  options = list(paging = FALSE,
                                 searching = FALSE),
                  rownames = FALSE)
  })

  # when file upload table is edited, edit reactive value upload df
  shiny::observeEvent(input$tbl_out_cell_edit, {

    # make sure not blank to avoid blanks in output

    info <- input$tbl_out_cell_edit
    val <- info$value

    if(val == ""){
      val <- NA
    }

    rv$df[info$row, info$col+1] <- val

    # get rownames for file
    row_indexes <- rv$upload_df %>%
      dplyr::mutate(rowname = dplyr::row_number()) %>%
      dplyr::group_by(file.name) %>%
      dplyr::summarise(min_row = dplyr::first(rowname), max_row=dplyr::last(rowname))

    rows <- row_indexes[info$row, 2:3]
    col <- paste0("cite_", names(rv$df[info$col+1]))
    file <- rv$df[info$row,1]

    rv$upload_df[c(rows$min_row:rows$max_row), col] <- val

  })


  ### Deduplication tab ####

  # when dedup button clicked, deduplicate
  shiny::observeEvent(input$identify_dups,{
    last_message <- NULL
    dedup_results <- withCallingHandlers(
      dedup_citations(rv$upload_df, merge_citations = TRUE),
      message = function(m) {
        if (!is.null(last_message)) removeNotification(last_message)
        last_message <<- showNotification(m$message, duration = NULL, type = "message")
      }
    )
    rv$unique <- dedup_results$unique

    n_citations <- nrow(rv$upload_df)
    n_unique <- nrow(rv$unique)
    n_duplicate <-n_citations - n_unique
    if (!is.null(last_message)) removeNotification(last_message)

    shinyalert::shinyalert("Deduplication complete",
                           paste("From a total of", n_citations, "citations added, there are", n_unique, "unique citations. Compare citations across sources,
                   labels, and strings in the visualisation tab"), type = "success")

  })

  #### Visualization tab ####
  output$plotgraph1<-plotly::renderPlotly({
    n_unique <- count_unique(rv$unique)

    # for each unique citation, which sources/ strings/ labels are present
    source_comparison <- compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_heatmap(source_comparison)
  })

  plotInput <- reactive({
    source_comparison <- CiteSource::compare_sources(rv$unique, comp_type = input$comp_type)
    plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))

  })

  output$plotgraph2<-shiny::renderPlot({
    print(plotInput())
  })

  output$downloadPlot <- shiny::downloadHandler(
    filename = function() { paste("upset", '.png', sep='') },
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    })
  output$reviewTab<-DT::renderDataTable({
    citations<-rv$unique
    citations$source<-rv$unique$cite_source
    record_level_table(citations=citations,return = "DT")
  })

  #### Export tab ####

  # Downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },

    content = function(file) {
      write.csv(rv$unique, file)
    }
  )

  # Downloadable bibtex
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".bib", sep="")
    },

    content = function(file) {
      export_bib(rv$unique, file)
    }
  )


  ### try plotly with shiny
  ## bar 1
  output$genderBarPlot <- renderPlotly({
    dat1 <- data.frame(
      gender = factor(c("Female", "Male", "Inconclusive")),
      count = c(3, 5, 2)
    )

    bar <- ggplot(data=dat1, aes(x=gender, y=count)) +
      geom_bar(stat="identity")

    fig <- ggplotly(bar)
    fig
  })

  ## bar 2
  output$testPlot <- renderPlotly({
    dat1 <- data.frame(
      sex = factor(c("Female","Female","Male","Male")),
      time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
      total_bill = c(13.53, 16.81, 16.24, 17.42)
    )

    p <- ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      scale_fill_manual(values=c("#999999", "#E69F00"))

    fig <- ggplotly(p)
    fig
  })

}

# Create Shiny app ----
shinyApp(ui, server)
