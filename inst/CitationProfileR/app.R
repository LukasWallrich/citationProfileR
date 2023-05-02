library(shiny)
library(plotly)
library(CitationProfileR)
library(shinythemes)

options(shiny.maxRequestSize=30*1024^2)

################### Define UI for data upload app ###################
ui <- navbarPage(
  theme = shinytheme("united"),
  title = "CitationProfileR",
  id = "tabs",
  header = tagList(shinybusy::add_busy_spinner(spin = "circle")),

  ### tab one -- Home/About/Ethics
  tabPanel(
    title = "Home",

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

  ### tab two -- upload pdf manuscripts
  tabPanel(
    title = "Upload",
    shiny::fluidRow(
      #allow for input validation HTML/CSS styling
      shinyFeedback::useShinyFeedback(),
      # style = "border: 1px solid black;",
      column(
        width = 12,
        h4("Upload your paper"),
        shiny::fileInput(
          inputId = "paper",
          label = NULL,
          multiple = FALSE,
          accept = c(".pdf"),
          buttonLabel = "Browse"
        ),
        textOutput("pdf_confirmation"),
        tableOutput("citation_table"),
        shiny::downloadButton(
          outputId = "redownload",
          label = "Download the file you just uploaded LOL"
        )
      )
    )

  ),

  ### tab three -- process data and download dataset
  ##############
  tabPanel(
    title = "Citation Data",

    # download citation data
    shiny::fluidRow(
      # style = "border: 1px solid black;",
      column(
        width = 12,
        h4("Download Your Citation Data"),
        mainPanel(
          htmlOutput("percentage_progress"),
          htmlOutput("progress_message"),
          tableOutput("transparency_table"),
          downloadButton(
            outputId = "downloadData",
            label = "Download CSV"
          )
        )
      )
    )
  ),
  #########
  ### tab four -- analysis report
  #####################
  tabPanel(
    title = "Analysis",
    shiny::fluidRow(
      column(
        width = 12,
        h4("Gender Breakdown Plot"),
        plotlyOutput("genderBarPlot")
      )
    ),

  ################
  ### download analysis
  ##############
    shiny::fluidRow(
      column(
        width = 12,
        h4("Download Your Analysis Report"),
        mainPanel(
          downloadButton(
            outputId = "downloadReport",
            label = "Download Diversity Report PDF"
          )
        )
      )
    )

  )

)


################### Define server logic to read selected file ###################
server <- function(input, output, session) {

  ### tab two -- upload pdf manuscripts
  #####################################
  ## Where we will hold the df that we will pass to tab 3
  citation_data <- reactiveVal()
  #For progress msgs
  perc_prog <- reactiveVal()
  prog_msg <- reactiveVal()
  output$percentage_progress <- renderUI(perc_prog(HTML("<h1>0%</h1>")))
  output$progress_message <- renderUI(prog_msg(HTML("<h1>Input A Paper On The Upload Tab</h1>")))
  observeEvent(input$paper, {

    #verify that the file upload is pdf kind
    not_pdf <- tools::file_ext(input$paper$name) != "pdf"
    if(not_pdf){
      shinyFeedback::feedbackWarning("paper", not_pdf , "Please select a pdf")
      return(NULL)
    }
    req(!not_pdf)

    #Confirm to user that file has been received
    output$pdf_confirmation <- renderPrint({
      paste("File uploaded:", input$paper$name)
    })

    #start messages
    output$percentage_progress <- renderUI(perc_prog(HTML("<h1>25%</h1>")))
    output$progress_message <- renderUI(prog_msg(HTML("<h1>Citations Are Being Extracted</h1>")))

    # extract the citations from PDf
    uploaded_paper <- input$paper$datapath
    citations <-parse_pdf_refs(uploaded_paper)
    citation_data(citations)
    output$citation_table <- renderTable(citation_data())

    # test upload successful with a redownload (already tested above with extracted table output so notfully needed)
    output$redownload <- downloadHandler(
      filename =  function() {
        paste(input$paper$name,".pdf", sep="")
      },
      content = function(file) {
        file.copy(uploaded_paper, file)
      },
      contentType = "application/pdf"
    )

  })

  ##################################


  ### tab three -- process data and download dataset
  ##################################################

  #once the data from GROBID has changed
  names_data <- reactiveVal()
  observeEvent(citation_data(), {

    output$percentage_progress <- renderUI(perc_prog(HTML("<h1>50%</h1>")))
    output$progress_message <- renderUI(prog_msg(HTML("<h1>First Names and Affiliations are Being Gathered</h1>")))
    # use reactive storing the output of parse_pdf_refs in get_author_info
    Full_author_info <- get_author_info(citation_data())

    #gets index of all column names that contain affiliation
    affiliation_columns<- grep("^affiliation", colnames(Full_author_info))
    # Initial order for  transparency table, selecting the columns we want to show
    col_names <- c('index', 'OG_Author', 'title', 'OG_doi', 'Date', 'given', 'family')
    info_tbl<- Full_author_info[, c(col_names,colnames(Full_author_info[affiliation_columns]))]

    names_data(info_tbl)
    #output to see the progress after getting name and affiliation
    output$full_authors <- renderTable(dplyr::arrange(info_tbl, index))

  })

  Transparency_data <- reactiveVal()
  observeEvent(names_data(), {
    output$percentage_progress <- renderUI(perc_prog(HTML("<h1>75%</h1>")))
    output$progress_message <- renderUI(prog_msg(HTML("<h1>Genders are Being Predicted</h1>")))
    # use reactive storing the output of get_author_info that gives us first name
    all_first_names <- names_data()$given

    # function to make api call and get info needed for the map over all names
    get_prediction_and_accuracy <- function(name){

      #if middle name or initial are there, use only the first name. Also get rid of any . as they break URL
      name <- strsplit(name, " ")[[1]][[1]]
      name <- gsub("[.]", "", name)

      if(name == "No result matched" | name == "Inconclusive" | is.na(name)){
        #nested_df <- tibble::tibble(data = list(tibble::tibble(gender_prediction = NA , accuracy = NA)))
        #nested_df <- list(NA, NA)
        nested_df <- tibble::tibble(gender_prediction = NA, accuracy = NA)
      }else{
        #Do API prediction call
        pred <- guess_gender(name, "UK")

        # troubleshoot print
        #print("Printing Gender Guesses")
        #print(pred)

        gender <- pred$gender
        accuracy_pred <- pred$accuracy
        #if returns a prediction write those down, else use NA
        if(gender == "male" | gender =="female"){
          #nested_df <- tibble::tibble(data = list(tibble::tibble(gender_prediction = gender , accuracy = accuracy_pred)))
          #nested_df <- list(gender, accuracy_pred)
          nested_df <- tibble::tibble(gender_prediction = gender, accuracy = accuracy_pred)
        }else{
          #nested_df <- tibble::tibble(data = list(tibble::tibble(gender_prediction = NA , accuracy = NA)))
          #nested_df <- list(NA, NA)
          nested_df <- tibble::tibble(gender_prediction = NA, accuracy = NA)
        }
      }
      #return the correct nested df
      return(nested_df)
    }

    # dataframe of the gender prediction and accuracy per name
    gender_columns <- purrr::map_dfr(all_first_names, get_prediction_and_accuracy)

    # combine general information per name and its gender prediction
    Trans_data <- cbind(names_data(), gender_columns)

    #styling
    Trans_data <- Trans_data %>%
      rename(Extracted_Authors = OG_Author, Extracted_DOI = OG_doi)

    Transparency_data(Trans_data)
    output$transparency_table <- renderTable(dplyr::arrange(Trans_data, index))
    output$percentage_progress <- renderUI(HTML("<h1>100%</h1>"))
    output$progress_message <- renderUI(HTML("<h1>Your Information is Ready</h1>"))
  })

  # TODO: test edge case of clicking the button before inputting
  # change names_data to transparency_table and put it closer to the other one
  output$downloadData <- downloadHandler(
    filename =  function() {
      paste(input$paper$name,"-Transparency-Data",".csv", sep="")
    },
    content = function(file) {
      write.csv(Transparency_data(), file)
    },
    contentType = "application/csv"
  )


  ###########################################################


  ### tab four -- analysis report

  # gender breakdown barplot
  ##################
  output$genderBarPlot <- renderPlotly({
    df <- Transparency_data()

    viz <- df %>%
      dplyr::group_by(gender_prediction) %>%
      dplyr::summarize(count = dplyr::n())
    print(viz)

    bar <- ggplot(data= viz, aes(x=gender_prediction, y = count)) +
      geom_bar(stat="identity")

    fig <- ggplotly(bar)
    fig
  })

  #have to change it
  # Downloadable diversity report pdf
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("diversity-report-", Sys.Date(), ".pdf", sep="")
    },

    content = function(file) {
      # code below is from https://shiny.rstudio.com/articles/generating-reports.html
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("diversity_report_template.Rmd", tempReport, overwrite = TRUE)

      params <- list(f = f_count,
                     m = m_count,
                     i = i_count)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  #
  #   withProgress(message = 'Predicting Genders', value = 0, {
  #     # Number of times we'll go through the loop
  #     n <- nrow(names_data())
  #
  #     finished_df <- names_data()
  #
  #     finished_df <- finished_df %>% dplyr::mutate(gender = NA) %>% dplyr::mutate(accuracy = NA)
  #
  #     for (i in 1:n) {
  #
  #       name <- finished_df$given[n]
  #       prediction <- guess_gender(name)
  #
  #       finished_df$gender <- prediction$gender
  #       finished_df$accuracy <- prediction$accuracy
  #       # Increment the progress bar, and update the detail text.
  #       incProgress(1/n, detail = paste("Predicted ", i, "Names"))
  #
  #       # Pause for 0.1 seconds to simulate a long computation.
  #       Sys.sleep(0.1)
  #     }
  #   })


  ######################
}

# Create Shiny app ----
shinyApp(ui, server)

