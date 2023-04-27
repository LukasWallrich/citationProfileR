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
          tableOutput("full_authors"),
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
  ## Where we will hold the df that we will pass to tab 3
  citation_data <- reactiveVal()
  observeEvent(input$paper, {
    print("I get inside the observe event")
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

    # extract the citations
    uploaded_paper <- input$paper$datapath
    citations <-parse_pdf_refs(uploaded_paper)
    citation_data(citations)
    print("I am able to pass the parse_pdf")
    print(paste("citations are ", data.frame(citations)))
    Full_author_info <- get_author_info(citations)
    # for testing we can display the table of extracted citations if contents is uncommented in UI
    output$citation_table <- renderTable(citation_data())

    output$full_authors <- renderTable(Full_author_info)



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

  ### tab three -- process data and download dataset
  #once the data from GROBID has changed
  # observeEvent(citation_data(), {
  #
  #   print("I get into the get_author_data observe event")
  #   Full_author_info <- get_author_info(citation_data())
  #   print(Full_author_info)
  #   print("I get passed the get_author_info information")
  #   #output$full_authors <- renderTable(Full_author_info)
  # })



  ### tab four -- analysis report
  #names <- c("Alex", "Jordan", "Casey", "Tom", "Grace", "Cindy", "Robert")
  # names <- c("Ali")
  #
  # f_count <- 0
  # m_count <- 0
  # i_count <- 0. # i stands for inconclusive
  #
  # for (name in names) {
  #   guess <- guess_gender(name)$gender
  #   if (guess == "female") {
  #     f_count <- f_count + 1
  #   } else if (guess == "male") {
  #     m_count <- m_count + 1
  #   } else {
  #     i_count <- i_count + 1
  #   }
  # }

  # gender breakdown barplot
  ##################
  output$genderBarPlot <- renderPlotly({
    df <- data.frame(
      gender = c("Female", "Male", "Inconclusive"),
      count = c(f_count, m_count, i_count)
    )

    df$gender <- factor(df$gender, levels = df$gender)

    bar <- ggplot(data=df, aes(x=gender, y=count)) +
      geom_bar(stat="identity")

    fig <- ggplotly(bar)
    fig
  })

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
  ######################
}

# Create Shiny app ----
shinyApp(ui, server)

