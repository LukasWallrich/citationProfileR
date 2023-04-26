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
      # style = "border: 1px solid black;",
      column(
        width = 12,
        h4("Upload your citation files"),
        shiny::fileInput(
          inputId = "citationFile",
          label = NULL,
          multiple = TRUE,
          accept = c(".pdf"),
          buttonLabel = "Browse"
        )
      )
    )

  ),

  ### tab three -- process data and download dataset
  tabPanel(
    title = "Citation Data",

    # download citation data
    shiny::fluidRow(
      # style = "border: 1px solid black;",
      column(
        width = 12,
        h4("Download Your Citation Data"),
        mainPanel(
          downloadButton(
            outputId = "downloadData",
            label = "Download CSV"
          )
        )
      )
    )
  ),

  ### tab four -- analysis report
  tabPanel(
    title = "Analysis",
    shiny::fluidRow(
      column(
        width = 12,
        h4("Gender Breakdown Plot"),
        plotlyOutput("genderBarPlot")
      )
    ),

    # download analysis
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

  names <- c("Alex", "Jordan", "Casey", "Tom", "Grace", "Cindy", "Robert")

  f_count <- 0
  m_count <- 0
  i_count <- 0. # i stands for inconclusive

  for (name in names) {
    guess <- guess_gender(name)$gender
    if (guess == "female") {
      f_count <- f_count + 1
    } else if (guess == "male") {
      m_count <- m_count + 1
    } else {
      i_count <- i_count + 1
    }
  }

  ### try plotly with shiny
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
}

# Create Shiny app ----
shinyApp(ui, server)

