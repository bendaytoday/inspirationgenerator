# This app returns a random link from a list of bookmarks
# Author: Ben Day
# Created on: 11/02/2021
# Modified on: 12/02/2021

library(shiny)
library(tidyverse)
library(DT)
library(rvest)

# USER INTERFACE
ui <- fluidPage(

    # Application title
    #titlePanel("Inspiration Generator"),
    headerPanel(
        fluidRow(
            div("Inspiration Generator"),
            div(h5("")),
            div(style = "font-size: 14px; 
                font-style: italic", 
                          "Follow the link for random inspiration...")),
        windowTitle = "Inspiration Generator"
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = 'ins', 
                        type = 'tabs',
                        tabPanel("Many topics",
                                 h1(""),
                                 actionButton(inputId = 'random',
                                              label = "Find me a random link",
                                              style = "color: #000; 
                                              background-color: #ffafd2; 
                                              border-color: #ee5396;
                                              font-size: 16px;"),
                                 h1("")
                        ),
                        tabPanel("Upload my links",
                                 h1(""),
                                 h2("Links are not saved!"),
                                 fileInput(inputId = 'file',
                                           label = "Upload list",
                                           accept = c('html', 'txt', 'csv')),
                                 h1("")
                                 )
                        )
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = 'tabs', 
                        type = 'tabs',
                        tabPanel("List", htmlOutput(outputId = 'list')),
                        tabPanel("Inspiration", 
                            h1(""),
                            uiOutput("tab"),
                            h1(""),
                            # p("Random link", style = "font-size:26px"),
                            # h1(""),
                            htmlOutput("link", inline = TRUE),
                            tags$head(tags$style("#link{color: red;
                                 font-size: 26px;
                                 font-style: italic;
                                 }"
                                                 )
                                      )
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    df <- read.csv('inspiration.csv')
    
    # Repo hyperlink
    url <- a("Github", href = "https://github.com/bendaytoday/inspirationgenerator")
    output$tab <- renderUI({
        tagList("View R Shiny code:", url)
    })
    
    # Function to wrangle data
    getPage <- function() {
        if (is.null(input$file)) {
            output$aa <- DT::renderDataTable({
                df %>% rename("Name" = 1,
                              "Link" = 2) %>%
                    select(1, 2) %>%
                    sample_n(nrow(.))}, 
                options = list(dom  = '<"top">t<"bottom">',
                                                                  searching = F,
                                                                  paging = FALSE
            ))
            DT::dataTableOutput("aa")
            }
        else return(includeHTML(input$file$datapath))
    }

    output$list <- renderUI({getPage()})
    
    observeEvent(input$random, {
        
        # Case if no upload
        if (is.null(input$file)) {
            
            # Return a random link
            y = slice(df, sample(1:nrow(df), 1))
            
            output$link <- renderUI(a(as.character(y$names), 
                                      href = as.character(y$links),
                                      target = "_blank")
            )
        }
        
        # Cases for uploads
        else if (str_detect(input$file$datapath, 'html') == TRUE) {
            
            a <- read_html(input$file$datapath)
            # Get bookmark names
            names <- a %>% html_nodes("dl a") %>%
                html_text()
            # Get links
            links <- a %>% html_nodes("dt a") %>%
                html_attr("href")
            # Make a dataframe
            df <- data.frame(cbind(names, links)) %>%
                filter(str_detect(links, 'http'))
            # Return a random link
            y = slice(df, sample(1:nrow(df), 1))
        
            output$link <- renderUI(a(as.character(y$names), 
                                  href = as.character(y$links),
                                  target = "_blank")
                                  )
        }
        
        else if (str_detect(input$file$datapath, 'csv') == TRUE) {
            
            df <- read.csv(input$file$datapath)
            
            if (ncol(df) != 2) {
                showNotification("File must have 2 columns: names, links. Using default links instead", type = 'error')
                df <- read.csv('inspiration.csv')
            }

            # Return a random link
            y = slice(df, sample(1:nrow(df), 1))
            
            output$link <- renderUI(a(as.character(y$names), 
                                      href = as.character(y$links),
                                      target = "_blank")
            )
        }
        
        updateTabsetPanel(session, "tabs",
                          selected = "Inspiration")
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
