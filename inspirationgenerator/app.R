# This app returns a random link from a list of bookmarks
# Author: Ben Day
# Created on: 11/02/2021
# Modified on: 13/02/2021

library(shiny)
library(tidyverse)
library(DT)
library(rvest)

# USER INTERFACE
ui <- fluidPage(

    # Application title
    headerPanel(
        fluidRow(
            div("Inspiration Generator"),
            div(h5("")),
            div(style = "font-size: 14px; 
                font-style: italic", 
                          "Follow the link for random inspiration..."),
            div(h5(""))),
        windowTitle = "Inspiration Generator"
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = 'ins', 
                        type = 'tabs',
                        tabPanel("Home",
                                 h1(""),
                                 actionButton(inputId = 'random',
                                              label = div("Find me a random link",
                                                          icon('lightbulb')),
                                              style = "color: #fff; 
                                              background-color: #03c04a; 
                                              border-color: #5dbb63;
                                              font-size: medium;"),
                                 #img(src = "app-ideas.png", height = 30),
                                 h1(""),
                                 uiOutput("tab"),
                                 tags$ui
                        ),
                        tabPanel("Upload",
                                 h1(""),
                                 h3("Links are not saved!"),
                                 fileInput(inputId = 'file',
                                           label = "Upload list",
                                           accept = c('html', 'txt', 'csv')),
                                 h1("")
                                 ),
                        tabPanel("Blogs",
                                 h1(""),
                                 actionButton(inputId = 'fs',
                                              label = div("Farnam Street Blog link ",
                                                          icon('lightbulb')),
                                              style = "color: #fff; 
                                              background-color: #cc3232; 
                                              border-color: #7e7e7e;
                                              font-size: medium;"),
                                 h1(""),
                                 actionButton(inputId = 'pg',
                                              label = div("Paul Graham Blog link ",
                                                          icon('lightbulb')),
                                              style = "color: #fff; 
                                              background-color: #666699; 
                                              border-color: #7e7e7e;
                                              font-size: medium;"),
                                 h1(""),
                                 actionButton(inputId = 'kk',
                                              label = div("Kevin Kelly Blog link   ",
                                                          icon('lightbulb')),
                                              style = "color: #000; 
                                              background-color: #fff; 
                                              border-color: #7e7e7e;
                                              font-size: medium;")),
                        tabPanel("New",
                                 h1(""),
                                 textInput(inputId = "newlink",
                                           label = "URL"),
                                 textInput(inputId = "newname",
                                           label = "Label"),
                                 actionButton(inputId = 'add', 
                                              label = "Add link to list",
                                              style = "color: #fff; 
                                              background-color: #337ab7; 
                                              border-color: #2e6da4;"))
                        )
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = 'tabs', 
                        type = 'tabs',
                        #tabPanel("List", htmlOutput(outputId = 'list')),
                        tabPanel("Inspiration", 
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
    
    # Clean links from txt
    a <- read.csv('sitemaps/kk.txt', header = FALSE) %>% 
        rename('links' = 1) %>%
        filter(str_detect(links, 'kk.org'))
    
    b <- read.csv('sitemaps/pg.txt', header = FALSE) %>%
        rename('links' = 1) %>%
        filter(str_detect(links, 'graham'))
    
    c <- read.csv('sitemaps/fs.txt', header = FALSE) %>%
        rename('links' = 1) %>%
        filter(str_detect(links, 'fs.blog/2'))
    
    # Repo hyperlink
    url <- a("View RShiny code on Github", 
             href = "https://github.com/bendaytoday/inspirationgenerator",
             target = '_blank')
    output$tab <- renderUI({
        tagList(h6(url))
    })
    
    # # Function to wrangle data
    # getPage <- function() {
    #     if (is.null(input$file)) {
    #         output$aa <- DT::renderDataTable({
    #             df %>% rename("Name" = 1,
    #                           "Link" = 2) %>%
    #                 select(1, 2) %>%
    #                 sample_n(nrow(.))}, 
    #             options = list(dom  = '<"top">t<"bottom">',
    #                                                               searching = F,
    #                                                               paging = FALSE
    #         ))
    #         DT::dataTableOutput("aa")
    #         }
    #     else return(includeHTML(input$file$datapath))
    # }
    # 
    # output$list <- renderUI({getPage()})
    
    observeEvent(input$random, {
        
        df <- read.csv('inspiration.csv')
        
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
    
    observeEvent(input$fs, {
            
        # Farnam Street is dataset c
        y = slice(c, sample(1:nrow(c), 1))
        
        output$link <- renderUI(a(as.character(y$links), 
                                  href = as.character(y$links),
                                  target = "_blank")
        )
        
        updateTabsetPanel(session, "tabs",
                          selected = "Inspiration")
        
    })
    
    observeEvent(input$pg, {
        
        # Paul Graham is dataset b
        y = slice(b, sample(1:nrow(b), 1))
        
        output$link <- renderUI(a(as.character(y$links), 
                                  href = as.character(y$links),
                                  target = "_blank")
        )
        
        updateTabsetPanel(session, "tabs",
                          selected = "Inspiration")
        
    })
    
    observeEvent(input$kk, {
        
        # Kevin Kelly is dataset a
        y = slice(a, sample(1:nrow(a), 1))
        
        output$link <- renderUI(a(as.character(y$links), 
                                  href = as.character(y$links),
                                  target = "_blank")
        )
        
        updateTabsetPanel(session, "tabs",
                          selected = "Inspiration")
        
    })
    

    # Write new URL to list ---------------------------------------------------

    observeEvent(input$add, {
        
        if (is.null(input$newlink)) return()
        else if (!is.null(input$newlink) & (input$newname == '')) {
            if (input$newlink %in% df$links == TRUE) {
                showNotification('Duplicate link not added', type = 'error')
                df <- df %>% distinct(names, links, .keep_all = TRUE)
                write.csv(df, 'inspiration.csv', row.names = FALSE)
                return()
            }
            else {
                df <- df %>% add_row(names = 'New link 1', 
                                     links = paste0(input$newlink))
                showNotification('Success! Added "New link 1"', type = 'message')
                write.csv(df, 'inspiration.csv', row.names = FALSE)
            }
        }
        
        else {
            if (input$newlink %in% df$links == TRUE) {
                showNotification('Duplicate link not added', type = 'error')
                df <- df %>% distinct(names, links, .keep_all = TRUE)
                write.csv(df, 'inspiration.csv', row.names = FALSE)
                return()
            }
            else {
                df <- df %>% add_row(names = paste0(input$newname), 
                                     links = paste0(input$newlink)) %>%
                    distinct(names, links, .keep_all = TRUE)
                
                showNotification('Success!', type = 'message')
                write.csv(df, 'inspiration.csv', row.names = FALSE)
            }
        }
        
    })    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
