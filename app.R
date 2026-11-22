#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
#library(shinycssloaders) 
#library(colourpicker)

survey <- read.csv("Survey Data Set (1).csv") 

source(file.path("ui-latex.R"))
source(file.path("ui-controls.R"))

ui <- page_navbar(
  theme = bs_theme(version = 5,
                   bootswatch = "minty", 
                   "navbar-bg" = "#e08a8e"),

  
  title = "Ctrl + Alt + Stat: Preferred Delivery Format Survey Explorer",
  
  header = latex_tags,
  
  nav_panel(
    title = "Interactive Graph Explorer", 
    
    layout_sidebar(
      sidebar = sidebar(
        
        selectInput(inputId = "plot_type",
                    label = "Choose The Graph Option You Would Like to See",
                    choices = list("Distribution of Preferred Delivery Format" = "DF",
                                   "Comparison across Learning Styles" = "LS"),
                    selected = "DF"), 
        #DF - change y-axis based on short/longer commute times
        #LS - change y-axis by 1-7 rating of benefitting from different learning
        #styles
        
        selectInput(inputId = "modify_type",
                    label = "What would you like to modify?",
                    choices = list("Year of Study" = "data",
                                   "Graph Appearance" = "graph"),
                    selected = "data"),
        
        var_selectors,    # which variable(s) to plot; see R/ui-controls.R
        #data_filters,     # demographic filters;        see R/ui-controls.R
        #graph_appearance, # colours/symbols/limits;     see R/ui-controls.R
        
        width = 400,
        open = "always"
      )
  )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
