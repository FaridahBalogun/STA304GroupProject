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
library(shinycssloaders)
library(colourpicker)

survey <- read.csv("Survey Data Set (1).csv", check.names = FALSE) 


delivery_labels <- c(
  "PO" = "Online",
  "PI" = "In Person",
  "H"  = "Hybrid",
  "NP" = "No Preference"
)
year <- survey[["Year of Study"]]
delivery_codes <- trimws(survey[["Preferred Deliverly Format"]])

survey <- data.frame(
  TotalCommuteTime = survey[["Total Commute Time (Minutes)"]],
  YearOfStudy = ifelse(year >= 4, "4+", as.character(year)),
  PreferredDeliveryFormat = unname(delivery_labels[delivery_codes]),
  HandsOnLearning = survey[["Benefit of Hands On Learning"]],
  AuditoryLearning = survey[["Benefits of Auditory Learning"]],
  VisualLearning = survey[["Benefit of Visual Learning"]]
)

source(file.path("site-helper.R"))
source(file.path("ui-latex.R"))
source(file.path("ui-controls.R"))

options(spinner.type = 8, spinner.color = "#ffce67")

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
                    choices = list("Distribution of Preferred Delivery Format" = 
                                     "commute",
                                   "Comparison across Learning Styles" = 
                                     "learning",
                                   "Learning Rating Heatmap" = "heatmap"),
                    selected = "commute"), 
        
        selectInput(inputId = "modify_type",
                    label = "What would you like to modify?",
                    choices = list("Data Filters" = "data",
                                   "Graph Appearance" = "graph"),
                    selected = "data"),
        
        conditionalPanel(
          condition = "input.plot_type == 'commute'",
          commute_selector
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'learning'",
          learning_selector
        ),
        conditionalPanel(
          condition = "input.modify_type == 'data'",
          year_filter,
          
          conditionalPanel(
            condition = "input.plot_type == 'heatmap'",
            heatmap_format_filter
          )
        ),# demographic filters;        see R/ui-controls.R
        graph_appearance, # colours/symbols/limits;     see R/ui-controls.R
        heatmap_appearance, 
        
        width = 400,
        open = "always"
      ), # end of sidebar
      
      conditionalPanel(
        condition = "input.plot_type == 'commute'",
        withSpinner(plotOutput("delivery_plot")),
        br(),
        textOutput("delivery_text")
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'learning'",
        withSpinner(plotOutput("learning_boxplot"))
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'heatmap'",
        withSpinner(plotOutput("learning_heatmap", height = "360px"))
      )
    ) # end of Conditional Panel
    
  )# end of layout
)


server <- function(input, output, session) {
  source(file.path("server-plots.R"), local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)