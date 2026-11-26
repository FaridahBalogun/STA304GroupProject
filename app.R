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
        withSpinner(plotOutput("learning_boxplot")),
        br(),
        textOutput("learning_summary_text"),
        tableOutput("learning_summary_table")
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'heatmap'",
        withSpinner(plotOutput("learning_heatmap", height = "360px")),
        br(),
        textOutput("heatmap_test_text")
      )
    ) # end of Conditional Panel
    
  ),

  nav_panel(
    title = "Analysis",
    h2("Summary of Findings"),
    p("Across the 95 respondents, the average rating for each learning style was:"),
    tableOutput("summary_table"),
    
    p("Among the 95 respondents, the responses for each delivery format was:"),
    tableOutput("delivery_frequency"),
    
    h3("Preferred Delivery Format By Commute Time"),
    p("To answer our research question: Does commute time have an effect on 
       students' preferred delivery format? We ran a Fisher's Exact Test. 
      Each cell shows the number and percentage of students within that commute 
      group (longer/shorter)."),
    tableOutput("commute_delivery_analysis"),
    textOutput("commute_fisher_result"),
    
    h3("Learning Rating Heatmap"),
    p("Using the Learning Rating Heatmap in the Interactive Graph Explorer Tab, 
      a Freidman test found evidence that students tended to rate 
      some learning styles higher than others across all delivery formats. 
      This relates to our research question: What effect do learning styles 
      (auditory, visual or hands-on) have on students' inclination to choose 
      online, in-person, or hybrid lecture formats?")
  ),
  
  nav_panel(
    title = "About",
    h2("About The Survey"),
    p("This survey was administered to examine the relationships between preferred
      delivery format, learning styles and commute time of students. Our aim was
      to answer the following research questions:"),
    tags$ul(
      tags$li("Do students prefer lectures to be in-person, online or hybrid?"),
      tags$li("Does commute time have an effect on students' preferred delivery 
              format?"),
      tags$li("What effect do learning styles (auditory, visual or hands-on) 
              have on students' inclination to choose online, in-person, 
              or hybrid lecture formats?"),
    ),
      
    h3("The Sample"),
    p("The data contains 95 student responses. Each respondent indicated their 
      year of study, preferred lecture delivery format, rated the 3 learning styles 
      and lecture effectiveness on a 1-7 rating scale and if a change in their 
      commute time would affect their preferred lecture delivery format."),
    
    h3("Caveats"), 
    tags$ul(
      tags$li("The survey was only administered to students in the STA304 class, 
              thus, it is not a random sample and may not generalize to all
              students."),
      tags$li("The rating items are ordinal (1-7), so means should be read with
              some caution"),
    )
  )
)# end of layout



server <- function(input, output, session) {
  source(file.path("server-plots.R"), local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)