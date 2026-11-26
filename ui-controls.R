# ui-controls
# Splitting the sidebar this way keeps app.R short and readable.

# ---- which variable(s) to plot -------------------------------------------
# These depend on the plot type, so each plot gets its own conditionalPanel.
commute_selector <- selectInput(
  inputId = "commute_group",
  label = "Which commute time distribution would you like to see?",
  choices = commute_choices,
  selected = "longer"
)

learning_selector <- selectInput(
  inputId = "learning_measure",
  label =  "Choose which Learning Style You Would Like to Compare 
                Across Preferred Delivery Formats",
  choices = learning_choices,
  selected = "AuditoryLearning"
)


# ---- demographic filter (the required "filter by a demographic") ---------
year_filter <- checkboxGroupInput(
  inputId = "year_filter",
  label = "Include which years of study?",
  choices = c("1", "2", "3", "4+"),
  selected = c("1", "2", "3", "4+")
) # End conditionalPanel

#heatmap filter by  learning style
heatmap_format_filter <- selectInput(
  inputId = "heatmap_format",
  label = "Include which preferred delivery format?",
  choices = c("All formats" = "all", delivery_levels),
  selected = "all"
)

heatmap_appearance <- conditionalPanel(
  condition = "input.plot_type == 'heatmap' && input.modify_type == 'graph'",
  
  radioButtons(
    inputId = "heatmap_values",
    label = "Display ratings as:",
    choices = c("Percentage" = "percent", "Count" = "count"),
    selected = "percent"
  ),
  
  selectInput(
    inputId = "heatmap_palette",
    label = "Colour palette:",
    choices = c("Blue" = "blue", "Teal" = "teal", "Purple" = "purple"),
    selected = "blue"
  ),
  
  checkboxInput(
    inputId = "heatmap_show_labels",
    label = "Show values inside the cells",
    value = TRUE
  ),
  
  sliderInput(
    inputId = "heatmap_label_size",
    label = "Cell text size",
    min = 0.6,
    max = 1.5,
    value = 0.85,
    step = 0.05
  )
)



# # ---- graph appearance (colours / symbols / axis limits) -------------------
graph_appearance <- div(
  conditionalPanel(
    condition = "input.modify_type == 'graph'",
    
    # appearance for the distribution (bar chart)
    conditionalPanel(
      condition = "input.plot_type == 'commute'",
      
      radioButtons(inputId = "commute_yaxis",
                   label = "Show each response as a:",
                   choices = c("Count"      = "count",
                               "Percentage" = "percent"),
                   selected = "count"),
      
      checkboxInput(inputId = "commute_multicolour",
                    label = "Use a different colour for each response?",
                    value = FALSE),
      
      # a single colour is used when the box above is unchecked ...
      conditionalPanel(
        condition = "input.commute_multicolour == false",
        colourInput(inputId = "commute_col",
                    label = "Input the bar colour.",
                    value = "#78c2ad"),
      ),
      
      # ... otherwise the user picks a palette (one colour per response)
      conditionalPanel(
        condition = "input.commute_multicolour == true",
        selectInput(inputId = "commute_palette",
                    label = "Colour palette (one colour per response):",
                    choices = list("Rainbow" = "rainbow",
                                   "Heat"    = "heat.colors",
                                   "Terrain" = "terrain.colors",
                                   "Topo"    = "topo.colors"),
                    selected = "rainbow")
      ),
      
      sliderInput(
        inputId = "commute_ymax",
        label = "Maximum y-axis value",
        min = 1,
        max = 50,
        value = 40
      ),
    ), # End conditionalPanel
    # 
    #     # appearance for the group comparison (boxplot)
    conditionalPanel(
      condition = "input.plot_type == 'learning'",
      
      checkboxInput(inputId = "learning_multicolour",
                    label = "Use a different colour for each Delivery Format?",
                    value = FALSE),
      
      # a single colour is used when the box above is unchecked ...
      conditionalPanel(
        condition = "input.learning_multicolour == false",
        colourInput(inputId = "learning_col",
                    label = "Input the box colour.",
                    value = "#f3969a")
      ),
      
      # ... otherwise the user picks a palette (one colour per group)
      conditionalPanel(
        condition = "input.learning_multicolour == true",
        selectInput(inputId = "learning_palette",
                    label = "Colour palette (one colour per group):",
                    choices = list("Rainbow" = "rainbow",
                                   "Heat"    = "heat.colors",
                                   "Terrain" = "terrain.colors",
                                   "Topo"    = "topo.colors"),
                    selected = "rainbow")
      ),
    ), # End conditionalPanel
  ), # End conditionalPanel (modify_type == 'graph')
)