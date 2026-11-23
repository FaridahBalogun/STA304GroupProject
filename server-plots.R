filtered <- reactive({
  survey[survey$Year.of.Study %in% input$YoS-filter]
})


DF_counts <- reactive({
  d <- filtered()
  table(d[[input$DF_var]])  # categorical item
})


observeEvent(list(input$DF_yaxis, input$DF_var,
                  input$YoS_filter), {
  counts <- DF_counts()
  if (sum(counts) == 0) return()
  top <- max(as.numeric(counts))
  updateSliderInput(session, "DF_ymax",
                    max = max(50, ceiling(top / 5) * 5),
                    value = ceiling((top + 2) / 5) * 5)
})

output$DF_plot <- renderPlot({
  d <- filtered()
  validate(need(nrow(d) > 0, "No students match the current filters."))
  
  counts   <- DF_counts()
  is_quant <- input$DF_var %in% unlist(DF_choices)
  xlab     <- if (is_quant) paste0(label_of(input$DF_var), " (1 = Low, 7 = High)")
  else label_of(input$DF_var)
  
  # counts vs percentages, controlled by the radio button
  if (input$DF_yaxis == "percent") {
    heights <- as.numeric(prop.table(counts)) * 100
    ylab    <- "Percentage of students (%)"
    labels  <- paste0(round(heights, 1), "%")
  } else {
    heights <- as.numeric(counts)
    ylab    <- "Number of students"
    labels  <- heights
  }
  
  # one colour, or one colour per response from the chosen palette
  bar_cols <- if (isTRUE(input$DF_multicolour)) {
    get(input$DF_palette)(length(heights))
  } else {
    input$DF_col
  }
  
  bp <- barplot(heights,
                names.arg = names(counts),
                main = paste("Distribution of", label_of(input$DF_var)),
                xlab = xlab,
                ylab = ylab,
                ylim = c(0, input$DF_ymax),
                col = bar_cols,
                border = "white")
  
  # print the count/percentage on top of each bar
  text(x = bp, y = heights, labels = labels, pos = 3, cex = 0.9, xpd = TRUE)
})
