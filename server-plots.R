filtered <- reactive({
  survey[survey$Year.of.Study %in% input$YoS-filter]
})


dist_counts <- reactive({
  d <- filtered()
  if (input$DF_var %in% unlist(DF_choices)) {
    table(factor(d[[input$DF_var]], levels = 1:7))   # rating item
  } else {
    table(d[[input$DF_var]])                          # categorical item
  }
})


observeEvent(list(input$DF_yaxis, input$DF_var,
                  input$YoS_filter), {
  counts <- DF_counts()
  if (sum(counts) == 0) return()
  if (input$DF_yaxis == "percent") {
    top <- max(as.numeric(prop.table(counts))) * 100
    updateSliderInput(session, "DF_ymax",
                      max = 100, value = min(100, ceiling((top + 10) / 5) * 5))
  } else {
    top <- max(as.numeric(counts))
    updateSliderInput(session, "DF_ymax",
                      max = max(50, ceiling(top / 5) * 5),
                      value = ceiling((top + 2) / 5) * 5)
  }
})