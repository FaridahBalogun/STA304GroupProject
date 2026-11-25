# server-plots
#
# All of the reactive logic and outputs. This file is sourced (locally) by the
# server function in app.R so that it can see `input`, `output`, and `session`.

# Reactive: the survey data AFTER applying the two demographic filters. Every
# plot and every interpretation reads from filtered(), so a change to a filter
# updates the whole app at once.

#####################################################
# Distribution of one variable (bar chart)          #
#####################################################

# Count delivery preferences after applying both sidebar filters.
delivery_counts <- reactive({
  req(input$commute_group)
  
  commute <- survey$TotalCommuteTime
  keep_year <- survey$YearOfStudy %in% input$year_filter
  
  if (input$commute_group == "longer") {
    keep_commute <- !is.na(commute) & commute > 40
  } else {
    keep_commute <- !is.na(commute) & commute < 40
  }
  
  preferred_format <- survey$PreferredDeliveryFormat[keep_commute & keep_year]
  
  table(factor(preferred_format, levels = delivery_levels))
})

observe({
  counts <- delivery_counts()
  req(sum(counts) > 0, input$commute_yaxis)
  
  if (input$commute_yaxis == "percent") {
    heights <- as.numeric(prop.table(counts)) * 100
    slider_max <- 110
  } else {
    heights <- as.numeric(counts)
    slider_max <- max(50, ceiling(max(heights) * 1.2))
  }
  
  minimum <- max(1, ceiling(max(heights) * 1.08))
  default <- min(slider_max, ceiling(max(heights) * 1.15))
  
  updateSliderInput(
    session,
    "commute_ymax",
    min = minimum,
    max = slider_max,
    value = default
  )
})

# Draw the four bars and print a count above each one.
output$delivery_plot <- renderPlot({
  counts <- delivery_counts()
  
  validate(need(
    sum(counts) > 0,
    "No delivery preferences to show for the selected commute time and years of study."
  ))
  
  req(input$commute_yaxis, input$commute_ymax)
  
  if (input$commute_yaxis == "percent") {
    heights <- as.numeric(prop.table(counts)) * 100
    bar_labels <- sprintf("%.1f%%", heights)
    y_label <- "Percentage of Students (%)"
  } else {
    heights <- as.numeric(counts)
    bar_labels <- heights
    y_label <- "Number of Students"
  }
  
  bar_colours <- if (isTRUE(input$commute_multicolour)) {
    palette_function <- match.fun(input$commute_palette)
    palette_function(length(counts))
  } else {
    input$commute_col
  }
  
  subtitle <- if (input$commute_group == "longer") {
    "Commute time greater than 40 minutes"
  } else {
    "Commute time less than 40 minutes"
  }
  
  bar_positions <- barplot(
    heights,
    names.arg = names(counts),
    main = "Preferred Delivery Format by Commute Time",
    sub = subtitle,
    xlab = "Preferred Delivery Format",
    ylab = y_label,
    col = bar_colours,
    border = "white",
    ylim = c(0, max(input$commute_ymax, ceiling(max(heights) * 1.08)))
  )
  
  text(x = bar_positions, y = heights, labels = bar_labels, pos = 3)
})

# Summarize the most common response using the same counts as the graph.
output$delivery_text <- renderText({
  counts <- delivery_counts()
  total <- sum(counts)
  req(total > 0)
  
  top_count <- max(counts)
  top_responses <- names(counts)[counts == top_count]
  top_percent <- 100 * top_count / total
  
  if (length(top_responses) == 1) {
    sprintf(
      'Of the %d students shown, "%s" was the most common response (%d students, %.1f%%).',
      total, top_responses, top_count, top_percent
    )
  } else {
    sprintf(
      "Of the %d students shown, the following responses tied for most common: %s (%d students each, %.1f%% each).",
      total, paste(top_responses, collapse = ", "), top_count, top_percent
    )
  }
})

#####################################################
# Comparison across a group (side-by-side boxplots) #
#####################################################
learning_data <- reactive({
  req(input$learning_measure)
  
  ratings <- survey[[input$learning_measure]]
  
  keep <- survey$YearOfStudy %in% input$year_filter &
    !is.na(ratings) &
    survey$PreferredDeliveryFormat %in% boxplot_delivery_levels
  
  data.frame(
    rating = ratings[keep],
    format = factor(
      survey$PreferredDeliveryFormat[keep],
      levels = boxplot_delivery_levels
    )
  )
})

output$learning_boxplot <- renderPlot({
  d <- learning_data()
  
  validate(need(
    nrow(d) > 0,
    "No learning benefit ratings to show for the selected years of study."
  ))
  
  rating_label <- names(learning_choices)[
    match(input$learning_measure, unlist(learning_choices))
  ]
  
  format <- d$format
  
  
  # Show how many ratings each box is based on.
  group_counts <- table(format)
  axis_labels <- paste0(names(group_counts), "\n(n = ", group_counts, ")")
  
  box_colours <- if (isTRUE(input$learning_multicolour)) {
    palette_function <- match.fun(input$learning_palette)
    palette_function(length(boxplot_delivery_levels))
  } else {
    input$learning_col
  }
  
  boxplot(
    d$rating ~ format,
    drop = FALSE,
    names = axis_labels,
    main = paste(rating_label, "by Preferred Delivery Format"),
    xlab = "Preferred Delivery Format",
    ylab = paste(rating_label, "Rating"),
    col = box_colours
  )
})

learning_summary <- reactive({
  d <- learning_data()
  ratings_by_format <- split(d$rating, d$format, drop = FALSE)
  
  # Match the lower edge, median, and upper edge of each base R box.
  box_values <- t(vapply(
    ratings_by_format,
    function(ratings) {
      if (length(ratings) == 0) {
        return(rep(NA_real_, 3))
      }
      
      unname(boxplot.stats(ratings)$stats[2:4])
    },
    numeric(3)
  ))
  
  data.frame(
    format = names(ratings_by_format),
    n = unname(lengths(ratings_by_format)),
    lower = box_values[, 1],
    median = box_values[, 2],
    upper = box_values[, 3],
    row.names = NULL
  )
})

output$learning_summary_table <- renderTable({
  result <- learning_summary()
  req(any(result$n > 0))
  
  middle_50 <- rep("\u2014", nrow(result))
  has_ratings <- result$n > 0
  middle_50[has_ratings] <- paste(
    result$lower[has_ratings],
    result$upper[has_ratings],
    sep = "\u2013"
  )
  
  data.frame(
    "Preferred delivery format" = result$format,
    "Students with a rating" = result$n,
    "Median rating" = ifelse(has_ratings, result$median, "\u2014"),
    "Middle 50%" = middle_50,
    check.names = FALSE
  )
}, na = "\u2014")

output$learning_summary_text <- renderText({
  result <- learning_summary()
  result <- result[result$n > 0, ]
  req(nrow(result) > 0)
  
  if (nrow(result) == 1) {
    return(sprintf(
      "For the selected years, ratings are available only for %s. The median rating is %g out of 7.",
      result$format, result$median
    ))
  }
  
  top_median <- max(result$median)
  top_formats <- result$format[result$median == top_median]
  
  if (length(top_formats) == 1) {
    sprintf(
      "Among the students shown, %s had the highest median rating (%g out of 7).",
      top_formats, top_median
    )
  } else {
    sprintf(
      "The highest median rating was %g out of 7, shared by %s.",
      top_median, paste(top_formats, collapse = ", ")
    )
  }
})



output$learning_heatmap <- renderPlot({
  req(input$heatmap_format, input$heatmap_values, input$heatmap_palette)
  
  keep <- survey$YearOfStudy %in% input$year_filter
  
  if (input$heatmap_format != "all") {
    keep <- keep &
      survey$PreferredDeliveryFormat %in% input$heatmap_format
  }
  
  # Each row represents a learning style. Each column represents a rating.
  counts <- t(vapply(
    heatmap_learning_choices,
    function(column) {
      ratings <- survey[[column]][keep]
      as.integer(table(factor(ratings, levels = 1:7)))
    },
    integer(7)
  ))
  
  row_totals <- rowSums(counts)
  
  validate(need(
    any(row_totals > 0),
    "No learning ratings to show for the selected years and delivery format."
  ))
  
  if (input$heatmap_values == "percent") {
    values <- sweep(counts, 1, pmax(row_totals, 1), "/") * 100
    scale_max <- 100
    scale_label <- "Percentage"
  } else {
    values <- counts
    scale_max <- max(counts)
    scale_label <- "Count"
  }
  
  # Mark learning styles with no recorded ratings for this selection.
  values[row_totals == 0, ] <- NA_real_
  
  palette_end <- switch(
    input$heatmap_palette,
    blue = "#2166AC",
    teal = "#00796B",
    purple = "#6A3D9A"
  )
  
  colours <- colorRampPalette(c("#F7F7F7", palette_end))(100)
  
  # Put the heatmap on the left and its colour scale on the right.
  layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
  
  par(mar = c(4, 10, 3, 0.5))
  plot.new()
  plot.window(xlim = c(0.5, 7.5), ylim = c(0.5, 3.5),
              xaxs = "i", yaxs = "i")
  
  for (row in seq_len(nrow(values))) {
    y <- nrow(values) - row + 1
    
    for (rating in 1:7) {
      value <- values[row, rating]
      
      if (is.na(value)) {
        cell_colour <- "#DDDDDD"
        cell_label <- "\u2014"
        label_colour <- "black"
      } else {
        fraction <- value / scale_max
        colour_index <- 1 + floor(fraction * (length(colours) - 1))
        cell_colour <- colours[colour_index]
        label_colour <- if (fraction >= 0.65) "white" else "black"
        
        cell_label <- if (input$heatmap_values == "percent") {
          sprintf("%.1f%%", value)
        } else {
          as.character(value)
        }
      }
      
      rect(rating - 0.5, y - 0.5, rating + 0.5, y + 0.5,
           col = cell_colour, border = "white")
      
      if (isTRUE(input$heatmap_show_labels)) {
        text(rating, y, labels = cell_label, col = label_colour, cex = 0.85)
      }
    }
  }
  
  axis(1, at = 1:7)
  axis(
    2,
    at = 3:1,
    labels = paste0(names(heatmap_learning_choices), " (n = ", row_totals, ")"),
    las = 1,
    tick = FALSE,
    cex.axis = 0.85
  )
  title(main = "Learning Benefit Ratings", xlab = "Rating (1\u20137)")
  
  # Draw the colour scale using the same range as the cells.
  par(mar = c(4, 0.5, 3, 3.5))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, scale_max),
              xaxs = "i", yaxs = "i")
  
  edges <- seq(0, scale_max, length.out = length(colours) + 1)
  rect(0.15, head(edges, -1), 0.55, tail(edges, -1),
       col = colours, border = NA)
  
  ticks <- if (input$heatmap_values == "percent") {
    seq(0, 100, by = 25)
  } else {
    unique(round(seq(0, scale_max, length.out = 5)))
  }
  
  tick_labels <- if (input$heatmap_values == "percent") {
    paste0(ticks, "%")
  } else {
    ticks
  }
  
  axis(4, at = ticks, labels = tick_labels, las = 1, cex.axis = 0.8)
  mtext(scale_label, side = 3, line = 0.5, cex = 0.8)
})

output$heatmap_test_text <- renderText({
  req(input$heatmap_format)
  
  # Apply the same Year of Study and delivery-format filters as the heatmap.
  keep <- survey$YearOfStudy %in% input$year_filter
  
  if (input$heatmap_format != "all") {
    keep <- keep &
      survey$PreferredDeliveryFormat %in% input$heatmap_format
  }
  
  # Each row is one student. Each column is one learning style.
  ratings <- survey[keep, unname(heatmap_learning_choices), drop = FALSE]
  
  # Use students who gave a valid 1–7 rating for all three styles.
  valid_ratings <- lapply(ratings, function(x) x %in% 1:7)
  ratings <- as.matrix(ratings[Reduce(`&`, valid_ratings), , drop = FALSE])
  n_students <- nrow(ratings)
  
  if (n_students < 2) {
    return(
      "The Friedman test needs at least two students who rated all three learning styles."
    )
  }
  
  # Rank the three ratings within each student, averaging ties.
  student_ranks <- t(apply(ratings, 1, rank, ties.method = "average"))
  
  if (all(student_ranks == 2)) {
    return(sprintf(
      "All %d students who rated all three styles gave the same rating to each style within their own responses. The Friedman test cannot be calculated for this selection.",
      n_students
    ))
  }
  
  average_ranks <- colMeans(student_ranks)
  ranking_summary <- paste(
    sprintf("%s = %.2f", names(heatmap_learning_choices), average_ranks),
    collapse = ", "
  )
  
  test <- friedman.test(ratings)
  
  p_label <- if (test$p.value < 0.001) {
    "p < 0.001"
  } else {
    sprintf("p = %.3f", test$p.value)
  }
  
  conclusion <- if (test$p.value < 0.05) {
    "The Friedman test found evidence that students tended to rate some styles higher than others"
  } else {
    "The Friedman test found insufficient evidence that students tended to rate some styles higher than others"
  }
  
  paste0(
    "This comparison includes ", n_students,
    " students who rated all three learning styles. ",
    "We ranked each student's three ratings from lowest (1) to highest (3), ",
    "averaging tied ranks. Average ranks were ", ranking_summary, ". ",
    conclusion, " (", p_label, ")."
  )
})

#####################################################
# Analysis tab: overall summary table               #
#####################################################

 #Mean / median / SD of every rating item across ALL respondents (this table is
 #a fixed overview, so it does not use the Explore-tab filters).
output$summary_table <- renderTable({
  items <- unlist(learning_choices)
  data.frame(
    Variable = names(learning_choices),
    Mean     = sapply(items, function(v) round(mean(survey[[v]]), 1)),
    Median   = sapply(items, function(v) median(survey[[v]])),
    SD       = sapply(items, function(v) round(sd(survey[[v]]), 1)),
    row.names = NULL,
    check.names = FALSE
  )
})

output$delivery_frequency <- renderTable({
  counts <- table(
    factor(survey$PreferredDeliveryFormat, levels = delivery_levels)
  )
  
  data.frame(
    "Preferred Delivery Format" = names(counts),
    "Frequency" = as.integer(counts),
    check.names = FALSE
  )
}, digits = 0)