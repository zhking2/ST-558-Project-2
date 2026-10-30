library(tidyverse)

# these lines are just a basic list of my variables

n_vars <- c(
  "Product Price" = "Sales",
  "Profit" = "Profit",
  "Quantity" = "Quantity",
  "Discount" = "Discount"
)

c_vars <- c(
  "Region" = "Region",
  "Segment" = "Segment",
  "Category" = "Category"
)

# Helper function to filter numeric ranges safely
filter_num_range <- function(data, var, range_val) {
  if (var == "none" || is.null(range_val)) {
    return(data)
  }
  data |> filter(.data[[var]] >= range_val[1] & .data[[var]] <= range_val[2])
}

# Frequency table (one-way or two-way)
freq_table <- function(data, v1, v2) {
  if (v2 == "none") {
    data |>
      group_by(across(all_of(v1))) |>
      summarize(count = n(), .groups = "drop")
  } else {
    data |>
      group_by(across(all_of(c(v1, v2)))) |>
      summarize(count = n(), .groups = "drop") |>
      pivot_wider(names_from = all_of(v2), values_from = count, values_fill = 0)
  }
}

# Numerical summary
n_summary <- function(data, v1, group) {
  if (group != "none") {
    data <- data |> group_by(across(all_of(group)))
  }
  data |>
    summarize(
      n = n(),
      mean = mean(.data[[v1]], na.rm = TRUE),
      median = median(.data[[v1]], na.rm = TRUE),
      .groups = "drop"
    )
}

# Bar plot based on inputted layout and faceting
bar_plot <- function(data, v1, v2, facet, position) {
  g <- ggplot(data, aes(x = .data[[v1]]))
  if (v2 == "none") {
    g <- g + geom_bar(fill = "blue")
  } else {
    v2_label <- names(c_vars)[c_vars == v2]
    g <- g +
      geom_bar(aes(fill = .data[[v2]]), position = position) +
      labs(fill = if (length(v2_label) > 0) v2_label else v2)
  }
  if (facet != "none") {
    g <- g + facet_wrap(vars(.data[[facet]]))
  }
  v1_label <- names(c_vars)[c_vars == v1]
  g + labs(
    title = paste("Frequency of Sales by", if (length(v1_label) > 0) v1_label else v1),
    y = if (position == "fill") "Proportion of Individual Product Sales" else "Frequency of Sales"
  ) +
    theme_minimal()
}

# All the plots for the numerical variables
fancy_plot <- function(data, plot_type, v1, sv2, group, facet) {
  v1_label <- names(n_vars)[n_vars == v1]
  v1_name <- if (length(v1_label) > 0) v1_label else v1
  
  if (plot_type == "histogram") {
    f <- ggplot(data, aes(x = .data[[v1]]))
    f <- if (group == "none") {
      f + geom_histogram(bins = 100, fill = "blue")
    } else {
      f + geom_histogram(aes(fill = .data[[group]]), bins = 40, position = "identity", alpha = 0.6)
    }
    f <- f + labs(title = paste("Distribution of", v1_name), y = "Number of Individual Product Sales")
  } else if (plot_type == "boxplot") {
    f <- if (group == "none") {
      ggplot(data, aes(x = "All sales", y = .data[[v1]])) +
        geom_boxplot(fill = "blue")
    } else {
      ggplot(data, aes(x = .data[[group]], y = .data[[v1]], fill = .data[[group]])) +
        geom_boxplot()
    }
    f <- f + labs(
      title = paste(v1_name, "by", if (group == "none") "all sales" else group),
      x = if (group == "none") NULL else group
    )
  } else { # scatterplot
    if (!is.null(sv2) && sv2 != "none") {
      data <- data |> filter(!is.na(.data[[sv2]]))
      sv2_label <- names(n_vars)[n_vars == sv2]
      sv2_name <- if (length(sv2_label) > 0) sv2_label else sv2
      
      f <- ggplot(data, aes(x = .data[[sv2]], y = .data[[v1]]))
      f <- if (group == "none") {
        f + geom_point(alpha = 0.3, color = "blue")
      } else {
        f + geom_point(aes(color = .data[[group]]), alpha = 0.3)
      }
      f <- f + labs(title = paste(v1_name, "vs.", sv2_name), x = sv2_name, y = v1_name)
    } else {
      f <- ggplot(data) + geom_blank() + labs(title = "Please select a secondary numeric variable for scatterplot.")
    }
  }
  
  if (group != "none" && plot_type != "boxplot") {
    f <- f + labs(fill = group, color = group)
  }
  if (facet != "none") {
    f <- f + facet_wrap(vars(.data[[facet]]))
  }
  f + theme_minimal()
}

# Slider for numerical range:
nslider <- function(id, var, data) {
  if (var == "none") {
    return(NULL)
  }
  ranger <- range(data[[var]], na.rm = TRUE)
  sliderInput(
    id,
    label = paste("Range for", names(n_vars)[n_vars == var]),
    min = floor(ranger[1]),
    max = ceiling(ranger[2]),
    value = c(floor(ranger[1]), ceiling(ranger[2]))
  )
}