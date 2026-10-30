

n_vars <- c(
  "Product Price" = "Sales",
  "Profit" = "Profit",
  "Quantity" = "Quantity",
  "Discount" = "Discount")

c_vars <- c(
  "Region" = "Region",
  "Segment" = "Segment",
  "Category" = "Category")

# functions to make: one-way table, two-way table, bar-chart, summary stats, other plot types 

# frequency table, one way if one variable, two-way if two variables

freq_table <- function(data, v1, v2) {
  if (v2 == "none") {
    data |>
      group_by(v1) |>
      summarize(count = n()) }
  else {
    data |>
      group_by(v1,v2) |>
      summarize(count = n()) |>
      pivot_wider(names_from = v2, values_from = count)}}

# numerical summary

n_summary <- function(data, v1, group) {
  if (group != "none") {
    data <- data |> group_by(across(all_of(group)))}
  data |>
    summarize(
      n = n(),
      mean = mean(.data[[v1]]),
      median = median(.data[[v1]]))}

# bar plot based on inputted layout and faceting

bar_plot <- function(data, v1, v2, facet, position) {
  g <- ggplot(data, aes(x = .data[[v1]]))
  if (v2 == "none") {
    g <- g + geom_bar(fill = "blue")} 
  else {
    g <- g +
      geom_bar(aes(fill = .data[[v2]]), position = position) +
      labs(fill = var_label(var2))}
  if (facet != "none") {
    g <- g + facet_wrap(vars(.data[[facet]]))}
  g + labs(
      title = paste("Frequency of Product Sales by", as.character(v1)),
      y = if (position == "fill") "Proportion of Individual Product Sales" else "Frequency of Sales"
    ) +
    theme_minimal()}

# all the plots for the numerical variables

fancy_plot <- function(data, plot_type, v1, sv2, group, facet) {
  if (plot_type == "histogram") {
    f <- ggplot(data, aes(x = .data[[v1]]))
    f <- if (group == "none") {f + geom_histogram(bins = 100, fill = "blue")} 
         else { f + geom_histogram(aes(fill = .data[[group]]),bins = 40,position = "identity")}
    f <- f + labs(title = paste("Distribution of", var_label(v1)), y = "Number of Individual Product Sales") } 
  else if (plot_type == "boxplot") {
    f <- if (group == "none") {ggplot(data, aes(x = "All sales", y = .data[[v1]])) +
        geom_boxplot(fill = "blue")} 
         else {ggplot(data, aes(x = .data[[group]], y = .data[[v1]], fill = .data[[group]])) +
               geom_boxplot()}
    f <- f +labs(title = paste(as.character(v1),"by",if (group == "none") "all sales" else as.character(group)),
        x = if (group == "none") NULL else as.character(group),)} 
  else { data <- data |> filter(!is.na(.data[[sv2]]))
    f <- ggplot(data, aes(x = .data[[sv2]], y = .data[[v1]]))
    f <- if (group == "none") {
         f + geom_point(alpha = 0.3, color = "blue")} 
         else {
         f + geom_point(aes(color = .data[[group]]), alpha = 0.3)}
    f <- f + labs(title = paste(as.character(v1), "vs.", as.character(sv2)),)}
  if (group != "none") {
    f <- f + labs(fill = as.character(group))}
  if (facet != "none") {
    f <- f + facet_wrap(vars(.data[[facet]]))}
  f + theme_minimal()}

# slider for numerical range:

nslider <- function(id, var, data) {
  if (var == "none") {
    return(NULL)
  }
  ranger <- range(data[[var]])
  sliderInput(
    id,
    label = paste("Range for", names(n_vars)[n_vars == var]),
    min = floor(ranger[1]),
    max = ceiling(ranger[2]),
    value = c(floor(ranger[1]), ceiling(ranger[2]))
  )
}