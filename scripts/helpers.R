

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
      pivot_wider(names_from = v2, values_from = count)
  }
}



