library(shiny)
library(shinyalert)
library(bslib)
library(tidyverse)

source("helpers.R")
store_df <- readRDS("data/store_df.rds")


ui <- page_sidebar(
  title = div (
    "Superstore Data Explorer"),
sidebar = sidebar(
  width = 300,
  h3("Subset based on Category:"),
  checkboxGroupInput("Category",
                     "Category",
                     choices = levels(store_df$Category)),
  h3("Subset based on Region:"),
  checkboxGroupInput("Region",
                     "Region",
                     choices = levels(store_df$Region)),
  h3("Choose First Numeric Variable"),
  selectInput("nvar1",
              "First:",
              choices = c("None" = "none", n_vars),
              selected = "none"),
  h3("Choose the Range:"),
  uiOutput("MAKESLIDER1"),
  h3("Choose Second Numeric Variable"),
  selectInput("nvar2",
              "Second",
              choices = c("None" = "none", n_vars),
              selected = "none"
              ),
  h3("Choose the Range:"),
  uiOutput("MAKESLIDER2"),
  actionButton("subset_but",
               "Filter",
               class = "btn-primary"),
  br(),
  textOutput("n_rows")
),
navset_tab(
  nav_panel(
    "About",
    h2("About the Explorer"),
    p("This explorer is an explorer that explores."),
    h2("About the Data"),
    p("It exists."),
    h2("Instructions for Use"),
    p("Working on it.")
  )
)
)
