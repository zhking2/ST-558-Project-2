library(shiny)
library(shinyalert)
library(bslib)
library(DT)
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
  uiOutput("nslide1"),
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
    img(src = "documents/sphoto.jpg", style = "max-width: 100%;"),
    p("Photo of store aisles, by lyzadanger on",
      a(
        "Wikimedia Commons",
        href = "https://commons.wikimedia.org/wiki/File:Fredmeyer_edit_1.jpg",
        target = "_blank"
      ),
      "(CC BY-SA 3.0)."),
    p("This explorer is an explorer that explores."),
    h2("About the Data"),
    p("It exists."),
    h2("Instructions for Use"),
    p("Working on it.")
  ),
  nav_panel(
    "Data Download",
    h3("First apply filters in the sidebar, then download data here."),
    downloadButton("download_data", "Download CSV"),
    br(),
    br(),
    DT::dataTableOutput("data_table")),
  nav_panel(
    "Data Exploration",
    layout_sidebar(
      sidebar = sidebar(
      title = "Options",
      width = 200,
      radioButtons(
        "var_type",
        "Variable Type",
        choices = c(
          "Numeric variables" = "n",
          "Categorical variables" = "c")),
      
      
      conditionalPanel(
        "input.summary_type == 'c'",
        selectInput(
          "cvar1",
          "First Variable",
          choices = c_vars,
          selected = "Region"
        ),
        selectInput(
          "cvar2",
          "Second variable:",
          choices = c("None" = "none", c_vars),
          selected = "none"),
        selectInput(
          "cfacet",
          "Faceting:",
          choices = c("None" = "none", c_vars)),
        radioButtons(
          "bstyle",
          "Bar style",
          choices = c(
            "Side-by-side" = "dodge",
            "Stack" = "stack",
            "Proportions" = "fill"))),
      
      
      conditionalPanel(
        "input.summary_type == 'n'",
        selectInput(
          "nvar1",
          "Primary Numeric Variable:",
          choices = n_vars,
          selected = "Profit"
        ),
        selectInput(
          "gvar1",
          "Categorical Grouping:",
          choices = c("None" = "none", c_vars),
          selected = "none"),
        selectInput(
          "nfacet",
          "Faceting:",
          choices = c("None" = "none", c_vars)),
        radioButtons(
          "gstyle",
          "Graph Type:",
          choices = c(
            "Histogram" = "histogram",
            "Scatterplot" = "scatterplot",
            "Box Plot" = "boxplot",
            "Violin Plot" = "violin"
            )),
        conditionalPanel(
              "input.num_plot == 'scatterplot'",
              selectInput(
                "scaty",
                "Secondary Numeric Variable",
                choices = n_vars,
                selected = "distance"))))),
      
      withSpinner(plotOutput("finplot", height = "500px")),
      h3("Numerical Summary"),
      div(style = "overflow-x: auto;", tableOutput("fintable"))
    )
  )
)

server <- function(input, output, session) {
  output$nslide1 <- renderUI()