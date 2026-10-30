library(shiny)
library(shinyalert)
library(bslib)
library(DT)
library(tidyverse)

source("scripts/helpers.R")
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
            "Box Plot" = "boxplot"
            )),
        conditionalPanel(
              "input.num_plot == 'scatterplot'",
              selectInput(
                "sv2",
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
  # input sliders for the subset tab
  output$nslide1 <- renderUI(nslider ("nr1", input$nvar1, store_df ))
  output$nslide2 <- renderUI(nslider ("nr2", input$nvar2, store_df ))
  
  # creating the reactive values button for subsetting
  subset <- reactiveValues(data = store_df)
  observeEvent(input$subset_but, {
    subset$data <- store_df |>
      filter(Category %in% input$Category, Region %in% input$Region) |>
      filter_num_range(input$nvar1, input$nslide1) |>
      filter_num_range(input$nvar1, input$nslide2)
  })
  
  # 
  re_data <- reactive({
    data <- subset_vals$data
    validate(need(
      nrow(data) > 0,
      "No individual product sales match."))
    
    if (input$summary_type == "cat") {
      validate(need(
        input$cat_var1 != input$cat_var2,
        "Chosen categorical variables must be different"))}
      if (input$num_plot == "scatter") {
        validate(need(
          input$scatter_x != input$num_var,
          "Chosen numerical variables must be different."))}}
    data)

  output$findplot <- renderPlot({
    data <- re_data()
    if (input$summary_type == "cat") {
      bar_plot(
        data,
        input$cvar1,
        input$cvar2,
        input$cfacet,
        input$bstyle
      )
    } else {
      fancy_plot(
        data,
        input$gstyle,
        input$nvar1,
        input$scaty,
        input$gvar1,
        input$nfacet
      )
    }
  })
  output$fintable <- renderTable(
    {
      data <- re_data()
      if (input$summary_type == "c") {
        freq_table(data, input$cvar1, input$cvar2)
      } else {
        n_summary(data, input$nvar1, input$gvar1)
      }
    },
    digits = 1,
    format.args = list(big.mark = ",")
  )
  
  # Data table of the (possibly subsetted) data
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      subset_vals$data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) |>
      DT::formatCurrency("price", digits = 0)
  })
  
  # Save the (possibly subsetted) data as a CSV file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("melbourne_housing_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(subset_vals$data, file)
    }
  )
  
  
}

shinyApp(ui, server)