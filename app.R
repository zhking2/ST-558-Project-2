library(shiny)
library(shinyalert)
library(bslib)
library(DT)
library(tidyverse)
library(shinycssloaders)

# loading the data and the helper functions
source("helpers.R")
store_df <- readRDS("data/store_df.rds")

# this section is dedicated to creating the UI
ui <- page_sidebar(
  title = div("Superstore Data Explorer"),
  theme = bs_theme(version = 5, bootswatch = "solar"),
  # the sidebar choice options should be fairly self explanatory based on the h3 titles
  sidebar = sidebar(
    width = 320,
    h3("Subset based on Category:"),
    checkboxGroupInput("Category",
                       "Category",
                       choices = levels(store_df$Category),
                       selected = levels(store_df$Category)),
    h3("Subset based on Region:"),
    checkboxGroupInput("Region",
                       "Region",
                       choices = levels(store_df$Region),
                       selected = levels(store_df$Region)),
    h3("Choose First Numeric Variable"),
    selectInput("nvar1_sub",
                "First:",
                choices = c("None" = "none", n_vars),
                selected = "none"),
    h3("Choose the Range:"),
    # this is the first slider
    uiOutput("nslide1"),
    h3("Choose Second Numeric Variable"),
    selectInput("nvar2_sub",
                "Second:",
                choices = c("None" = "none", n_vars),
                selected = "none"),
    h3("Choose the Range:"),
    # this is the second slider
    uiOutput("nslide2"),
    # this is the action button to add the filtering
    actionButton("subset_but",
                 "Filter",
                 class = "btn-primary"),
    br(),
    textOutput("n_rows")
  ),
  navset_tab(
    # the first nav panel gives a lot of background information, and includes the required image and links
    nav_panel(
      "About",
      h2("About the Explorer"),
      img(src = "sphoto.jpg", style = "max-width: 50%;"),
      p("Photo of store aisles, by lyzadanger on",
        a(
          "Wikimedia Commons",
          href = "https://commons.wikimedia.org/wiki/File:Fredmeyer_edit_1.jpg",
          target = "_blank"
        ),
        "(CC BY-SA 3.0)."),
      p("This explorer allows the user to sub-set the data based on the categorical variables Region and Category, and the Numerical Variables Product Price, Profit, Quantity, and Discount. From there, the user can create a variety of summary visuals and tables."),
      h2("About the Data"),
      p("This dataset simulates each individual item purchased from a US based superstore selling exclusively office products. It was created by Juhi Badiyani on",
        a(
          "Kaggle.",
          href = "https://www.kaggle.com/datasets/juhi1994/superstore/data",
          target = "_blank"
        ),),
      h2("Instructions for Use"),
      p("In the first sidebar, select the categories and regions you would like to view. Then, choose your two sub-setting numerical variables, and select the range on which you would like them to be sub-set."),
      p("From there, you can select either the option to download the data or use the visual explorer in the top tabs."),
      p("In the data download tab, you can either download the data as a csv file, or view it inline using the page options."),
      p("In the data exploration tab, you must select either categorical or numerical variables to explore, and then select the variables you'd like to look at, and if/how you'd like them to be faceted or grouped, and also what kind of graph you'd like. The graph and table will automatically update."),
      p("However, if you'd like to change the subsetting post-hoc, you must press the filter button again.")
      ),
    # the meat of the second panel, for the data-download, is actually located in the server section
    nav_panel(
      "Data Download",
      h3("First apply filters in the sidebar, then download data here."),
      downloadButton("download_data", "Download CSV"),
      br(),
      br(),
      DT::dataTableOutput("data_table")
    ),
    # this is the panel that actually includes the graphs and tables for the data
    # but also holds a sidebar to make selections about the graphs/tables
    nav_panel(
      "Data Exploration",
      # this sidebar allows the user to make choices about the output, starting with cat/num variables
      layout_sidebar(
        sidebar = sidebar(
          title = "Options",
          width = 250,
          position = "right",
          radioButtons(
            "var_type",
            "Variable Type",
            choices = c(
              "Numeric variables" = "n",
              "Categorical variables" = "c"
            )
          ),
          # this panel only shows up if categorical variables are selected, and allows for the user to choose variables, grouping, faceting, and graph type
          conditionalPanel(
            "input.var_type == 'c'",
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
              selected = "none"
            ),
            selectInput(
              "cfacet",
              "Faceting:",
              choices = c("None" = "none", c_vars)
            ),
            radioButtons(
              "bstyle",
              "Bar style",
              choices = c(
                "Side-by-side" = "dodge",
                "Stack" = "stack",
                "Proportions" = "fill"
              )
            )
          ),
          # this panel only shows up if numerical variables are selected, and allows for the user to choose variables, grouping, faceting, and graph type
          conditionalPanel(
            "input.var_type == 'n'",
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
              selected = "none"
            ),
            selectInput(
              "nfacet",
              "Faceting:",
              choices = c("None" = "none", c_vars)
            ),
            radioButtons(
              "gstyle",
              "Graph Type:",
              choices = c(
                "Histogram" = "histogram",
                "Scatterplot" = "scatterplot",
                "Box Plot" = "boxplot"
              )
            ),
            conditionalPanel(
              "input.gstyle == 'scatterplot'",
              selectInput(
                "sv2",
                "Secondary Numeric Variable",
                choices = n_vars,
                selected = "Sales"
              )
            )
          )
        ),
        # and here is the actual output of the graph and then the table
        h3("Numerical Summary"),
        div(style = "overflow-x: auto;", tableOutput("fintable")),
        h3("Graphical Summary"),
        withSpinner(plotOutput("finplot", height = "500px"))
      )
    )
  )
)

server <- function(input, output, session) {
  # these lines allow for the range sliders in the primary sidebar
  output$nslide1 <- renderUI(nslider("nr1", input$nvar1_sub, store_df))
  output$nslide2 <- renderUI(nslider("nr2", input$nvar2_sub, store_df))
  
  # this is the baseline for the reactive data subsetting
  subset <- reactiveValues(data = store_df)
  
  # this section does the subsetting based on the above user inputs
  observeEvent(input$subset_but, {
    filtered <- store_df
    if (!is.null(input$Category)) {
      filtered <- filtered |> filter(Category %in% input$Category)
    }
    if (!is.null(input$Region)) {
      filtered <- filtered |> filter(Region %in% input$Region)
    }
    filtered <- filtered |>
      filter_num_range(input$nvar1_sub, input$nr1) |>
      filter_num_range(input$nvar2_sub, input$nr2)
    
    subset$data <- filtered
  })
  

  output$n_rows <- renderText({
    paste("Number of rows:", nrow(subset$data))
  })
  
  
  re_data <- reactive({
    data <- subset$data
    validate(need(
      nrow(data) > 0,
      "No individual product sales match your filter criteria."
    ))
    
    if (input$var_type == "c") {
      if (input$cvar2 != "none") {
        validate(need(
          input$cvar1 != input$cvar2,
          "Chosen categorical variables must be different."
        ))
      }
    } else {
      if (input$gstyle == "scatterplot") {
        validate(need(
          input$nvar1 != input$sv2,
          "Chosen numerical variables must be different."
        ))
      }
    }
    data
  })
  
  # this section creates the final plots
  output$finplot <- renderPlot({
    data <- re_data()
    if (input$var_type == "c") {
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
        input$sv2,
        input$gvar1,
        input$nfacet
      )
    }
  })
  
  # and this section creates the final tables
  output$fintable <- renderTable(
    {
      data <- re_data()
      if (input$var_type == "c") {
        freq_table(data, input$cvar1, input$cvar2)
      } else {
        n_summary(data, input$nvar1, input$gvar1)
      }
    },
    digits = 1,
    format.args = list(big.mark = ",")
  )
  
  # and this section is what allows for the data to be downloaded in that tab
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      subset$data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) |>
      DT::formatCurrency("Sales", digits = 2)
  })
  

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("superstore_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(subset$data, file)
    }
  )
}

shinyApp(ui, server)