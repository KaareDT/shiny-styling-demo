library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(thematic)
library(gitlink)

source("setup.R")
source("helpers.R")

thematic_shiny()

ui <- page_sidebar(

  ribbon_css(),

  theme = bs_theme(version = 5,
                   bootswatch = "darkly",
                   success = "#86C7ED", # customize colors used in theme-settings below
                   "table-color" = "#86C7ED"), # customize colors of table text

  title = "Effectiveness of DemoCo App Free Trial by Customer Profile",
  sidebar = sidebar(
    class = "bg-secondary", #change color of sidebar
    sidebar_content,
    HTML('<img src="logo.png" width="100%" height="auto">')
  ),
  layout_columns(
      card(card_header("Conversions over time",class = "text-success"),
      plotOutput("line")),

    card(card_header("Conversion rates", class = "text-success"),
                     plotOutput("bar")),

    value_box(title = "Recommended Trial",
                     textOutput("recommended_eval"),
              showcase = bs_icon("stars"),
              theme = "success"),

    value_box(title = "Users", class = "text-success",
                     textOutput("number_of_users"),
              showcase = bs_icon("people"),
              theme = "secondary"),

    value_box(title = "Avg Spend", class = "text-success",
                     textOutput("average_spend"),
              showcase = bs_icon("coin"),
              theme = "secondary"),

    card(card_header("Conversion rates by subgroup"),
                     tableOutput("table")),

    col_widths = c(8,4
                   ,4,4,4,
                   12),
    row_heights = c(4, 1.5, 3)
  )
)



# Define the Shiny server function
server <- function(input, output) {

  #bs_themer() # toggle on to get interactive themer

  # Filter data according to inputs
  selected_industries <-
    reactive(if (is.null(input$industries)) industries else input$industries)

  selected_propensities <-
    reactive(if (is.null(input$propensities)) propensities else input$propensities)

  selected_contracts <-
    reactive(if (is.null(input$contracts)) contracts else input$contracts)

  selected_data <-
    reactive({
      filter_users(selected_industries(),
                   selected_propensities(),
                   selected_contracts())
    })

  selected_data_by_group <-
    reactive({
      filter_users_by_group(selected_industries(),
                            selected_propensities(),
                            selected_contracts())
    })


  # Make plots
  output$line <-
    renderPlot({
      plot_conversions_over_time(selected_data())
    })

  output$bar <-
    renderPlot({
      plot_conversions_by_group(selected_data_by_group())
    })


  # Compute values for value boxes
  output$recommended_eval <-
    renderText({
      choose_recommendation(selected_data())
    })

  output$number_of_users <-
    renderText({
      count_users(selected_data())
    })

  output$average_spend <-
    renderText({
      compute_average_spend(selected_data())
    })


  # Render table
  output$table <-
    renderTable(digits = 0, {
      make_table(selected_data_by_group())
    })
}


# Create the Shiny app
shinyApp(ui = ui, server = server)
