#--------------------------------------------------------------------------
# 
# coagmet_daily_viewer
# 
# A Shiny app to visualize/map daily weather data from CoAgMet stations in CO
#
# Andy Pickering
# andypicke@gmail.com
# 2024-06-09
#
#--------------------------------------------------------------------------

# Load libraries
library(shiny)
library(bslib)
#devtools::install_github("andypicke/rcoagmet")
library(rcoagmet)
library(leaflet)
library(leaflegend)
library(dplyr)
library(DT)
library(tidyr)

# download station metadata; only need to do this once per app session
meta_coag <- rcoagmet::get_coagmet_meta(network = "coagmet") |> 
  filter(active == "active") |>
  select(station, name, location, elevation_ft, irrigation, timestep_s, network, longitude_deg_e, latitude_deg_n)


#--------------------------------------------------------------------------
# UI
#--------------------------------------------------------------------------
ui <- 
  
  page_sidebar(
    title = "CoAgMet Daily Viewer",
    
    sidebar = sidebar(
      # Date input ; Default value is the date in client's time zone
      dateInput(inputId = "date_to_plot", 
                label = "Date To View:", 
                value = Sys.Date() - 1 , 
                max = Sys.Date() - 1),
      # select variable to plot map of
      selectInput(inputId = "plot_var", 
                  label = "Variable to Plot", 
                  choices = c("max_temp", "min_temp", "precip"))
    ),
    
    navset_card_underline(
      title = "Visualizations",
      
      # Leaflet map
      nav_panel("Plot", leaflet::leafletOutput("map")),
      
      # Data table
      nav_panel("Table", DTOutput("data_table")),
      
      # About
      nav_panel("About", 
                h3("A Shiny App to Display CoAgMet Weather Data",),
                h5("Displays daily data from the ",
                   a(href = "https://coagmet.colostate.edu/", "CoAgMet"), 
                   "weather station network"
                   ),
                h5("Data is retrieved from the CoAgMet API using the ", 
                   a(href = "https://github.com/andypicke/rcoagmet", "rcoagmet"),
                   "package" 
                   ),
                h5("Source code for the app is availabe on ",
                   a(href = "https://github.com/andypicke/coagmet_daily_viewer", "github")
                   )
                )
      )
    )




#--------------------------------------------------------------------------
# SERVER
#--------------------------------------------------------------------------
server <- function(input, output) {
  
  # download daily data for 1 day (function in /R)
  coag_daily <- reactive({
    download_daily_data(input$date_to_plot)
  })
  
  # merge the station metadata and daily data
  data_merged <- reactive({
    meta_coag |> left_join(coag_daily(), by = "station")
  })
  
  
  #--------------
  # Generate outputs (map function in /R)
  #--------------
  
  
  output$selecttext <- renderText(input$plot_var)
  
  output$map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged = data_merged(), 
                     var_to_plot = input$plot_var
    )
  })
  
  
  # DataTable output
  output$data_table <- renderDT(
    {
      data_merged() |>
        select(station, name, location, max_temp, min_temp, precip, solar_rad) |>
        datatable(
          rownames = FALSE,
          extensions = c("Responsive", "Buttons"),
          options = list(
            buttons = c("excel", "csv", "pdf"),
            dom = "Bftip"
          )
        )
    },
    server = FALSE
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
