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
#devtools::install_github("andypicke/rcoagmet")
library(rcoagmet)
library(leaflet)
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
ui <- fluidPage(
  
  # Application title
  titlePanel("CoAgMet Daily Viewer"),
  
  # Date input ; Default value is the date in client's time zone
  dateInput(inputId = "date_to_plot", label = "Date To View:", value = Sys.Date() - 1 , max = Sys.Date() - 1),
  
  tabsetPanel(
    tabPanel("Max Temperature",   leaflet::leafletOutput("max_temp_map")),
    tabPanel("Precipitation", leaflet::leafletOutput("precip_map")),
    tabPanel("Solar Radiation",   leaflet::leafletOutput("solarrad_map")),
    tabPanel("Data Table", DTOutput("data_table")),
    tabPanel("About", 
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
  ) # tabsetPanel
  
  
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
  
  output$max_temp_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged = data_merged(), var_to_plot = "max_temp", display_name = "Max Temperature <br> [&#176; F]")
  })
  # 
  output$precip_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged = data_merged(), var_to_plot = "precip", display_name = "Precipitation <br> [in]")
  })
  
  output$solarrad_map <- leaflet::renderLeaflet({
    map_data_leaflet(data_merged = data_merged(),  var_to_plot = "solar_rad", display_name = "Solar Radiation <br> [W/m<sup>2</sup>]")
  })
  
  
  # DataTable output
  output$data_table <- renderDT(
    {
      data_merged() |>
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
