
#-------------- Define function to make leaflet map of data

map_data_leaflet <- function(data_merged, var_to_plot, display_name = ""){
  
  dat_to_plot <-  data_merged |>
      dplyr::select(c(name, network, date, longitude_deg_e, latitude_deg_n)) |>
      mutate(plot_var = pull(data_merged[, which(names(data_merged) == var_to_plot)]) ) |>
      filter(!is.na(plot_var))

  pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)

  m <- dat_to_plot |>
    leaflet() |>
    addTiles() |>
    addCircleMarkers(lng = ~longitude_deg_e, lat = ~latitude_deg_n,
                     label = paste(dat_to_plot$name, ": ",dat_to_plot$plot_var),
                     color = "grey",
                     weight = 1,
                     fillColor = ~pal(plot_var),
                     fillOpacity = 0.5 ,
                     popup = paste(dat_to_plot$name, "<br>",
                                   "Network: ", dat_to_plot$network, "<br>",
                                   dat_to_plot$date )) |>
    addLegend(values = ~plot_var,
              pal = pal,
              title = display_name) 
  
}
