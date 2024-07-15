
#-------------- Define function to make leaflet map of data

map_data_leaflet <- function(data_merged, var_to_plot){
  
  if (var_to_plot == "max_temp") {
    display_name <- "Max Temperature <br> [&#176; F]"
  } else if (var_to_plot == "min_temp") {
    display_name <- "Min Temperature <br> [&#176; F]"
  } else if (var_to_plot == "precip") {
    display_name <- "Precipitation <br> [in]"
  } else {
    display_name <- var_to_plot
  }
  
  dat_to_plot <-  data_merged |>
    dplyr::select(c(name, network, date, longitude_deg_e, latitude_deg_n)) |>
    mutate(plot_var = pull(data_merged[, which(names(data_merged) == var_to_plot)]) ) |>
    filter(!is.na(plot_var))
  
  if (var_to_plot == "precip") {
    pal <- colorNumeric(palette = "Blues", domain = dat_to_plot$plot_var)
  } else {
    pal <- colorNumeric(palette = "YlOrRd", domain = dat_to_plot$plot_var)
  }
  
  
  # make labels to show on hover
  # see https://stackoverflow.com/questions/30964020/popup-when-hover-with-leaflet-in-r
  labs <- as.list(paste(dat_to_plot$name, "<br>",
                        "Network: ", dat_to_plot$network, "<br>",
                        dat_to_plot$date, "<br>",
                        var_to_plot, " : ", dat_to_plot$plot_var)
  )
  
  
  
  m <- dat_to_plot |>
    leaflet() |>
    addTiles() |>
    addCircleMarkers(lng = ~longitude_deg_e, lat = ~latitude_deg_n,
                     label = lapply(labs,HTML),
                     color = "grey",
                     weight = 1,
                     fillColor = ~pal(plot_var),
                     fillOpacity = 0.5) |>
    addLegend(values = ~plot_var,
              pal = pal,
              title = display_name,) 
  
}
