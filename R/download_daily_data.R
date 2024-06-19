# function to download daily data for CoAgMet stations and clean up
download_daily_data <- function(wh_date){
  
  df <- rcoagmet::get_coagmet_data(station_id = "all", 
                                   time_step = "daily", 
                                   date_from = wh_date, 
                                   date_to = wh_date, 
                                   network = "coagmet") |>
    select(station, date, avg_temp, max_temp, min_temp, solar_rad, precip, rh_max, rh_min) |>
    drop_na() |>
    mutate(rh_max = 100*rh_max, 
           rh_min = 100*rh_min)
  
}