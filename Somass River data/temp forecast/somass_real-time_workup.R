# Packages ----------------------------------------------------------------


# Install packages
pkgs <- c("here", 'tidyverse', "readxl", "janitor", "httr", "rvest")
#install.packages(pkgs)

# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(janitor)
library(httr)
library(rvest)



# Load real-time data from ECCC website -----------------------------------

# URL parameters
station_id = "08HB017"
end_date <- Sys.Date()
start_date <- paste0(curr_yr, "-06-01")
parameter <- 5 # 5 is water temperature; 46 = level, 47 = discharge


# url with the real-time data
url <- paste0(
  "https://wateroffice.ec.gc.ca/services/real_time_data/csv/inline",
  "?stations[]=", station_id,
  "&parameters[]=", parameter,
  # Start & end dates and times
  "&start_date=", start_date, "%2000:00:00",
  "&end_date=", end_date, "%2023:59:59"
)


# Get content
real_time <- httr::GET(url) |> 
  content() |> 
  clean_names() |> 
  rename_with(~str_remove_all(.x, "_.*"))


# Air temperature data ----------------------------------------------------


# URL parameters
air_temps <- expand_grid(
  # Stipulate parameters to feed into the URLs to be scraped
  weather_station = 8045,
  year = curr_yr,
  month = 6:as.numeric(format(Sys.Date(), "%m")),
  day = 1:31
) |> 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
  # Constrain dates to the range present the hydrometric data
  filter(between(date, min(real_time$date), Sys.Date() - 1)) |>
  mutate(
    # Generate list of URLs to scrape data from 
    url = paste0(
      "https://climate.weather.gc.ca/climate_data/hourly_data_e.html",
      "?StationID=", weather_station,
      "&searchMethod=contains",
      "&Month=", month,
      "&Day=", day,
      "&timeframe=", 1, # 1 = hourly, 2 = daily, 3 = monthly
      "&Year=", year
    ),
    # Iterate through the URLs and store the tables in data frames
    data = map(
      .x = url,
      ~ .x |> 
        read_html() |> 
        html_nodes("table") |> 
        html_table() |> 
        list_rbind(),
      .progress = "Data scraping progress"
    )
  ) |> 
  unnest(data) |> 
  select(-url) |> 
  clean_names() |> 
  # Create a date variable that includes hourly data
  mutate(
    dtt = paste0(year, "-", month, "-", day, " ", timelst) |> 
      as.POSIXct(format = "%Y-%m-%d %H:%M")
  )

# Note that it would be much faster to do this using the bulk download function described at:
# https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Cygwin/DataDownloadOptionsDocument_Cygwin_EN.pdf
# I just wasn't able to figure out how to tweak the code from Cygwin to R. 

  

# Add air temperature measurements to water temperature readings ----------


# Combined data
combined_temps <- real_time |> 
  select(date, value) |> 
  left_join(select(air_temps, date, temp_definition_c)) |> 
  rename(
    "wtemp" = value,
    "atemp" = temp_definition_c
  ) 
  



# Plots comparing recorded air to water temperatures ----------------------


# With no time lagging
combined_temps |> 
  pivot_longer(
    contains("temp"),
    names_to = "measurement"
  ) |> 
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(colour = measurement)) +
  labs(
    x = NULL,
    y = "Temperature (C)"
  ) 
# Probably not worth looking into further until more data become available;
# no obvious relationship is apparent
