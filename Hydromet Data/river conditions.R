# Packages & functions ----------------------------------------------------

pkgs <- c(
  "tidyverse","readxl","purrr","zoo", "here", "magrittr","janitor", "mgcv")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(purrr)
library(janitor)
library(zoo)
library(magrittr)
library(mgcv)


# Current year
curr_yr <- 2024


# Hydromet time series ----------------------------------------------------


# The historical time series
hist <- read_xlsx(here("Hydromet Data", "Hydromets_historic.xlsx"), na = c("","-999")) %>% 
  rename(
    wtemp = water_temp_celcius, 
    atemp = air_temp_celcius,
    gauge = staff_gauge_mH20, 
    depth = sensor_depth_mH20,
    rainfall = rainfall_mm
  ) %>% 
  mutate(
    across(everything(), ~str_replace(.,"[:punct:]{2}Estimated","") %>% str_trim()), # remove flags on estimated values
    across(atemp:day, as.numeric),
    # Massage the various date/time formats
    station_time = as.POSIXct(station_time, format = "%Y-%m-%d %H:%M:%S"),
    date = station_time %>% as.Date(), 
    d.m = date %>% format("%d %b"),
    doy = date %>% format("%j") %>% as.numeric(),
    # Some filters to trim bogus values from different measures
    wtemp = if_else(!between(wtemp, 2, 30), NA_real_, wtemp),
    atemp = if_else(!between(atemp, -30, 50), NA_real_, atemp),
    depth = if_else(!between(depth, 0.05, 5), NA_real_, depth),
    gauge = if_else(!between(gauge, 0, 15), NA_real_, gauge),
    rainfall = if_else(!between(rainfall, 0, 10), NA_real_, rainfall)
  ) |> 
  pivot_longer(atemp:rainfall, names_to = "var") |> 
  group_by(station, doy, var) |> 
  mutate(z = scale(value)[,1])


# Some histograms to check for any remaining extreme values.
# Look at all measures at once
if(FALSE){ # Change to TRUE to run, otherwise this step is skipped over
  hist %>% 
    ggplot(aes(value)) +
    facet_wrap(~var, scales = "free") +
    geom_histogram(fill = "white", colour = "black")
} # empty if statement blocks this from running
# Water depths/staff gauges > 5 are probably errors


# Plot using heatmap to colour values
if(FALSE) {hist |> 
    filter(
      !abs(z) > 3,
      var %in% c("wtemp", "depth")
    ) |> 
    ggplot(aes(doy, value)) +
    facet_grid(
      var ~ station, 
      scales = "free_y", 
      switch = "y"
    ) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)), 
      geom = "raster", 
      contour = FALSE,
      n = 150
    ) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    coord_cartesian(expand = FALSE) +
    guides(fill = "none") +
    labs(y = NULL, x = "Day of year") +
    theme(
      strip.background.y = element_blank(),
      strip.placement = "outside",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
} # Set to "TRUE" to run


# The current year time series
curr_file <- here("Hydromet Data", "Hydromets_current.xlsx")
curr <- curr_file |> 
  excel_sheets() |> 
  purrr::set_names() |> 
  map(read_xlsx, path = curr_file) |> 
  map(~mutate(.x, across(everything(), as.character))) |> 
  list_rbind(names_to = "station") |> 
  clean_names() |> 
  rename(
    wtemp = water_temp_c, 
    atemp = air_temp_c,
    gauge = staff_gauge_m_h20, 
    depth = sensor_depth_m_h20
  ) %>% 
  mutate(
    # remove flags on estimated values
    across(everything(), ~str_replace(.,"[:punct:]{2}Estimated","") %>% str_trim()),
    across(everything(), parse_guess),
    station_time = as.POSIXct(time),
    year = format(station_time, "%Y") |> as.numeric(),
    month = format(station_time, "%m") |> as.numeric(),
    day = format(station_time, "%j") |> as.numeric(),
    date = as.Date(station_time, format = "%m/%d/%Y"),
    d.m = as.Date(date) %>% format("%d %b"),
    doy = date %>% format("%j") %>% as.numeric(),
    wtemp = case_when(
      !between(wtemp, 2, 30) ~ NA_real_, 
      month %in% 5:10 & wtemp < 2 ~ NA_real_,
      TRUE ~ wtemp
    ),
    atemp = if_else(!between(atemp, -30, 50), NA_real_, atemp),
    depth = if_else(!between(depth, 0.05, 5), NA_real_, depth),
    gauge = if_else(!between(gauge, 0, 15), NA_real_, gauge),
  ) %>% 
  # Remove most recent day's data since averages 
  # will be based on cooler overnight temps only
  filter(date < max(date, na.rm = T)) |> 
  pivot_longer(
    c(atemp, wtemp, gauge, depth),
    names_to = "var"
  ) |> 
  distinct(station_time, station, var, .keep_all = TRUE) # Drop any duplicated rows


# function to summarize by date
daily_sum <- function(data) {
  data %>% 
    ungroup() |> 
    summarise(
      .by = c(station, year, month, d.m, day, date, doy, var),
      mean = mean(value,na.rm = TRUE),
      q5 = quantile(value, 0.05, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    ) %>% 
    # Remove all NaN, Inf, and NA values
    mutate(
      across(
        mean:last_col(), 
        ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, as.numeric(.x))
      )
    ) %>% 
    # Next steps are used to calculate the 7-, 15-, and 30-day rolling averages
    group_by(station, var) %>% 
    arrange(date, .by_group = TRUE) %>% 
    mutate(
      across(
        mean:last_col(), 
        list(`7d` = ~rollmean(.x, k=7, fill = NA),
             `15d` = ~rollmean(.x, k=15, fill = NA),
             `30d` = ~rollmean(.x, k=30, fill = NA)), 
        .names = "{.fn}.{.col}")
    ) %>% 
    ungroup()
}


# Save new data frames for plotting
hist_sum <- hist |> 
  filter(!abs(z) > 3) |> # Remove values with z > 3
  daily_sum()

curr_sum <- daily_sum(curr)

hist_doy <- hist_sum |> 
  group_by(station, doy, var) |> 
  # Calculate mean of the rolling average means, min of the q5, and max of the q95
  summarize(
    across(matches("\\d+d\\.mean"), ~mean(.x, na.rm = TRUE)),
    across(matches("\\d+d\\.q5"), ~min(.x, na.rm = TRUE)),
    across(matches("\\d+d\\.q95"), ~max(.x, na.rm = TRUE)),
    max = max(max),
    min = min(min),
    mean = mean(mean)
  ) |> 
  ungroup() |> 
  drop_na(doy)


# Average graph
legend <- c(paste0("Historical average (2013-", curr_yr - 1, ")"), as.character(curr_yr))

(comp_plot <- hist_sum |> 
  filter(var %in% c("wtemp", "depth")) %>% 
  mutate(
    var = case_when(
      var == "wtemp" ~ "Water temperature (°C)",
      var == "depth" ~ "Sensor depth (m)",
      TRUE ~ var
    )
  ) %>% 
  ggplot(aes(as.Date(doy + as.Date(paste0(curr_yr - 1, "-12-31"))), mean)) +
  facet_grid(
    var ~ station, 
    scales = "free_y", 
    switch = "y"
  ) +
  geom_smooth(
    aes(colour = legend[1]), 
    method = "loess",
    span = 0.5,
    se = FALSE,
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = hist_doy |> 
      filter(var %in% c("wtemp", "depth")) %>% 
      mutate(
        var = case_when(
          var == "wtemp" ~ "Water temperature (°C)",
          var == "depth" ~ "Sensor depth (m)",
          TRUE ~ var
        ),
        date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
      ) |> 
      group_by(var, station) |> 
      mutate(
        ymin_smooth = stats::predict(loess(min ~ doy, span = 0.5)),
        ymax_smooth = stats::predict(loess(max ~ doy, span = 0.5))
      ),
    aes(ymin = ymin_smooth, ymax = ymax_smooth),
    alpha = 0.4,
    fill = "blue"
  ) +
  geom_line(
    data = curr_sum %>% 
      #filter(between(day, 136, 273)) %>% # Option to restrict date range of graph
      filter(var %in% c("wtemp","depth")) %>% 
      mutate(
        var = case_when(
          var == "wtemp" ~ "Water temperature (°C)",
          var == "depth" ~ "Sensor depth (m)",
          TRUE ~ var
        ),
        date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
      ), 
    aes(y = `7d.mean`, colour = legend[2]), 
    linewidth = 1.15
  ) +
  labs(y = NULL, x = NULL) +
  #scale_y_continuous(limits = c(0,30), breaks = seq(0,25, by = 5)) +
  scale_colour_manual(
    "", 
    values = set_names(c("blue", "red"), legend)
  ) +
  scale_x_date(
    date_labels = "%b", 
    breaks = seq.Date(
      as.Date(paste0(curr_yr, "-01-01")), 
      as.Date(paste0(curr_yr, "-12-31")), 
      by = "month"
    ),
    expand = c(0,0)
  ) + 
  guides(fill = "none") +
  coord_cartesian(
    xlim = c(
      as.Date(paste0(curr_yr, "-05-01")), 
      as.Date(paste0(curr_yr, "-10-15"))
    )
  ) +
  theme(
    legend.position = c(0.5,0.9),
    legend.direction = "horizontal",
    legend.background = element_rect(colour="black"),
    strip.background.y = element_blank(),
    strip.background.x = element_rect(fill = "white", colour = "black"),
    strip.placement = "outside",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
)
  

# Save the plot
ggsave(
  plot = comp_plot,
  here("Hydromet Data", "plots", "R-PLOT_Stamp-Sproat_Hydromets.png"),
  width = 6,
  height = 4.5,
  units = "in"
)


# Weekly average temperatures
curr %>% 
  pivot_wider(
    names_from = var,
    values_from = value
  ) |> 
  filter(!(day < max(day)-7)) %>% # Keep data only from most recent 7 days
  group_by(station) %T>%
  # options arguments below disable warnings associated with averaging date-class variables
  {options(warn = -1)} %>% 
  summarize(
    across(c(wtemp,date), 
           .fns = list(mean = mean, min = min, max = max), 
           .names = "{.fn}_{.col}",
           na.rm = TRUE)
  ) %T>%
  {options(warn = 0)} %>% 
  select(-mean_date)




# Individual year comparisons ---------------------------------------------


# Compared to 2021 and 2015 (other warm years)
hist_sum |> 
  filter(
    var %in% c("wtemp", "depth"),
    year %in% c(2015, 2021)
  ) %>% 
  mutate(
    var = case_when(
      var == "wtemp" ~ "Water temperature (°C)",
      var == "depth" ~ "Sensor depth (m)",
      TRUE ~ var
    )
  ) %>% 
  ggplot(aes(x = as.Date(doy + as.Date(paste0(curr_yr - 1, "-12-31"))), y = `7d.mean`)) +
  facet_grid(
    var ~ station, 
    scales = "free_y", 
    switch = "y"
  ) +
  geom_line(aes(colour = as.factor(year))) +
  geom_line(
    data = curr_sum %>% 
      #filter(between(day, 136, 273)) %>% # Option to restrict date range of graph
      filter(var %in% c("wtemp","depth")) %>% 
      mutate(var = case_when(
        var == "wtemp" ~ "Water temperature (°C)",
        var == "depth" ~ "Sensor depth (m)",
        TRUE ~ var
      ),
      date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy), 
    aes(y = `7d.mean`, colour = legend[2]), 
    linewidth = 1.15
  ) +
  labs(y = NULL, x = NULL) +
  #scale_y_continuous(limits = c(0,30), breaks = seq(0,25, by = 5)) +
  #scale_colour_manual("", 
  #                    values = set_names(c("blue", "red"), legend)) +
  scale_x_date(
    date_labels = "%b", 
    breaks = seq.Date(as.Date(paste0(curr_yr, "-01-01")), 
                      as.Date(paste0(curr_yr, "-12-31")), 
                      by = "month"),
    expand = c(0,0)
  ) + 
  guides(fill = "none") +
  coord_cartesian(
    xlim = c(as.Date(paste0(curr_yr, "-05-01")), as.Date(paste0(curr_yr, "-10-15")))
  ) +
  theme(
    legend.position = c(0.5,0.9),
    legend.direction = "horizontal",
    legend.background = element_rect(colour="black"),
    strip.background.y = element_blank(),
    strip.background.x = element_rect(fill = "white", colour = "black"),
    strip.placement = "outside",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Pull data from Water Survey of Canada gauge at Papermill Dam -------------------------

library(httr)
library(jsonlite)
library(dplyr)

# Base URL for the API
base_url <- "https://api.weather.gc.ca/collections/hydrometric-daily-mean/items"

# Initialize parameters
limit <- 500
offset <- 0
station_name <- "somass"

# Initialize an empty data frame to store the results
pmd_data <- data.frame()

while (TRUE) {
  # Construct the query URL with the current offset
  query_url <- paste0(base_url, "?limit=", limit, "&offset=", offset, "&STATION_NAME=", station_name)
  
  # Send the GET request
  response <- GET(query_url)
  
  # Check if the request was successful
  if (http_status(response)$category != "Success") {
    print("Failed to retrieve data")
    break
  }
  
  # Parse the JSON content
  content <- content(response, "text")
  data <- fromJSON(content, flatten = TRUE)
  
  # Check if there's data in the response
  if (length(data$features) == 0) {
    print("No more data to retrieve")
    break
  }
  
  # Extract the records
  records <- data$features
  
  # Append the records to the all_data data frame
  pmd_data <- bind_rows(pmd_data, records)
  
  # Update the offset for the next request
  offset <- offset + limit
}


# Clean up the data
pmd_data <- pmd_data |> 
  rename_with(~str_remove(.x, "geometry|properties")) |> 
  janitor::clean_names() |> 
  unnest_wider(col = coordinates, names_sep = "_") |> 
  rename(
    "latitude" = coordinates_1,
    "longitude" = coordinates_2
  )



# Save the data as a .csv file
write.csv(
  pmd_data,
  file = here(
    "Hydromet Data",
    paste0(
      "SomassGaugeData_WaterOffice_",
      Sys.Date(),
      ".csv"
    )
  ),
  row.names = FALSE
)


# Remove all versions except the latest version of the download
old_versions <- list.files(
  here("Hydromet Data"),
  pattern = "WaterOffice",
  full.names = TRUE
) |> 
  as_tibble_col(column_name = "filename") |>
  mutate(
    date = str_extract(filename, "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}(?=\\.csv)") |> 
      as.Date()
    ) |> 
  arrange(date) |> 
  slice(-1) |> # Remove the first (most recent) file
  pull(filename)


# Delete the old files
file.remove(old_versions)



# Load Somass gauge data and plot -----------------------------------------


# Read the latest data
somass_data <- list.files(
  here("Hydromet Data"),
  pattern = "WaterOffice",
  full.names = TRUE
) |> 
read.csv() |> 
  mutate(
    date = as.Date(date),
    year = format(date, "%Y") |> as.numeric(),
    month = format(date, "%m"),
    doy = format(date, "%j") |> as.numeric()
  )


# Historical average (by day) data for Somass
somass_hist <- somass_data |> 
  pivot_longer(
    cols = c(discharge), # Add additional variables here if desired,
    names_to = "measure",
    values_to = "value"
  ) |> 
  summarize(
    .by = c(doy, measure),
    mean = mean(value,na.rm = TRUE),
    q5 = quantile(value, 0.05, na.rm = TRUE),
    q95 = quantile(value, 0.95, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  ) |> 
  arrange(measure, doy) |> 
  mutate(
    .by = measure,
    across(
      mean:last_col(), 
      list(`7d` = ~rollmean(.x, k=7, fill = NA),
           `15d` = ~rollmean(.x, k=15, fill = NA),
           `30d` = ~rollmean(.x, k=30, fill = NA)), 
      .names = "{.fn}_{.col}"),
    date = as.Date(doy, origin = paste0(curr_yr-1, "-12-31"))
  )


# See what it looks like
ggplot(somass_hist, aes(x = date, y = `15d_mean`)) +
  facet_grid(~measure) +
  geom_line(colour = "blue") +
  geom_ribbon(
    aes(
      ymin = `15d_q5`,
      ymax = `15d_q95`
    ),
    fill = "blue",
    alpha = 0.4
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = c(
      as.Date(paste0(curr_yr, "-01-01")),
      as.Date(paste0(curr_yr, "-12-31"))
    ),
    expand = c(0, 0)
  )

