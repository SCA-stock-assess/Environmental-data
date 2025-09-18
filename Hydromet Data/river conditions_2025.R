# Packages & functions ----------------------------------------------------

pkgs <- c(
  "tidyverse","readxl","purrr","zoo", "here", "magrittr","janitor", "mgcv",
  "rvest", "askpass"
  )
# install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(purrr)
library(janitor)
library(zoo)
library(magrittr)
library(mgcv)
library(rvest)
library(askpass)


# Current year
curr_yr <- 2025



# Load data from the online database --------------------------------------


# Log in to the database
my_session <- session("https://data.romcomm.com/") # start a session

log_in_form <- html_form(my_session)[[1]] # extract the log in form data


session_submit( # Submit a new session specifying the username and password
  my_session, 
  html_form_set(
    log_in_form,
    `ctl00$cphBody$txtUsername` = "dfoweather",
    `ctl00$cphBody$txtPass` = askpass() # Password input via dialogue box
  )
)


# List of parameters to pass to url
params <- list(
  # lid and gid, combined, lead to Sproat and Stamp hydromets, respectively
  lid = c("2022628796262", "2018510632229"),
  gid = c("0", "7"),
  # Use 365 days for time range
  days = rep(8760, 2) # see html_elements(url, "option") for available submissions
)


# Declare function to extract data from a single url 
get_data <- function(lid, gid, days) {
  path <- paste0(
    "https://data.romcomm.com/Detail.aspx?lid=",
    lid,
    "&gid=",
    gid,
    "&days=",
    days
  )
  
  url <- session_jump_to(my_session, path)
  
  table <- html_elements(url, "table") |> 
    html_table() |> 
    pluck(3)
  
  return(table)
}


# Scrape the data
latest_year_data <- pmap(params, get_data) |> 
  set_names(c("Sproat", "Stamp")) |>
  list_rbind(names_to = "station")


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
if(TRUE){ # Change to TRUE to run, otherwise this step is skipped over
  hist %>% 
    ggplot(aes(value)) +
    facet_wrap(~var, scales = "free") +
    geom_histogram(fill = "white", colour = "black")
} # empty if statement blocks this from running
# Water depths/staff gauges > 5 are probably errors


# Plot using heatmap to colour values
if(TRUE) {hist |> 
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
curr <- latest_year_data |> 
  clean_names() |> 
  rename("wtemp" = water_temperature_c) |> 
  mutate(
    depth = if_else(
      station == "Sproat",
      sensor_depthm,
      if_else(
        !is.na(levelm) & !is.na(surface_levelm),#Now checks that both levelm and surface_levelm are #present before subtracting.
        
        levelm - surface_levelm,   #Prevents Stamp depth from becoming all NA if one of those columns is missing or empty in the scraped table

        NA_real_
      )
    ),
    station_time = as.POSIXct(received, format = "%d-%b-%y %I:%M %p"),
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
    depth = if_else(!between(depth, 0.05, 5), NA_real_, depth),
  ) |> 
  # Remove most recent day's data since averages 
  # will be based on cooler overnight temps only
  filter(
    date < max(date, na.rm = T),
    # Only keep the 2025 data
    year == max(year, na.rm = T)
  ) |> 
  pivot_longer(
    c(wtemp, depth),
    names_to = "var"
  ) |> 
  select(station, station_time:last_col())



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
  # mutate(
  #   m50_smooth = stats::predict(gam(mean ~ s(doy, k = 1), family = gaussian()))
  # ) |> 
  ungroup() |> 
  drop_na(doy)




### FIGURE 3 IN THE IN-SEASON SOCKEYE BULLETIN:
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
      data = curr_sum |>
        filter(var %in% c("wtemp","depth")) |>
        mutate(
          var = recode(var, wtemp = "Water temperature (°C)", depth = "Sensor depth (m)"),
          date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
        ),
      aes(y = mean, colour = legend[2]),
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
      as.Date(paste0(curr_yr, "-04-01")), #start plot in April (04) or May (05)
      as.Date(paste0(curr_yr, "-10-15")) #end in October (10)
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
  filename = here("Hydromet Data", "plots", paste0("R-PLOT_Stamp-Sproat_Hydromets_", Sys.Date(), ".png")), #added the date to the name
  width = 6,
  height = 4.5,
  units = "in"
)





#### THIS WEEKLY AVERAGE TEMPERATURES IS PART OF THE ENVIRONMENTAL CONDITIONS BLURB
#### IN THE IN-SEASON BULLETIN:

# Weekly average temperatures
curr |>  
  distinct(station, station_time, var, .keep_all = TRUE) |> 
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


#3-day average escapement:(input into Soxsum in rows 433 & 436 about SPR and STP 3 day average water temperature)
curr |>  
  distinct(station, station_time, var, .keep_all = TRUE) |> 
  pivot_wider(
    names_from = var,
    values_from = value
  ) |> 
  filter(!(day < max(day)-3)) %>% # Keep data only from most recent 7 days
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


