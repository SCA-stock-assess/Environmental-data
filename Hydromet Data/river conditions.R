# Packages & functions ----------------------------------------------------

pkgs <- c("tidyverse","readxl","purrr","reshape2", "zoo","magrittr", "rvest", "XML", "RSelenium", "binman")
#install.packages(pkgs)

library(tidyverse); theme_set(theme_bw(base_size = 22))
library(readxl)
library(purrr)
library(reshape2)
library(zoo)
library(magrittr)
library(mgcv)
library(rvest)
library(httr)
library(jsonlite)
#library(RSelenium)

# Current year
curr_yr <- 2023


# Hydromet time series ----------------------------------------------------

# The historical time series
hist <- read_xlsx("./Hydromet Data/Hydromets_historic.xlsx", na = c("","-999")) %>% 
  rename(wtemp = water_temp_celcius, atemp = air_temp_celcius,
         gauge = staff_gauge_mH20, depth = sensor_depth_mH20,
         rainfall = rainfall_mm) %>% 
  mutate(across(everything(), ~str_replace(.,"[:punct:]{2}Estimated","") %>% str_trim()), # remove flags on estimated values
         across(atemp:day, as.numeric),
         # Massage the various date/time formats
         station_time = as.POSIXct(station_time, format = "%Y-%m-%d %H:%M:%S"),
         date = station_time %>% as.Date(), 
         d.m = date %>% format("%d %b"),
         doy = date %>% format("%j") %>% as.numeric(),
         # Some filters to trim bogus values from different measures
         wtemp = if_else(wtemp > 30 | wtemp < 2, NA_real_, wtemp),
         atemp = if_else(atemp < -30, NA_real_, atemp),
         depth = if_else(depth > 5 | depth < 0.05, NA_real_, depth),
         gauge = if_else(gauge < 0 | gauge > 15, NA_real_, gauge),
         rainfall = if_else(rainfall < 0, NA_real_, rainfall)) |> 
  pivot_longer(atemp:rainfall,
               names_to = "var") |> 
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
  filter(!abs(z) > 3,
         var %in% c("wtemp", "depth")) |> 
  ggplot(aes(doy, value)) +
  facet_grid(var ~ station, scales = "free_y", switch = "y") +
  stat_density_2d(aes(fill = after_stat(ndensity)), 
                  geom = "raster", 
                  contour = FALSE,
                  n = 150) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  coord_cartesian(expand = FALSE) +
  guides(fill = "none") +
  labs(y = NULL, x = "Day of year") +
  theme(strip.background.y = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, hjust = 1))
} # Set to "TRUE" to run


# The current year time series
curr <- read_xlsx(paste0("./Hydromet Data/Hydromets_", curr_yr, ".xlsx"), na = c("","-999")) %>% 
  rename(wtemp = water_temp_celcius, atemp = air_temp_celcius,
         gauge = staff_gauge_mH20, depth = sensor_depth_mH20,
         rainfall = rainfall_mm) %>% 
  mutate(across(everything(), ~str_replace(.,"[:punct:]{2}Estimated","") %>%  str_trim()), # remove flags on estimated values
         station_time = as.POSIXct(station_time),
         date = as.Date(station_time, format = "%m/%d/%Y"),
         d.m = as.Date(date) %>% format("%d %b"),
         doy = date %>% format("%j") %>% as.numeric(),
         across(c(year:day,atemp:rainfall), as.numeric),
         wtemp = case_when(
           wtemp > 30 ~ NA_real_,
           month %in% 5:10 & wtemp < 2 ~ NA_real_,
           TRUE ~ wtemp),
         atemp = if_else(atemp < -30, NA_real_, atemp),
         depth = if_else(depth > 14 | depth < 0.05, NA_real_, depth),
         gauge = if_else(gauge < 0 | gauge > 15, NA_real_, gauge),
         rainfall = if_else(rainfall < 0, NA_real_, rainfall)) %>% 
  # Remove most recent day's data since averages 
  # will be based on cooler overnight temps only
  filter(date < max(date, na.rm = T)) |> 
  pivot_longer(atemp:rainfall,
               names_to = "var") |> 
  distinct(station_time, station, var, .keep_all = TRUE) # Drop any duplicated rows


# function to summarize by date
daily_sum <- function(data) {
  data %>% 
    group_by(station, year, month, d.m, day, date, doy, var) %>% 
    summarise(mean = mean(value,na.rm = TRUE),
              q5 = quantile(value, 0.05, na.rm = TRUE),
              q95 = quantile(value, 0.95, na.rm = TRUE),
              min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE)) %>% 
    # Remove NA, NaN, Inf values 
    ungroup() %>% 
    # Remove all NaN, Inf, and NA values
    mutate(across(mean:last_col(), ~ if_else(
      is.nan(.x) | is.infinite(.x), NA_real_, as.numeric(.x)
    ))) %>% 
    # Next steps are used to calculate the 7-, 15-, and 30-day rolling averages
    group_by(station, var) %>% 
    arrange(date, .by_group = TRUE) %>% 
    mutate(across(
      mean:last_col(), 
      list(`7d` = ~rollmean(.x, k=7, fill = NA),
           `15d` = ~rollmean(.x, k=15, fill = NA),
           `30d` = ~rollmean(.x, k=30, fill = NA)), 
      .names = "{.fn}.{.col}")) %>% 
    ungroup()
}


# Save new data frames for plotting
hist_sum <- hist |> 
  filter(!abs(z) > 2.5) |> # Remove values with z > 3
  daily_sum()

curr_sum <- daily_sum(curr)

hist_doy <- hist_sum |> 
  group_by(station, doy, var) |> 
  # Calculate mean of the rolling average means, min of the q5, and max of the q95
  summarize(across(matches("\\d+d\\.mean"), ~mean(.x, na.rm = TRUE)),
            across(matches("\\d+d\\.q5"), ~min(.x, na.rm = TRUE)),
            across(matches("\\d+d\\.q95"), ~max(.x, na.rm = TRUE)),
            max = max(max),
            min = min(min),
            mean = mean(mean)) |> 
  ungroup() |> 
  drop_na(doy)


# Average graph
legend <- c(paste0("Historical average (2013-", curr_yr - 1, ")"), as.character(curr_yr))

hist_sum |> 
  filter(var %in% c("wtemp", "depth")) %>% 
  mutate(var = case_when(
    var == "wtemp" ~ "Water temperature (°C)",
    var == "depth" ~ "Sensor depth (m)",
    TRUE ~ var
  )) %>% 
  ggplot(aes(as.Date(doy + as.Date(paste0(curr_yr - 1, "-12-31"))), mean)) +
  facet_grid(var ~ station, scales = "free_y", switch = "y") +
  geom_smooth(aes(colour = legend[1]), 
              method = "loess",
              span = 0.5,
              se = FALSE,
              linewidth = 0.5) +
  geom_ribbon(data = hist_doy |> 
                filter(var %in% c("wtemp", "depth")) %>% 
                mutate(var = case_when(
                  var == "wtemp" ~ "Water temperature (°C)",
                  var == "depth" ~ "Sensor depth (m)",
                  TRUE ~ var
                ),
                date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy) |> 
                group_by(var, station) |> 
                mutate(ymin_smooth = stats::predict(loess(min ~ doy, span = 0.5)),
                       ymax_smooth = stats::predict(loess(max ~ doy, span = 0.5))),
              aes(ymin = ymin_smooth,
                  ymax = ymax_smooth),
              alpha = 0.4,
              fill = "blue") +
  geom_line(data = curr_sum %>% 
              #filter(between(day, 136, 273)) %>% # Option to restrict date range of graph
              filter(var %in% c("wtemp","depth")) %>% 
              mutate(var = case_when(
                var == "wtemp" ~ "Water temperature (°C)",
                var == "depth" ~ "Sensor depth (m)",
                TRUE ~ var
              ),
              date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy), 
            aes(y = `7d.mean`, colour = legend[2]), 
            linewidth = 1.15) +
  labs(y = NULL, x = NULL) +
  #scale_y_continuous(limits = c(0,30), breaks = seq(0,25, by = 5)) +
  scale_colour_manual("", 
                      values = set_names(c("blue", "red"), legend)) +
  scale_x_date(date_labels = "%b", 
               breaks = seq.Date(as.Date(paste0(curr_yr, "-01-01")), 
                                 as.Date(paste0(curr_yr, "-12-31")), 
                                 by = "month"),
               expand = c(0,0)) + 
  guides(fill = "none") +
  coord_cartesian(xlim = c(as.Date(paste0(curr_yr, "-05-01")), 
                           as.Date(paste0(curr_yr, "-10-15")))) +
  theme(legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(colour="black"),
        strip.background.y = element_blank(),
        strip.background.x = element_rect(fill = "white", colour = "black"),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Weekly average temperatures
curr %>% 
  pivot_wider(names_from = var,
              values_from = value) |> 
  filter(!(day < max(day)-7)) %>% # Keep data only from most recent 7 days
  group_by(station) %T>%
  # options arguments below disable warnings associated with averaging date-class variables
  {options(warn = -1)} %>% 
  summarize(across(c(wtemp,date), 
                   .fns = list(mean = mean, min = min, max = max), 
                   .names = "{.fn}_{.col}",
                   na.rm = TRUE)) %T>%
  {options(warn = 0)} %>% 
  select(-mean_date)




# Individual year comparisons ---------------------------------------------

# 2023 compared to 2021 and 2015 (other warm years)
hist_sum |> 
  filter(var %in% c("wtemp", "depth"),
         year %in% c(2015, 2021)) %>% 
  mutate(var = case_when(
    var == "wtemp" ~ "Water temperature (°C)",
    var == "depth" ~ "Sensor depth (m)",
    TRUE ~ var
  )) %>% 
  ggplot(aes(x = as.Date(doy + as.Date(paste0(curr_yr - 1, "-12-31"))), y = `7d.mean`)) +
  facet_grid(var ~ station, scales = "free_y", switch = "y") +
  geom_line(aes(colour = as.factor(year))) +
  geom_line(data = curr_sum %>% 
              #filter(between(day, 136, 273)) %>% # Option to restrict date range of graph
              filter(var %in% c("wtemp","depth")) %>% 
              mutate(var = case_when(
                var == "wtemp" ~ "Water temperature (°C)",
                var == "depth" ~ "Sensor depth (m)",
                TRUE ~ var
              ),
              date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy), 
            aes(y = `7d.mean`, colour = legend[2]), 
            linewidth = 1.15) +
  labs(y = NULL, x = NULL) +
  #scale_y_continuous(limits = c(0,30), breaks = seq(0,25, by = 5)) +
  #scale_colour_manual("", 
  #                    values = set_names(c("blue", "red"), legend)) +
  scale_x_date(date_labels = "%b", 
               breaks = seq.Date(as.Date(paste0(curr_yr, "-01-01")), 
                                 as.Date(paste0(curr_yr, "-12-31")), 
                                 by = "month"),
               expand = c(0,0)) + 
  guides(fill = "none") +
  coord_cartesian(xlim = c(as.Date(paste0(curr_yr, "-05-01")), 
                           as.Date(paste0(curr_yr, "-10-15")))) +
  theme(legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(colour="black"),
        strip.background.y = element_blank(),
        strip.background.x = element_rect(fill = "white", colour = "black"),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Pilot web scraping to read data directly --------------------------------

url <- "http://www.pacfish.ca/wcviweather/"

session <- session(url)
pg_form <- html_form(session)[[1]]

test <- read_html(url)

node <- html_elements(test, "a") |> 
  html_attr("href") |> 
  str_subset("Sproat|Stamp")

paste0(url, node[1]) |> 
  read_html() |> 
  html_elements("div") |> 
  html_table()


# Suggested from stack overflow
start_date <- as.Date('2023-05-01')
end_date <- Sys.Date()

pair_ids <- session |> 
  html_elements("input") |> 
  html_attr("id")


# Try RSelenium
driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever =
                                system2(command = "wmic",
                                        args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                        stdout = TRUE,
                                        stderr = TRUE) %>%
                                stringr::str_subset(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.\\d+") |> 
                                str_extract("\\d+\\.\\d+\\.\\d+\\.\\d+")
                              #stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                              #magrittr::extract(!is.na(.)) #%>%
                              #stringr::str_replace_all(pattern = "\\.",
                              #                         replacement = "\\\\.") %>%
                              #paste0("^",  .) %>%
                              #stringr::str_subset(string =
                              #                      binman::list_versions("chromedriver") %>%
                              #                      dplyr::last()) %>%
                              #as.numeric_version() %>%
                              #max() %>%
                              #as.character())
)
remote_driver <- driver[["client"]]
remote_driver$open()

remote_driver$navigate(url)

