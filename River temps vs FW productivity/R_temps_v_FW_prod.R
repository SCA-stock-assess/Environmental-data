# Packages ----------------------------------------------------------------


pkgs <- c("here", "tidyverse", "readxl")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw())
library(readxl)


# Load and filter spawner-smolt model posterior ---------------------------


# Posterior estimates of spawner-smolt residuals from Bev-Holt
ss_post <- here(
  "River temps vs FW productivity",
  "Barkley_Sockeye_spawner-smolt_residuals_summary.csv"
) |> 
  read.csv() |> 
  rename_with(\(x) str_remove_all(x, "X|\\.$")) |> 
  rename_with(\(x) paste0("q_", x), .cols = matches("^\\d")) |> 
  filter(
    cu %in% c("Great Central", "Sproat"),
    Rmeas == "BYO"
  )


# Load available river temperature data -----------------------------------


# Hydrometric data from Stamp & Sproat rivers
hydat <- here(
  "Hydromet data",
  "Hydromets_historic - Updated.xlsx"
) |> 
  read_xlsx() |> 
  mutate(system = if_else(station == "Stamp", "Great Central", station))


# Additional data compiled by Howard Stiff
spot_t <- here(
  "River temps vs FW productivity",
  "Spot and Mean Daily Water Temp Data At Depth.csv"
) |> 
  read.csv() |> 
  filter(
    str_detect(
      Location,
      "(Somass|Sproat|Stamp) River"
    )
  ) |> 
  mutate(
    system = case_when(
      str_detect(Location, "Stamp") ~ "Great Central",
      str_detect(Location, "Sproat") ~ "Sproat",
      str_detect(Location, "Somass") ~ "Somass"
    ),
    date = as.Date(paste0(str_extract(Date, ".*-.*-"), Year), format = "%d-%b-%Y")
  ) |> 
  select(
    system, date, min_temp = MinWaterT, mean_temp = MeanWaterT, 
    max_temp = MaxWaterT, n = CountWaterT     
  ) 

# Ensure all dates are represented in the data, including as NAs
all_dates <- seq.Date(
  min(spot_t$date, as.Date(hydat$station_time), na.rm = TRUE),
  max(spot_t$date, as.Date(hydat$station_time), na.rm = TRUE),
  by = "1 day"
) |> 
  as_tibble_col(column_name = "date") |> 
  expand_grid(system = unique(hydat$system))


# Join the two data series
temps <- hydat |> 
  mutate(date = as.Date(station_time)) |> 
  summarize(
    .by = c(system, date),
    n = n(),
    min_temp = min(water_temp_celcius),
    mean_temp = mean(water_temp_celcius),
    max_temp = max(water_temp_celcius)
  ) |> 
  bind_rows(spot_t) |> 
  mutate(
    month = format(date, "%m"),
    day = format(date, "%d"),
    year = format(date, "%Y"),
    across(c(month, day, year), as.numeric)
  ) |> 
  right_join(all_dates)


# Examine water temperature summary windows -------------------------------


# First, try something intuitive: average mid-June to mid-July water temps
temps |> 
  mutate(mm_dd = as.numeric(paste0(month, day))) |> 
  filter(between(mm_dd, 615,715)) |> 
  summarize(
    .by = c(system, year),
    temp = mean(mean_temp, na.rm = FALSE)
  ) |> 
  left_join(
    ss_post,
    by = c("year", "system" = "cu")
  ) |> 
  filter(!if_any(c(temp, q_50), is.na)) |> 
  ggplot(aes(x = temp, y = q_50)) +
  facet_wrap(~system) +
  geom_point(aes(colour = year)) +
  geom_smooth(method = "lm") +
  scale_colour_viridis_c() +
  labs(
    y = "Smolts-per-spawner residuals",
    x = "Mean mid-June to mid-July river temperature (°C)"
  )
# Possible trend evident for Sproat (noisy)


# How about a broader date range? Jul-Aug
temps |> 
  mutate(mm_dd = as.numeric(paste0(month, day))) |> 
  filter(month %in% 7:8) |> 
  summarize(
    .by = c(system, year),
    temp = mean(mean_temp, na.rm = FALSE)
  ) |> 
  left_join(
    ss_post,
    by = c("year", "system" = "cu")
  ) |> 
  filter(!if_any(c(temp, q_50), is.na)) |> 
  ggplot(aes(x = temp, y = q_50)) +
  facet_wrap(~system) +
  geom_point(aes(colour = year)) +
  geom_smooth(method = "lm") +
  scale_colour_viridis_c() +
  labs(
    y = "Smolts-per-spawner residuals",
    x = "Mean July to August river temperature (°C)"
  )


# How about # days exceeding 21C?
temps |> 
  filter(month %in% 6:8) |> 
  mutate(hot = if_else(mean_temp > 21, 1, 0)) |> 
  filter(!is.na(mean_temp)) |> 
  summarize(
    .by = c(system, year),
    days = n(),
    n_hot_days = sum(hot)
  ) |> 
  mutate(prop_hot_days = n_hot_days/days) |> 
  left_join(
    ss_post,
    by = c("year", "system" = "cu")
  ) |> 
  filter(!if_any(c(prop_hot_days, q_50), is.na)) |> 
  ggplot(aes(x = prop_hot_days, y = q_50)) +
  facet_wrap(~system) +
  geom_point(aes(colour = year)) +
  geom_smooth(method = "lm") +
  scale_colour_viridis_c() +
  labs(
    y = "Smolts-per-spawner residuals",
    x = "Proportion of measured days between June-Aug with temp. > 21°C"
  )


