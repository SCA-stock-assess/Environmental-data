# Packages ----------------------------------------------------------------


# Install packages
pkgs <- c("here", 'tidyverse', "readxl", "janitor")
#install.packages(pkgs)

# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(janitor)


# State the current year
curr_yr <- 2024



# Build historical climatology from harbour survey measurements -----------


# Load historical harbour survey data from Braden's work,
# which includes some reconstruction via imputation
hist_weekly <- here(
  "Somass River data",
  "weekly",
  "somass_weekly.csv"
) |> 
  read.csv() |> 
  mutate(date = as.Date(date))


# List harbour survey data in local repo files
hs_files <- list.files(
  here("Harbour Survey", "raw data"),
  pattern = "hs-[[:digit:]]{4}",   # Names should all follow this format
  recursive = TRUE,
  full.names = TRUE
)
  

# Load harbour survey river measurements from local repo
local_data <- hs_files |> 
  as_tibble_col(column_name = "filename") |>
  rowwise() |> 
  mutate(sheets = list(excel_sheets(filename))) |> # List file sheets
  unnest_wider(sheets, names_sep = "_") |> 
  rowwise() |> 
  # Read data from the "Data Assmebly" sheets in each file
  mutate(data = list(read_xlsx(path = filename, sheet = sheets_2))) |> 
  pull(data) |> 
  # Convert vars to character to ensure seamless row binding
  map(~mutate(.x, across(everything(), as.character))) |>
  bind_rows() |> 
  clean_names() |> # tidy the column names
  filter(station_cd == "PASR") |> # keep only measurements recorded in the river
  mutate(across(everything(), parse_guess)) # revert vars by guessing based on values


# Trim local data and reformat to match historical data
local_trim <- local_data |> 
  select(sample_time, water_temp_c) |> 
  rename(
    "date" = sample_time,
    "wSom" = water_temp_c
  ) |> 
  mutate(
    year = as.numeric(format(date, "%Y")),
    type = "Observed"
  ) |> 
  filter(year < curr_yr)


# Check whether any dates overlap between the two datasets
max(hist_weekly$date); min(local_trim$date) # Doesn't look like it


# Combine the two time series and group into weeks
somass_temps <- bind_rows(hist_weekly, local_trim) |> 
  mutate(
    doy = as.numeric(format(date, "%j")),
    week = cut_width(doy, width = 7, boundary = 0)
  )


# Weekly climatology
wk_clim <- somass_temps |> 
  summarize(
    .by = week,
    # Calculate summary stats for each week across years
    across(
      wSom,
      .fns = list(
        "median" = ~median(.x, na.rm = TRUE),
        "q10" = ~quantile(.x, 0.10, na.rm = TRUE),
        "q90" = ~quantile(.x, 0.90, na.rm = TRUE),
        "q25" = ~quantile(.x, 0.25, na.rm = TRUE),
        "q75" = ~quantile(.x, 0.75, na.rm = TRUE)
      ),
      .names = "{.fn}"
    )
  ) |> 
  mutate(
    week_num = as.integer(week),
    # Fit LOESS smooths to the weekly summary data
    # (makes the lines smoother when plotting)
    across(
      !contains("week"),
      .fns = list(
        "smooth" = ~predict(loess(.x ~ week_num))
      ),
      .names = "{.fn}_{.col}"
    )
  )
 


# Load real-time data from ECCC website -----------------------------------




# Plot real-time observations versus climatology data ---------------------


# Manipulate the weekly data into a format for the current year
wk_clim |> 
  mutate(
    doy = as.numeric(str_extract(week, "[[:digit:]]+(?=,)")),
    mid_doy = doy + 4,
    mid_doy = if_else(mid_doy > 365, NA, mid_doy),
    date = as.Date(mid_doy, origin = paste0(curr_yr - 1, "-12-31")),
    #date = as.Date(paste(curr_yr, format(date, "%m"), format(date, "%d"), sep = "-"))
  ) |> 
  ggplot(aes(x = date, y = smooth_median)) +
  geom_line(colour = "blue", linewidth = 0.75) +
  geom_ribbon(
    aes(ymin = smooth_q10, ymax = smooth_q90),
    fill = "blue",
    alpha = 0.15
  ) +
  geom_ribbon(
    aes(ymin = smooth_q25, ymax = smooth_q75),
    fill = "blue",
    alpha = 0.25
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = 0)
  ) +
  labs(
    x = NULL,
    y = "Water temperature (C)"
  )
