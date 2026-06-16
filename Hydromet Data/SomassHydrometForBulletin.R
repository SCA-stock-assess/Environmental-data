library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(purrr)
library(janitor)
library(zoo)
library(magrittr)   # retained for %T>% in summary blocks
library(mgcv)
library(rvest)
library(askpass)
library(glue)

# Current year
curr_yr <- 2026

# Login ------------------------------------------------------------------
my_session <- session("https://data.romcomm.com/")
log_in_form <- html_form(my_session)[[1]]

my_session <- session_submit(
  my_session,
  html_form_set(
    log_in_form,
    `ctl00$cphBody$txtUsername` = "dfoweather",
    `ctl00$cphBody$txtPass`     = askpass()
  )
)

# Station parameters -----------------------------------------------------
params <- list(
  lid     = c("2022628796262", "2018510632229"),
  gid     = c("0", "7"),
  station = c("Sproat", "Stamp"),
  days    = rep(8760, 2)
)

# Scrape function --------------------------------------------------------
get_data <- function(lid, gid, station, days) {
  path <- paste0(
    "https://data.romcomm.com/Detail.aspx?lid=", lid,
    "&gid=",  gid,
    "&days=", days
  )
  
  tryCatch({
    url    <- session_jump_to(my_session, path)
    tables <- html_elements(url, "table") |> html_table()
    
    # Sanity check #1: no tables at all — likely login failure or page change
    if (length(tables) == 0)
      stop("No tables found on page — possible login failure or page change")
    
    # Identify data table by expected column name rather than position
    tbl_idx <- detect_index(tables, \(t) "Received" %in% names(t))
    
    # Sanity check #2: expected table not found — report what columns were found
    if (tbl_idx == 0L)
      stop(glue(
        "Data table not found for {station} — columns in first table: ",
        paste(names(tables[[1]]), collapse = ", ")
      ))
    
    tbl <- tables[[tbl_idx]]
    
    # Sanity check #3: table exists but is empty
    if (nrow(tbl) == 0)
      stop(glue("Table found but has 0 rows for {station}"))
    
    # Sanity check #4: suspiciously few rows (< 1 week of hourly data)
    if (nrow(tbl) < 168)
      warning(glue("{station}: only {nrow(tbl)} rows returned — expected ~8760"))
    
    message(glue("{station}: {nrow(tbl)} rows retrieved"))
    
    tbl
    
  }, error = \(e) {
    warning(glue("Failed to scrape {station}: {e$message}"))
    NULL
  })
}

# Scrape current year ----------------------------------------------------
latest_year_data <- pmap(params, get_data) |>
  set_names(c("Sproat", "Stamp")) |>
  compact() |>
  list_rbind(names_to = "station")

# Process current-year time series ---------------------------------------
curr <- latest_year_data |>
  clean_names() |>
  rename(wtemp = water_temperature_c) |>
  mutate(
    # Sproat: sensor_depthm is direct sensor depth
    # Stamp:  levelm is the depth measure; surface_levelm is absolute water surface
    #         elevation (~10m) and is not a depth reference — use levelm directly
    depth = case_when(
      station == "Sproat" ~ sensor_depthm,
      station == "Stamp"  ~ levelm,
      .default = NA_real_
    ),
    station_time = as.POSIXct(received, format = "%d-%b-%y %I:%M %p"),
    year  = format(station_time, "%Y") |> as.numeric(),
    month = format(station_time, "%m") |> as.numeric(),
    day   = format(station_time, "%j") |> as.numeric(),
    date  = as.Date(station_time),
    d.m   = format(date, "%d %b"),
    doy   = format(date, "%j") |> as.numeric(),
    # QC: flag implausible water temperatures as NA
    wtemp = if_else(between(wtemp, 2, 30),   wtemp, NA_real_),
    # QC: flag implausible depth values as NA
    depth = if_else(between(depth, 0.05, 5), depth, NA_real_)
  ) |>
  # Exclude most recent day: partial data biased toward cooler overnight readings
  filter(
    date < max(date, na.rm = TRUE),
    year == max(year, na.rm = TRUE)
  ) |>
  pivot_longer(c(wtemp, depth), names_to = "var") |>
  select(station, station_time:last_col())

# Daily summary function -------------------------------------------------
daily_sum <- function(data) {
  data |>
    ungroup() |>
    summarise(
      .by = c(station, year, month, d.m, day, date, doy, var),
      mean = mean(value, na.rm = TRUE),
      q5   = quantile(value, 0.05, na.rm = TRUE),
      q95  = quantile(value, 0.95, na.rm = TRUE),
      min  = min(value, na.rm = TRUE),
      max  = max(value, na.rm = TRUE)
    ) |>
    # Replace NaN and Inf produced by aggregating all-NA groups
    mutate(
      across(
        mean:last_col(),
        \(x) if_else(is.nan(x) | is.infinite(x), NA_real_, as.numeric(x))
      )
    ) |>
    group_by(station, var) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      across(
        mean:last_col(),
        list(
          `7d`  = \(x) rollmean(x, k = 7,  fill = NA),
          `15d` = \(x) rollmean(x, k = 15, fill = NA),
          `30d` = \(x) rollmean(x, k = 30, fill = NA)
        ),
        .names = "{.fn}.{.col}"
      )
    ) |>
    ungroup()
}

# Historical time series -------------------------------------------------
# Multi-year hydromet data; restricted to curr_yr-10 to curr_yr-1 for climatology.
hist <- read_xlsx(
  here("Hydromet Data", "Hydromets_historic - 2025.xlsx"),
  na = c("", "-999")
) |>
  clean_names() |>   # yields: station, station_time, air_temp_celcius,
  # water_temp_celcius, staff_gauge_m_h20,
  # sensor_depth_m_h20, rainfall_mm, year
  rename(
    wtemp    = water_temp_celcius,
    atemp    = air_temp_celcius,
    gauge    = staff_gauge_m_h20,
    depth    = sensor_depth_m_h20,
    rainfall = rainfall_mm
  ) |>
  mutate(
    # Remove estimation flags (e.g. "..Estimated") from character columns only
    across(where(is.character), \(x) str_replace(x, "[:punct:]{2}Estimated", "") |> str_trim()),
    across(c(wtemp, atemp, gauge, depth, rainfall), as.numeric),
    station_time = as.POSIXct(station_time, format = "%Y-%m-%d %H:%M:%S"),
    date     = as.Date(station_time),
    d.m      = format(date, "%d %b"),
    doy      = format(date, "%j") |> as.numeric(),
    # QC: flag implausible values as NA
    wtemp    = if_else(between(wtemp, 2, 30),    wtemp,    NA_real_),
    atemp    = if_else(between(atemp, -30, 50),  atemp,    NA_real_),
    depth    = if_else(between(depth, 0.05, 5),  depth,    NA_real_),
    gauge    = if_else(between(gauge, 0, 15),     gauge,    NA_real_),
    rainfall = if_else(between(rainfall, 0, 10), rainfall, NA_real_)
  ) |>
  # Restrict to 10 complete calendar years prior to current year
  # (uses raw `year` column to avoid date-parsing edge cases)
  filter(between(as.numeric(year), curr_yr - 10, curr_yr - 1)) |>
  pivot_longer(c(wtemp, atemp, gauge, depth, rainfall), names_to = "var") |>
  group_by(station, doy, var) |>
  mutate(z = scale(value)[, 1])

# Diagnostic: distribution of all variables — set to TRUE to run
if (FALSE) {
  hist |>
    ggplot(aes(value)) +
    facet_wrap(~var, scales = "free") +
    geom_histogram(fill = "white", colour = "black")
}

# Diagnostic: heatmap of wtemp and depth by DOY — set to TRUE to run
if (FALSE) {
  hist |>
    filter(!abs(z) > 3, var %in% c("wtemp", "depth")) |>
    ggplot(aes(doy, value)) +
    facet_grid(var ~ station, scales = "free_y", switch = "y") +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom    = "raster",
      contour = FALSE,
      n       = 150
    ) +
    scale_fill_viridis_c(option = "mako", direction = -1) +
    coord_cartesian(expand = FALSE) +
    guides(fill = "none") +
    labs(y = NULL, x = "Day of year") +
    theme(
      strip.background.y = element_blank(),
      strip.placement    = "outside",
      axis.text.x        = element_text(angle = 45, hjust = 1)
    )
}

# Summarise to daily resolution ------------------------------------------
hist_sum <- hist |>
  filter(!abs(z) > 3) |>   # remove outliers beyond 3 SD within station/doy/var
  daily_sum()

curr_sum <- daily_sum(curr)

# Collapse historical daily summaries to DOY climatology -----------------
hist_doy <- hist_sum |>
  group_by(station, doy, var) |>
  summarize(
    across(matches("\\d+d\\.mean"), \(x) mean(x, na.rm = TRUE)),
    across(matches("\\d+d\\.q5"),   \(x) min(x,  na.rm = TRUE)),
    across(matches("\\d+d\\.q95"),  \(x) max(x,  na.rm = TRUE)),
    max  = max(max,  na.rm = TRUE),
    min  = min(min,  na.rm = TRUE),
    mean = mean(mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  drop_na(doy)

# Figure 6: current year vs historical average ---------------------------
recode_var <- function(x) case_when(
  x == "wtemp" ~ "Water temperature (°C)",
  x == "depth" ~ "Sensor depth (m)",
  TRUE ~ x
)

hist_doy_smooth <- hist_doy |>
  filter(var %in% c("wtemp", "depth")) |>
  mutate(
    var  = recode_var(var),
    date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
  ) |>
  group_by(var, station) |>
  mutate(
    ymin_smooth = predict(loess(min ~ doy, span = 0.5)),
    ymax_smooth = predict(loess(max ~ doy, span = 0.5))
  ) |>
  ungroup()

hist_sum_plot <- hist_sum |>
  filter(var %in% c("wtemp", "depth")) |>
  mutate(
    var  = recode_var(var),
    date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
  )

curr_sum_plot <- curr_sum |>
  filter(var %in% c("wtemp", "depth")) |>
  mutate(
    var  = recode_var(var),
    date = as.Date(paste0(curr_yr - 1, "-12-31")) + doy
  )

col_hist   <- "#4E84C4"   # medium blue  — historical smooth line
col_ribbon <- "#A8C8E8"   # light blue   — historical range ribbon
col_curr   <- "#C0392B"   # muted red    — current year line

legend <- c(
  paste0("Historical (", curr_yr - 10, "-", curr_yr - 1, ")"),
  as.character(curr_yr)
)

comp_plot <- ggplot(hist_sum_plot, aes(x = date, y = mean)) +
  facet_grid(var ~ station, scales = "free_y", switch = "y") +
  geom_ribbon(
    data   = hist_doy_smooth,
    aes(ymin = ymin_smooth, ymax = ymax_smooth),
    fill   = col_ribbon,
    alpha  = 0.5,
    colour = NA
  ) +
  geom_smooth(
    aes(colour = legend[1]),
    method    = "loess",
    span      = 0.5,
    se        = FALSE,
    linewidth = 0.6
  ) +
  geom_line(
    data      = curr_sum_plot,
    aes(y = mean, colour = legend[2]),
    linewidth = 1.55
  ) +
  scale_colour_manual(
    "",
    values = setNames(c(col_hist, col_curr), legend)
  ) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq.Date(
      as.Date(paste0(curr_yr, "-01-01")),
      as.Date(paste0(curr_yr, "-12-31")),
      by = "month"
    ),
    expand = c(0, 0)
  ) +
  coord_cartesian(
    xlim = c(
      as.Date(paste0(curr_yr, "-04-01")),
      as.Date(paste0(curr_yr, "-10-30"))
    )
  ) +
  labs(y = NULL, x = NULL) +
  guides(fill = "none") +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.background = element_blank(),
    legend.key         = element_blank(),
    strip.background.x = element_rect(fill = "grey92", colour = "grey40"),
    strip.background.y = element_blank(),
    strip.text.x       = element_text(face = "bold", size = 11),
    strip.text.y       = element_text(size = 10),
    strip.placement    = "outside",
    panel.border       = element_rect(colour = "grey40", fill = NA),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

comp_plot

# Save figure ------------------------------------------------------------
ggsave(
  plot     = comp_plot,
  filename = here(
    "Hydromet Data", "plots",
    paste0("Fig6_Stamp-Sproat_Hydromets_", Sys.Date(), ".png")
  ),
  width  = 6,
  height = 4.5,
  units  = "in"
)

# Weekly average water temperature ---------------------------------------
# Used in the environmental conditions blurb of the in-season bulletin
curr |>
  distinct(station, station_time, var, .keep_all = TRUE) |>
  pivot_wider(names_from = var, values_from = value) |>
  filter(day >= max(day) - 7) |>
  group_by(station) %T>%
  { options(warn = -1) } %>%       # suppress warnings from averaging date columns
  summarize(
    across(
      c(wtemp, date),
      list(mean = mean, min = min, max = max),
      .names = "{.fn}_{.col}",
      na.rm  = TRUE
    )
  ) %T>%
  { options(warn = 0) } %>%
  select(-mean_date)

# 3-day average water temperature ----------------------------------------
# Input into SoxSum rows 478 & 481 (Sproat and Stamp 3-day average water temperature)
curr |>
  distinct(station, station_time, var, .keep_all = TRUE) |>
  pivot_wider(names_from = var, values_from = value) |>
  filter(day >= max(day) - 3) |>
  group_by(station) %T>%
  { options(warn = -1) } %>%
  summarize(
    across(
      c(wtemp, date),
      list(mean = mean, min = min, max = max),
      .names = "{.fn}_{.col}",
      na.rm  = TRUE
    )
  ) %T>%
  { options(warn = 0) } %>%
  select(-mean_date)



