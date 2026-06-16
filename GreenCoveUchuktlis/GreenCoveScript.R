library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(tidyverse)
library(lubridate)
library(here)

# ── 1. Parse & rename ────────────────────────────────────────────────────────
dput(names(GreenCoveCleaned)) #to get the column names easily
GreenCove<- read.csv("Green Cove Data 2026.csv") %>% 
 rename(
    DateTimeRaw = Date.Time.US.Pacific. ,
    DOPercent = DO....,
    pH = pH.pH.,
    SalinityPSU = Salinity.PSU.,
  #  TurbidityNTU = Turbidity.NTU. ,
    WaterTemp = Water.Temp.C.
  )

GreenCoveCleaned <- GreenCove %>% 
  mutate(
    DateTime = mdy_hm(DateTimeRaw, tz="America/Los_Angeles")) %>% 
  select(-DateTimeRaw)
# ── 2. Long format for faceted plotting ──────────────────────────────────────
GreenCoveLong <- GreenCoveCleaned %>% 
  pivot_longer(
    cols = c(DOPercent, pH, SalinityPSU, WaterTemp),
    names_to = "Variable",
    values_to = "Measurement") %>% 
  mutate(
    Variable = factor(Variable,
                      levels = c("WaterTemp", "DOPercent", "SalinityPSU"),
                      labels = c("Temp (°C)", "DO (%sat)", "Salinity (PSU)"))
  ) %>% 
  filter(!is.na(Variable))

(GCoveplot <- ggplot(GreenCoveLong, aes(x = DateTime, y = Measurement)) +
    geom_line(
      colour    = "#5A8A6A",
      linewidth = 0.8
    ) +
    facet_wrap(
      ~Variable,
      scales         = "free_y",
      ncol           = 1,
      strip.position = "left"
    ) +
    scale_x_datetime(
      date_labels = "%b %d",
      date_breaks = "1 week",
      expand      = c(0, 0)
    ) +
    labs(y = NULL, x = NULL) +
    theme_bw(base_size = 11) +
    theme(
      strip.background.y = element_blank(),
      strip.text.y       = element_text(size = 10),
      strip.placement    = "outside",
      panel.border       = element_rect(colour = "grey40", fill = NA),
      panel.grid.major   = element_line(colour = "grey90"),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1)
    )
)



GreenCoveLong |>
  filter(Variable == "DO (%sat)") |>
  mutate(Date = as.Date(DateTime, tz = "America/Los_Angeles")) |>
  summarise(mean = mean(Measurement, na.rm = TRUE),
            lo   = min(Measurement, na.rm = TRUE),
            hi   = max(Measurement, na.rm = TRUE), .by = Date) |>
  ggplot(aes(Date)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(aes(y = mean)) +
  theme_bw()

GCPlotForBulletin<- GreenCoveLong  |>
  mutate(Date = as.Date(DateTime, tz = "America/Los_Angeles")) |>
  summarise(Measurement = mean(Measurement, na.rm = TRUE), .by = c(Date, Variable)) |>
  ggplot(aes(Date, Measurement)) +
  geom_line(colour = "#6a9e78") +
  geom_point(size = 2, colour = "black") +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme_bw()+
  labs(x = NULL, y = NULL)+
  theme(panel.grid = element_blank())

ggsave(
  plot = GCPlotForBulletin,
  filename = here("GreenCoveUchuktlis", "Plots", paste0("GreenCoveWQ", Sys.Date(), ".png")), #added the date to the name
  width = 6.5,
  height = 5.5,
  units = "in"
)

