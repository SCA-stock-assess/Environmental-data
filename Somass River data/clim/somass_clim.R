# Package install
pkgs <- c("tidyverse", "ggplot2", "ggrepel", "ggpmisc", 'lubridate', "here")
#install.packages(pkgs)


# Libraries.
library(tidyverse); library(ggplot2); library(ggrepel)
library(ggpmisc); library(lubridate); library(here)


# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title  = element_blank(),
        legend.margin = margin(c(0,0,-5,0)),
        plot.margin   = margin(c(2,10,2,2)))

skip <- c("2007", "2009", "2011")

'%ni%' <- Negate("%in%")

# Response variables:
# ATUs -------------------------------------------------------------------------

annual <- here(
  "Somass River data",
  "clim",
  "somass_weekly.csv"
) |> 
  read.csv() %>% 
  group_by(year) %>% 
  mutate(doy = yday(date)) %>% 
  filter(doy > 121 & doy < 274) %>%
  filter(year %ni% skip) %>%      # Exclude missing years here.
  mutate(tDif = doy - lag(doy),
         tATU = tDif * wSom) %>% 
  summarise(ATUs  = sum(tATU, 
                        na.rm = TRUE),
            nSur  = n()) 

# Above suggests 2021 missing 2 data points. Should be slightly higher.
# Mean and Total ATUs trend identically given standardized weekly data.

(atu.plot <- ggplot(data = annual, 
                    aes(x = year,
                        y = ATUs)) +
    geom_line(linewidth = 3/4, alpha = 1/3) +
    geom_point(size = 3, shape = 21,
               color = "white",
               fill = "black") +
    mytheme +
    labs(x = NULL, y = "ATUs") +
    scale_x_continuous(breaks = seq(200, 2022, 2)))

ggsave("plots/atu_ts.png", units = "px",
       width = 2000, height = 1000)



# Maximum ----------------------------------------------------------------------

maxT <- som %>% 
  filter(year %ni% skip) %>% 
  group_by(year) %>% 
  summarise(maxT = max(wSom, na.rm = TRUE))


(temp <- ggplot(data = maxT, 
                aes(x = year,
                    y = maxT)) + 
    geom_line(size = 3/4, alpha = 1/3) +
    geom_point(size = 3, shape = 21,
               color = "white",
               fill = "black") +
    mytheme +
    labs(x = NULL, y= "Maximum temperature (°C)") +
    scale_x_continuous(breaks = seq(2000, 2025, 2)))


# Exposure index ---------------------------------------------------------------

pass <- read.csv("somass_sockeye.csv") %>% 
  mutate(date = ymd(date)) %>% 
  select(c(3,4,5,7)) %>% 
  mutate(river = case_when(
    system == "Sproat Lake"        ~ "Sproat River",
    system == "Great Central Lake" ~ "Stamp River"),
  )


ggplot(data = pass[pass$year > 2003,], 
       aes(x = date,
           y = adj_adults,
           color = system)) +
  geom_line() + mytheme +
  facet_wrap(~year, scales = "free") +
  labs(x = NULL, y = "Sockeye abundance")


som <- read.csv("somass_weekly.csv")

somassPass <- pass %>% 
  filter(year %in% annual$year) %>% 
  filter(year %ni% skip) %>% 
  group_by(date) %>% 
  summarise(somass = round(sum(adj_adults, 
                               na.rm = TRUE), 0)) %>% 
  mutate(date_adj = date - 5,
         year = year(date_adj),
         weekSum = zoo::rollsumr(somass, k = 7, 
                            fill  = somass, 
                            align = "right")) %>% 
  merge(., som[,c(1:2)],   all.x  = FALSE,
        by.x = "date_adj", by.y   = "date") %>% 
  group_by(year) %>% 
  mutate(wExpInd = wSom * weekSum/sum(weekSum)) %>% 
  summarise(expInd = sum(wExpInd, na.rm = TRUE)) 


(exp.plot <- ggplot(somassPass,
                    aes(x = year, 
                        y = expInd)) +
    geom_line(size = 3/4, alpha = 1/3) +
    geom_point(size = 3, shape = 21,
               color = "white",
               fill = "black") +
    mytheme +
    labs(x = NULL, y = "Exposure Index (°C)") +
    scale_x_continuous(breaks = seq(2000, 2025, 2)))



sproatpass <- pass[pass$river == "Sproat River", ] %>% 
  filter(year %in% annual$year) %>% 
  group_by(year) %>% 
  mutate(doy = yday(date)) %>% 
  # Julian dates below.
  summarise(minDateJ = round(quantile(doy, probs = 0.1), 0),
            maxDateJ = round(quantile(doy, probs = 0.9), 0),
            medDateJ = round(quantile(doy, probs = 0.5), 0)) %>% 
  # Calendar dates here. 
  mutate(minDate = as.Date(minDateJ, paste0(year, "-01-01")),
         maxDate = as.Date(maxDateJ, paste0(year, "-01-01")),
         medDate = as.Date(medDateJ, paste0(year, "-01-01")))


(minP <- ggplot(data = sproatpass, aes(x = year, y = minDateJ)) +
  geom_line(size = 3/4, alpha = 1/3) +
  geom_point(size = 3, shape = 21,
             color = "white",
             fill = "black") +
  mytheme +
  labs(x = NULL, y = "Day of 10% Passage") +
  scale_x_continuous(breaks = seq(2000, 2025, 2)) +
    scale_y_continuous(expand = c(0,15)))

(medP <- ggplot(data = sproatpass, aes(x = year, y = medDateJ)) +
    geom_line(size = 3/4, alpha = 1/3) +
    geom_point(size = 3, shape = 21,
               color = "white",
               fill = "black") +
    mytheme +
    labs(x = NULL, y = "Day of 50% Passage") +
    scale_x_continuous(breaks = seq(2000, 2025, 2)) +
    scale_y_continuous(breaks = seq(190, 250, 5),
                       expand = c(0,5)))

(maxP <- ggplot(data = sproatpass, aes(x = year, y = maxDateJ)) +
    geom_line(size = 3/4, alpha = 1/3) +
    geom_point(size = 3, shape = 21,
               color = "white",
               fill = "black") +
    mytheme +
    labs(x = NULL, y = "Day of 90% Passage") +
    scale_x_continuous(breaks = seq(2000, 2025, 2)) +
    scale_y_continuous(breaks = seq(290, 320, 2)))




# Predictors second:
# 1. Snow pack -----------------------------------------------------------------

# Mount Cokely snow pack data
# https://aqrt.nrs.gov.bc.ca/Data/Location/Summary/Location/3B02A/Interval/Latest

snow <- read.csv("DataSetExport-SD.Field Visits@3B02A-20230825160412.csv",
                 skip = 2) %>% 
  select(c(1,3)) %>% 
  `colnames<-`(., c("date", "snow_cm")) %>% 
  mutate(date  = mdy(sub(" .*", "", date)),
         Year  = year(date),
         month = month(date)) %>%
  filter(month %in% c(1:4)) %>% 
  group_by(Year) %>% 
  summarise(mSnow = mean(snow_cm, na.rm = TRUE),
            sSnow = sd(snow_cm, na.rm = TRUE),
            nSnow = n())

# Visualize snow pack time series.
# Note missing data in 2019-2022.
(snow.plot <- ggplot(data = snow %>% 
         filter(Year > 1999), 
       aes(x = Year, 
           y = mSnow,
           group = 1)) +
  geom_line(alpha = 1/3, size = 3/4) +
  geom_errorbar(aes(ymin = mSnow - sSnow,
                    ymax = mSnow + sSnow),
                alpha = 1/2,
                width = 1/2) +
  geom_point(size  = 3, colour = "white",
             shape = 21, fill  = "black") + 
  mytheme +
  scale_x_continuous(breaks = seq(200, 2100, 3)) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  labs(x = NULL, y = "Mount Cokely snow pack (cm)"))

ggsave("plots/cokely_snow.png", units = "px",
       width = 2000, height = 1000)


# Anomaly ----------------------------------------------------------------------

# NOAA ocean index data.
noaa <- read_table(file = "cpc.ncep.noaa.gov_data_indices_oni.ascii.txt") %>% 
  filter(SEAS %in% c("JFM", "FMA", "MAM")) %>% 
  group_by(YR) %>% 
  summarise(anomM = mean(ANOM)) %>% 
  filter(YR %in% annual$year) %>% 
  rename("year" = "YR")

(anom.plot <- ggplot(data = noaa,
                    aes(x = year,
                        y = anomM)) +
    geom_line(alpha = 1/3, size  = 3/4) + 
    geom_point(size = 3, shape = 21, 
               color = "white", fill = "black") +
    mytheme + labs(x = NULL, y = 'SST Anomaly Index') +
    scale_x_continuous(breaks = seq(200, 2022, 3)))


# PNI ---------------------------------------------------------------------

pni <- read.csv("PNW_aPNI.csv", 
                skip = 1, 
                check.names = F) %>% 
  select(c(Year, 'Annual aPNI')) %>% 
  rename('aPNI' = 'Annual aPNI') %>% 
  filter(Year %in% annual$year)

(pni.plot <- ggplot(data = pni, 
                    aes(x = Year, y = aPNI)) +
    geom_line(alpha = 1/3, size  = 3/4) + 
    geom_point(size = 3, shape = 21, 
               color = "white", fill = "black") +
    mytheme + labs(x = NULL, y = 'PNI') +
    scale_x_continuous(breaks = seq(200, 2022, 3)))



# -------------------------------------------------------------------------

(preds <- cowplot::plot_grid(snow.plot, pni.plot, anom.plot, 
                             ncol = 1, align = "v",
                             labels = c("1a", "2a", "3a"), 
                             label_x = 0.12))

(snowpack <- ggplot()+geom_density(data = snow, aes(mSnow)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(pniden <- ggplot()+geom_density(data = pni, aes(aPNI)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(anomDen <- ggplot()+geom_density(data = noaa, aes(anomM)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))

(predDens <- cowplot::plot_grid(snowpack, pniden, anomDen, ncol = 1, align=  "v",
                                labels = c("1b", "2b", "3b"),
                                label_x = 0.165))

cowplot::plot_grid(preds, predDens, ncol = 2, rel_widths = c(5,3))


ggsave("plots/preds_TS.png", units = "px",
       width = 2000, height = 2000)

(respTS <- cowplot::plot_grid(atu.plot, exp.plot, temp, 
                             minP, medP, maxP,
                             ncol = 1, align = "v",
                             labels = c("1a", "2a", "3a","4a", "5a", "6a"),
                             label_x = 0.08))

(maxHis <- ggplot()+geom_density(data = maxT, aes(maxT)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(ATUHis <- ggplot()+geom_density(data = annual, aes(ATUs)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(expHis <- ggplot()+geom_density(data = somassPass, aes(expInd)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(minD <- ggplot()+geom_density(data = sproatpass, aes(minDateJ)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(medD <- ggplot()+geom_density(data = sproatpass, aes(medDateJ)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))
(maxD <- ggplot()+geom_density(data = sproatpass, aes(maxDateJ)) +
    labs(x = NULL, y = NULL) + mytheme +
    scale_y_continuous(labels = function(x) sprintf("%.3f", x)))

(respD <- cowplot::plot_grid(ATUHis, expHis, maxHis,
                             minD, medD, maxD,
  ncol = 1, align = "v", label_x = 0.14,
  labels = c("1b", "2b", "3b", "4b", "5b", "6b")))

(f <- cowplot::plot_grid(respTS, respD, ncol = 2, rel_widths = c(5,2)))

ggsave("plots/conditions.png", units= "px", width = 2500, height = 3000)


# -------------------------------------------------------------------------

ExposureIndex <- somassPass %>% 
  mutate(type = "response",
         var  = "Exposure Index") %>% 
  rename("val"  = "expInd")

MaximumTemp <- maxT %>% 
  mutate(type = "response", 
         var  = "Maximum Temperature") %>% 
  rename("val" = "maxT")

ATUs <- annual[,c(1:2)] %>% 
  mutate(type = "response",
         var  = "Accumulated Thermal Units") %>% 
  rename("val" = "ATUs")

dateVars <- sproatpass %>%  
  pivot_longer(cols = c("minDateJ", "medDateJ", "maxDateJ"),
               names_to = "var", values_to = "val") %>% 
  mutate(type = "response") %>% 
  mutate(var = case_when(
    var == "maxDateJ" ~ "Date of 90% Passage (DOY)",
    var == "medDateJ" ~ "Date of 50% Passage (DOY)",
    var == "minDateJ" ~ "Date of 10% Passage (DOY)"
  )) %>% 
  split(., .$var)

resp.list <- c(list(ExposureIndex, MaximumTemp, ATUs), dateVars)

library(ggrepel)

multiplot <- function(x) {
  
 newdf <- merge(x, predvars, by = "year") %>% 
   mutate(yr = substr(year, 3, 4)) 

 ggplot(data = newdf, 
        aes(x = val.y,
            y = val.x)) +
   mytheme +
   geom_smooth(method = "lm",
               alpha = 1/6,
               color = "black",
               linetype = "dashed")  +
   scale_y_continuous(expand = expand_scale(mult = c(1/10, 1/3))) +
   geom_point(size = 2,
              shape = 21,
              colour = "black",
              fill   = "gray50") +
   facet_wrap(~ var.y, scales = "free") +
   labs(x = NULL, y = unique(newdf$var.x)) +
   stat_poly_eq(use_label(c("R2", "p")),
                label.x = "left",
                label.y = "top",
                small.p = TRUE) +
   theme(axis.title.y = element_text(size = 9),
         strip.background = element_blank())
 
  }

ps <- lapply(resp.list, multiplot)

cowplot::plot_grid(ps[[1]] + theme(strip.background = element_blank()),
                   ps[[2]] + theme(strip.text = element_blank()),
                   ps[[3]] + theme(strip.text = element_blank()),
                   ps[[4]] + theme(strip.text = element_blank()),
                   ps[[5]] + theme(strip.text = element_blank()),
                   ps[[6]] + theme(strip.text = element_blank()),
                   ncol = 1, align = "v")

ggsave("plots/var_cors.png", units= "px",
       width = 2500, height = 3200)

################################################################################

library(GGally)


rcond <- responsevars[,c(1,2,6,10)] %>% 
  `colnames<-`(., c("Year", "Exposure index", "Maximum temperature", "Accumulated thermal units"))

rconds12 <- rcond[rcond$Year != "2012", ]

ggpairs(data = rcond[,-1],
        upper = list(continuous = wrap("cor", 
                     method = "spearman")),
        lower = list(continuous = wrap("points", 
                     size = 2.5, alpha = 1/3))) +
  mytheme; ggsave("plots/cond_cors.png", units = "px",
  width = 2500, height = 2000)

ggplot(data = rcond, 
       aes(x = `Exposure index`,
           y = `Accumulated thermal units`)) + 
  geom_smooth(method = "lm", colour = "black",
              linetype = 2, se = FALSE) +
  geom_smooth(data = rconds12, method = "lm",
              fullrange = T, alpha = 1/10,
              se = FALSE, color = "gray") +
  geom_point(size = 2, shape = 21,
             color = "black", fill = "gray90") +
  mytheme +
  labs(x = "Exposure index (°C)",
       y = "Accumulated thermal units (°C)")
