# Pre-work ---------------------------------------------------------------------

# Packages
pkgs <- c(
  "tidyverse", "tsibble", "forecast", "lubridate", "zoo", 'gginnards',
  "tseries", "imputeTS", "scales", "broom", "svMisc", "tidyhydat", "gg4x",
  "here"
)
#install.packages(pkgs)


# Libraries.
library(here); library(tidyverse);
library(ggh4x); library(tsibble); library(forecast)
library(lubridate); library(zoo); library(gginnards)
library(tseries); library(imputeTS); library(scales)
library(broom); library(svMisc); library(tidyhydat)
# Once per quarter, download the updated HYDAT database
#download_hydat()


# Set seed.
set.seed(856)


# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.margin = margin(c(0, 0, -5, 0)))


# Custom "not-in" operator.
"%ni%" <- Negate("%in%")


# Water temperature data -------------------------------------------------------

spot <- list.files(
  here("Somass temperature forecast", "temp forecast", "data"),
  pattern = "Spot and Mean Daily Water Temp Data At Depth",
  full.names = TRUE
) |> 
  tail(1) |> 
  read.csv() %>% 
  filter(Location == "Somass River [PaperMill 1990s-2000s]") %>% 
  dplyr::select(c(5,7, 11)) %>%           # Remove unnecessary columns.
  mutate(date = dmy(Date),                # Reformat date column.
         doy  = yday(date),               # Day of the year.
         week = week(ymd(date))) %>%      # Week - because data are weekly (roughly).
  dplyr::select(-Date)                    # Remove original dates.


# Five-year window to avoid overfitting.
win <- seq(as.Date(max(spot$date)) - (365.25*5),
            as.Date(max(spot$date)), by = "days")

somass <- spot[spot$date %in% win,]       # Subset to above window.

alldates <- as.data.frame(win) %>%       
  `colnames<-`(., c("date")) %>%            # Rename column,
  mutate(week = week(ymd(date)),            # Reformat dates.
         Year = year(date)) %>% 
  merge(., somass, 
        by    = c("date", "week", "Year"),  # Re-add spot check temp data.
        all.x = TRUE) %>%                   # Even missing data.
  filter(date %in% win)                     # Subset.

weekly <- alldates %>%                          
  group_by(Year, week) %>%                  # For each year and week... 
  summarise(SomT = mean(MeanWaterT,         # Calculate mean spot check temperature.
                        na.rm = TRUE), 
            date = min(date))               # And select first day in that 7 day window.
weekly$SomT[is.nan(weekly$SomT)] <- NA      # Assign missing data to NA (instead of NaN).

# Impute weekly data using linear interpolation.
weekimp <- na_interpolation(x = weekly$SomT,
                            option = "linear")


# Assign to new object, specify if data are imputed or observed.
impDF <- data.frame(wSom = as.numeric(weekimp),
                    date = ymd(weekly$date),
                    year = year(ymd(weekly$date))) %>% 
  filter(date %in% win) %>% 
  mutate(type = case_when(
    date %ni% somass$date ~ "Observed",
    date %in% somass$date ~ "Imputed"
  )) 

impDF <- impDF[impDF$date %in% win,]

# write.csv(impDF, "somass_weekly.csv", row.names = F)

STS <- ts(as.numeric(impDF$wSom),       # Set Somass temperatures as a time series object.
          frequency = 365.25/7)         # Weekly averages with annual seasonality.


# Seasonality ------------------------------------------------------------------

# Set up forecast horizon, here h = 4 weeks.
fh <- 4

# Set up list to store output values.
bestfit <- list(aicc = Inf) 

for(i in 1:25) {                        # For fourier terms 1 - 25.
  fit <- auto.arima(STS,                # Conduct automatic ARIMA models.
                    xreg = fourier(STS, K = i), # Use Fourier on timeseries data with varying number of terms.
                    seasonal = FALSE)   # Fourier to encompass seasonality so exclude from base model.
  
  if(fit$aicc < bestfit$aicc)        # If AIC is lower than subsequent model.
    bestfit <- fit                   # Choose that model.
  else break;                        # Otherwise, exit loop (saves a lot of memory).
}

(bf <- ncol(bestfit$xreg)/2)         # Optimal number of Fourier terms.
bestfit$arma; bestfit                # Optimal model and error distribution.
summary(bestfit)                     # Prints favourable summary stats.
(order <- bestfit$arma[1:3])

harmonics <- fourier(STS,            # Fit optimal Fourier model to observed data.
                     K = bf)   

# Air temperature data ---------------------------------------------------------

# Read in daily air temperature data.
airtemp <- read.csv(here("Somass temperature forecast", "temp forecast", "data", "alberni_air_temps.csv")) %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-rTemp)

fullair <- as.data.frame(win) %>%    # air temperature data available.
  `colnames<-`(., c("date")) %>%     # Rename column.
  merge(., airtemp, by = "date",     # Merge with original air temp data.
        all.x   = TRUE) %>%          # Include rows with missing temp data.
  mutate(year = year(date),          # Add year as grouping factor.
         doy  = yday(date))          # Add day of the year.


# Use linear interpolation to impute missing data.
airimp <- na_interpolation(x = fullair$MaxT,
                           option = "linear")

air.impall <- data.frame(tImp = as.numeric(airimp), # Store imputed data in DF.
                         date = fullair$date,       # Add date info.
                         year = fullair$year) %>%   # Add year.
  mutate(rAir = rollapply(tImp, 7, 
                mean, partial = TRUE)) %>%          # Rolling average of air temp.
  filter(date %in% impDF$date)                      # Keep data weekly.

# # Combine all spot check data and air temp data.
# dat <- merge(impDF, air.impall, 
#              by = c("date", "year")) %>% 
#   select(c(1:3, "rAir"))


# ARIMA ------------------------------------------------------------------------

arm.xreg <- function(xreg) {
  # y = Somass temperatures.
  Arima(y = as.numeric(STS),
        # Optimal error structure defined earlier.
        order = order,
        seasonal = FALSE,
        # Covariates = Fourier terms and whatever else is defined.
        xreg = as.matrix(cbind(harmonics, xreg)))
}

arm_dat <- air.impall[,ncol(air.impall)]

(opt_arima <- arm.xreg(xreg = arm_dat))


# Simulations ------------------------------------------------------------------

nsim <- 1000L
fh <- 4 # Future horizon (# weeks)
future <- matrix(NA, nrow = fh, ncol = nsim)

# Add forecast data
future_temps <- tribble(
  ~weeks_into_future, ~temperature,
  1, 20,
  2, 21, 
  3, 22,
  4, 25
)

for(i in seq(nsim)) {
  
  # Simulate the best-performing model. 
  future[,i] <- simulate(opt_arima,
                         # 4 weeks of forecasted harmonics (~ newxreg).
                         xreg  = cbind(as.matrix(fourier(STS, K = bf, h = fh)), 
                                       xreg = as.numeric(pull(future_temps, temperature))),
                         # Goal is future, bootstrapped values.
                         future = TRUE,
                         bootstrap = TRUE)
  
  # Print progress bar for each iteration. Takes a bit.
  progress(i, nsim) 
}

# Isolate simulated data in dataframe. 
(future_sims <- as.data.frame(future) %>% 
    # h = forecast horizon (in weeks).
    mutate(h = as.factor(paste(1:fh, "weeks"))) %>% 
    # Pivot to long-form for easier grouping. 
    pivot_longer(cols = -c(h),
                 names_to  = "sim", 
                 values_to = "temp") %>% 
    mutate(d = as.Date(max(impDF$date) + 7*as.numeric(substr(x = h, 1, 1)))))

(sim_summ <- future_sims %>% 
    group_by(h) %>% 
    # For each forecasted week, get mean temperature,
    # and % of days (from simulation) above 18, 19, 20C. 
    summarise(mean = mean(temp, na.rm = TRUE),
              p18  = round(sum(temp > 18, na.rm = TRUE)/nsim * 100, 1),
              p19  = round(sum(temp > 19, na.rm = TRUE)/nsim * 100, 1),
              p20  = round(sum(temp > 20, na.rm = TRUE)/nsim * 100, 1)) %>% 
    mutate(date = max(impDF$date) + seq(7, 7*fh, 7)))


(forecast_probs <- ggplot(data = impDF,
                          aes(x = date,
                              y = wSom)) +
    geom_hline(yintercept = c(18,19,20),
               colour = "red2",
               linetype = "dashed",
               alpha = c(1/8, 2/5, 1)) +
    geom_line(size = 1, colour = "black") +
    geom_point(size = 2, shape = 21,
               colour = "white",
               fill = "black",
               stroke = 2) + mytheme +
    labs(x = NULL, y = "Somass temperature (°C)") +
    coord_cartesian(xlim = c(max(impDF$date) - 100,
                             max(impDF$date) + 8*fh)) +
    geom_boxplot(data = future_sims,
                 aes(x  = d, y = temp,
                     group = h, width  = 8),
                 fill = "gray95",
                 alpha = 1/2,outlier.alpha = 0) +
    scale_y_continuous(limits = c(min(future_sims$temp),
                                  max(impDF$wSom)+1),
                       breaks = seq(1, 50, 3)) +
    geom_text(data = sim_summ,
              aes(x = date-3/2, y = 21,
                  label = sprintf("%0.1f", p18)),
              size = 3, hjust = 0) +
    geom_text(data = sim_summ,
              aes(x = date-3/2, y = 22,
                  label = sprintf("%0.1f", p19)),
              size = 3, hjust = 0) +
    geom_text(data = sim_summ,
              aes(x = date-3/2, y = 23,
                  label = sprintf("%0.1f", p20)),
              size = 3, hjust = 0) +
    annotate("text", y = c(21, 22, 23), size = 3,
             label = c("p18 (%) = ",
                       "p19 (%) = ",
                       "p20 (%) = "),
             x = max(impDF$date)-1))


ggsave("plots/forecast_wProbs.png", units = "px",
       width = 2200, height = 1200)