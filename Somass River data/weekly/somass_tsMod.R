

# Pre-work ---------------------------------------------------------------------


# Libraries.
library(tidyverse); library(tsibble); library(forecast)
library(lubridate); library(zoo); library(gginnards)
library(tseries); library(imputeTS); library(scales)
library(broom); library(svMisc); library(tidyhydat)
library(ggh4x)


# Set working directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()


# Set seed.
set.seed(856)


# Custom theme.
mytheme <- theme_bw() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.margin = margin(c(0, 0, -5, 0)))


# Custom "not-in" operator.
"%ni%" <- Negate("%in%")


# Explore time series data -----------------------------------------------------

spot <- read.csv("Spot and Mean Daily Water Temp Data At Depth 21.09.21.csv") %>% 
  filter(Location == "Somass River [PaperMill 1990s-2000s]") %>% 
  dplyr::select(c(5,7, 11)) %>%           # Remove unnecessary columns.
  mutate(date = dmy(Date),                # Reformat date column.
         doy  = yday(date),               # Day of the year.
         week = week(ymd(date))) %>%      # Week - because data are weekly (roughly).
  dplyr::select(-Date)                    # Remove original dates.

max(spot$date)


win <- seq(as.Date("2017-01-01"),         # Isolate study window in variable.
           as.Date("2022-12-31"),         # Almost 5 full years.
           by = "days")                   # of daily info.


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

# Visualize NA distribution.
ggplot_na_distribution(x = weekly$SomT,
                       x_axis_labels = weekly$date)

# Save NA distribution plot to directory.
ggsave("plots/somass_spotcheck.png", units = "px", 
       width = 2500, height = 1250)

# Impute weekly data using linear interpolation.
weekimp <- na_interpolation(x = weekly$SomT,
                            option = "linear")

# Visualize imputations.
ggplot_na_imputations(weekly$SomT, weekimp) +
  ylab("Temperature (ºC)") + 
  ggtitle(NULL, subtitle = NULL) +
  mytheme + xlab("Week")

ggsave("plots/imputed_somass.png", units = "px",
       widt = 2000, height = 1200)

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

ns <- decompose(STS); plot(ns)    # View decomposition of time series data.
plot(ns$seasonal)                 # Clearly strong seasonal component.

# Check frequency of time series computationally.
(ffreq <- forecast::findfrequency(STS))
# Aligns well with expected periodicity. 
all.equal(ffreq, 52)

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
  else break;                        # Otherwise, exist loop (saves a lot of memory).
}

(bf <- ncol(bestfit$xreg)/2)         # Optimal number of Fourier terms.
bestfit$arma; bestfit                # Optimal model and error distribution.
summary(bestfit)                     # Prints favourable summary stats.
(order <- bestfit$arma[1:3])

# I find the "checkresiduals" function from forecast awkward.
# I just recreate the same plots here. 
resVals <- data.frame(r = as.numeric(bestfit$residuals))

# Histogram of residual variance.
(resHis <- ggplot(data = resVals) +
    geom_histogram(aes(x = r), binwidth = 1/10, 
                   colour = "black", fill = "gray80") + 
    labs(x = "Residual", y = "Frequency") + mytheme)

# Autocorrelation function plot. 
(acfP <- ggplot(data = data.frame(acf = Acf(bestfit$residuals)$acf,
                                  lag = Acf(bestfit$residuals)$lag) %>% 
                  filter(lag != 0), aes(x = lag, y = acf)) +
    geom_point() + mytheme + geom_segment(aes(x = lag, xend = lag, yend = 0)) +
    labs(x = "Lag", y = "Autocorrelation function") +
    geom_hline(yintercept = 2/sqrt(length(STS))*c(1, -1), 
               colour = "blue", linetype = 2) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_x_continuous(limits = c(0, 40)) +
    scale_y_continuous(limits = c(-0.13, 0.13)))

# Time series of residual variation.
(resplot <- ggplot(data = data.frame(date = impDF$date,
                                     res = bestfit$residuals)) +
    geom_line(aes(x = date, y = res)) + mytheme +
    labs(x = NULL, y = "Residual") + theme(plot.background = element_blank()) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))

# Throw em all together.
cowplot::plot_grid(resplot ,cowplot::plot_grid(acfP, resHis, 
         ncol = 2, align = "vh"), ncol = 1, align = "h", 
         rel_heights = c(5,6)) + theme(plot.background = element_blank())

ggsave("plots/arima_diagnostics.png", units = "px",
       width = 3000, height = 1800)


harmonics <- fourier(STS,            # Fit optimal Fourier model to observed data.
                     K = bf)         # Using lowest AIC selection.
nrow(harmonics) == length(STS)       # Both 249 rows.

# Test for stationarity. 
Box.test(bestfit$residuals, type = "Ljung-Box")

# Air temperature data ---------------------------------------------------------

# Read in daily air temperature data.
airtemp <- read.csv("alberni_temps.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-rTemp)

fullair <- as.data.frame(win) %>%    # air temperature data available.
  `colnames<-`(., c("date")) %>%     # Rename column.
  merge(., airtemp, by = "date",     # Merge with original air temp data.
        all.x   = TRUE) %>%          # Include rows with missing temp data.
  mutate(year = year(date),          # Add year as grouping factor.
         doy  = yday(date))          # Add day of the year.

# Plot distribution of NAs.
ggplot_na_distribution(fullair$MaxT,
                       x_axis_labels = fullair$date)

# Use linear interpolation to impute missing data.
airimp <- na_interpolation(x = fullair$MaxT,
                           option = "linear")

# Visualize imputed data.
ggplot_na_imputations(fullair$MaxT, airimp,
                      x_axis_labels = fullair$date) +
  ylab("Temperature (°C)") + ggtitle(NULL,subtitle = NULL) +
  mytheme + xlab(NULL)

ggsave("plots/air_temp_imp.png", units = "px",
       width = 2000, height = 1200)

air.impall <- data.frame(tImp = as.numeric(airimp),       # Store imputed data in DF.
                         date = fullair$date,             # Add date info.
                         year = fullair$year) %>%         # Add year.
  mutate(rAir = rollapply(tImp, 7, mean, partial = TRUE)) # Rolling average of air temp.


# Combine all spot check data and air temp data.
dat <- merge(impDF, air.impall, 
             by = c("date", "year")) %>% 
  select(c(1:3, "rAir"))

datPL <- dat %>% 
  pivot_longer(cols = c("wSom", "rAir")) %>% 
  mutate(lab = case_when(name == "wSom" ~ "Somass temperature (°C)",
                         name == "rAir" ~ "Air temperature (°C)"))

# visualize relationship - both highly seasonal.
(sumtPan <- ggplot(data = datPL,
       aes(x = date, y = value, colour = lab)) +
  geom_line(size = 4/5, alpha = 3/4) + mytheme +
  labs(x = NULL, y = "Temperature (°C)") +
  facet_wrap(~year, scales = "free_x") +
  scale_x_date(date_labels = "%b") +
  theme(legend.position = "none"))

(sumtFul <- ggplot(data = datPL,
                aes(x = date, y = value, colour = lab)) +
    geom_line(size = 4/5, alpha = 3/4) + mytheme +
    labs(x = NULL, y = "Temperature (°C)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y"))

cowplot::plot_grid(sumtFul, sumtPan, ncol = 1)

# Save time series plot.
ggsave("plots/temps_TS.png", units = "px",
       width = 2000, height = 2500)


# Fits between Somass temp and air temp.
(fit  <- lm(data = dat, wSom ~ rAir)); summary(fit)
(fit2 <- lm(data = dat, wSom ~ poly(rAir, 2, raw = TRUE))); summary(fit2)
(fit3 <- lm(data = dat, wSom ~ poly(rAir, 3, raw = TRUE))); summary(fit3)
anova(fit, fit2, fit3)

# Isolate as dataframe for later. 
airvars <- data.frame(date = dat$date,
           poly(dat$rAir, 3, raw = TRUE)) 

# Flow effects -----------------------------------------------------------------

# Ash river flows - m3/s.
ash <- hy_daily_flows(station_number = "08HB023",
                      start_date = min(win)) %>%
  mutate(year = as.factor(year(Date)),
         rAsh = rollapply(Value, 14, mean,
                             partial = TRUE)) %>%
  dplyr::select(c("Date", "rAsh", "year")) %>%
  `colnames<-`(., tolower(c(colnames(.)))) 

# Isolate Ash flows. 
ashFlow <- ash[ash$date %in% impDF$date, c("date", "rash")]


# Sproat river flows - m3/s.
sproat <- hy_daily_flows(station_number = "08HB008",
                         start_date = min(win)) %>%
  mutate(year = as.factor(year(Date)),
         rSproat = rollapply(Value, 14, mean,
                             partial = TRUE)) %>%
  dplyr::select(c("Date", "rSproat", "year")) %>%
  `colnames<-`(., tolower(c(colnames(.)))) 

# Sproat contributes much more water, although temporal patterns are similar.
summary(sproat$rsproat); summary(ash$rash)
t.test(sproat$rsproat, ash$rash, paired = TRUE)

# Account for anomalously high flooding events. 
# Make downstream analysis challenging and don't reflect typical hydrology.
hist(sproat$rsproat)
(spq <- quantile(sproat$rsproat, probs = c(0.90, 0.95, 0.99))); spq[[1]]
sproat$rsproat[sproat$rsproat > spq[[1]]] <- spq[[1]]
hist((sproat$rsproat))

# Isolate Sproat flows. 
sproatFlow <- sproat[sproat$date %in% impDF$date, c("date", "rsproat")]
 
# Make sure all dates are accounted for. 
nrow(ashFlow) == nrow(sproatFlow) & nrow(impDF)

flows <- merge(ashFlow, sproatFlow, by = "date") %>% 
  pivot_longer(cols = c("rash", "rsproat")) %>% 
  mutate(name = tools::toTitleCase(sub('.', '', name)))

# (flowcor <- lm(data = flows, rash ~ rsproat)); summary(flowcor)

(flowTS <- ggplot(data = flows, aes(x = date, y = value, colour = name)) +
    geom_line(size = 1) + mytheme +
    labs(x = NULL, y = "Discharge (cms)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top"))

(sumTS <- ggplot(data = flows %>% 
                  mutate(year = year(date),
                         month = month(date),
                         doy = yday(date)) %>% 
                  filter(month %in% 5:9),
                aes(x = date, y = value, colour = name)) +
    geom_line(size = 1) +
    facet_wrap(~year, scales = "free_x") +
    mytheme +
    labs(x = NULL, y = "Discharge (cms)") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 8)) +
    scale_x_date(breaks = "6 weeks",
                 date_labels = "%B %d"))

cowplot::plot_grid(flowTS, sumTS, ncol = 1)
ggsave("plots/flows.png", units = "px",
       width = 2000, height = 2500)


# Split into training and test data --------------------------------------------

# Original temperature time series. 
STStrain <- STS[1:(length(STS)-nrow(impDF[impDF$date >= "2022-01-01",]))]
STStest  <- STS[1:(length(STS)-nrow(impDF[impDF$date <  "2022-01-01",]))]
length(STStrain); length(STStest); length(STS)

# Harmonics.
harmTrain <- tail(harmonics, nrow(impDF[impDF$date <  "2022-01-01",])); nrow(harmTrain)
harmTest  <- head(harmonics, nrow(impDF[impDF$date >= "2022-01-01",])); nrow(harmTest)

# Air temperature.
airTrain <- airvars[airvars$date <  "2022-01-01",]; nrow(airTrain)
airTest  <- airvars[airvars$date >= "2022-01-01",]; nrow(airTest)

# Flows
flowTrain <- sproatFlow[sproatFlow$date <  "2022-01-01",]; nrow(flowTrain)
flowTest  <- sproatFlow[sproatFlow$date >= "2022-01-01",]; nrow(flowTest)

# Test for equal lengths. Looks good.
all.equal(length(STStrain), nrow(flowTrain), nrow(harmTrain), nrow(airTrain), 266)
all.equal(length(STStest),  nrow(flowTest),  nrow(harmTest),  nrow(airTest),  52)

# TS Model selection -----------------------------------------------------------

# Define a funcion to test ARIMA with various covariate terms.
arm.xreg <- function(xreg) {
  # y = Somass temperatures.
  Arima(y = as.numeric(STStrain),
        # Optimal error structure defined earlier.
        order = order,
        seasonal = FALSE,
        # Covariates = Fourier terms and whatever else is defined.
        xreg = as.matrix(cbind(harmTrain, xreg)))
}


# Set up a list so the computation is vectorized. 
# All covariates and select combinations of covariates. 
xregs <- list(air1 = airTrain[,  2], air2 = airTrain[,2:3],
              air3 = airTrain[,2:4], sproat = flowTrain[,2],
              sproat2 = as.data.frame(poly(flowTrain$rsproat, 2)),
              harmonics = NULL, 
              air1spr1 = cbind(airTrain[,2], flowTrain[,2]),
              air1spr2 = cbind(airTrain[,2], 
                         as.data.frame(poly(flowTrain$rsproat, 2, raw = T))),
              air2Spr1 = cbind(airTrain[,2:3], flowTrain[,2]),
              air2Spr2 = cbind(airTrain[,2:3], 
                         as.data.frame(poly(flowTrain$rsproat, 2, raw = T))),
              air3Spr2 = cbind(airTrain[,2:4],
                         as.data.frame(poly(flowTrain$rsproat, 2, raw = T))))

# Apply to all list elements simultaneously. 
(xreg_mods <- lapply(xregs, arm.xreg))

# Extract list values and reorganize into a data.frame.
(mod_vals <-  as.data.frame(do.call(rbind,
           lapply(xreg_mods, accuracy))) %>% 
           mutate(AIC  = c(do.call(rbind, 
                  lapply(xreg_mods, AIC))),
           xreg = names(xregs),
           deltaAIC = round(AIC - min(AIC), 2)) %>% 
    relocate("xreg", "AIC", "deltaAIC") %>% 
    # Present lowest relative AIC first.
    arrange(deltaAIC) %>% 
    `rownames<-`(., c(seq(1, nrow(.), 1))))

write.csv(mod_vals, "arima_models.csv", row.names = F)

# Isolate the selected model. Lowest AIC within 1% and most parsimonious. 
(optMod <- xreg_mods[[mod_vals[mod_vals$xreg == "air1", 1]]])

arimaDF <- data.frame(obs   = as.numeric(STStrain),
                      date  = as.Date(airTrain[,1]),
                      h1m   = fitted(optMod, h = 1),
                      h2m   = fitted(optMod, h = 2),
                      h3m   = fitted(optMod, h = 3),
                      h4m   = fitted(optMod, h = 4)) %>% 
  mutate(month = month(date),
         year = year(date),
         MAE2  = abs(obs - h2m)) %>% 
  filter(month %in% c(4:9))


# Convert to LF for plotting purposes.
armLF <- arimaDF %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "h") %>%
  mutate(MAE = abs(obs - value),
         fh  = case_when(h == "h1m" ~ "FH = 1 week",
                         h == "h2m" ~ "FH = 2 weeks",
                         h == "h3m" ~ "FH = 3 weeks",
                         h == "h4m" ~ "FH = 4 weeks"),
         year = as.factor(year),
         upper = value + 1.96*sqrt(optMod$sigma2),
         lower = value - 1.96*sqrt(optMod$sigma2))

# Plot observed and predicted values by year and horizon.
ggplot(data = armLF,
       aes(x = date, y = obs)) +
  mytheme +
  geom_ribbon(aes(x = date, ymin = lower,
                  ymax = upper, fill = year),
              colour = NA,  alpha = 1/5) +
  geom_line(aes(x = date, y = value, colour = year),
            size = 1, alpha = 2/3, linetype=  1) +
  geom_point(size = 1.5, shape = 21,
             colour = "gray50", fill = "gray90") +
  facet_grid2(c("fh", "year"),
              scales = "free_x", independent = "x") +
  labs(x = NULL, y = "Somass temperature (°C)") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") +
  theme(legend.position = "none")

ggsave("plots/arima_acc.png", units = "px",
       width = 2500, height = 2000)


#Performance: Training ---------------------------------------------------------


# Define forecasting function for cross-validation.
fc <- function(y, h, xreg, newxreg) {
  
  # Parameters from original ARIMA models.
  # No seasonality, use Fourier terms from earlier instead.
  fit <- Arima(y, order =  order,
               seasonal =  FALSE,
               xreg     =  xreg)
  
  # Input values above. 
  # Splitting into train and test data is automatic.
  forecast(fit, xreg = newxreg, h = h) 
  
}

# Perform cross-validation. 
# Weekly Somass temperatures as time series, Fourier terms as covariates. 
# Looking ahead h = 1:4 weeks.
forcv <- tsCV(as.numeric(STStrain), fc,
          h = fh, xreg = as.matrix(cbind(harmTrain, 
                                  airTrain[,c(2)])))
head(forcv,  15) # Check formatting.

# Meteorological season definitions. 
seasons <- airTrain %>% 
  rownames_to_column(var = "index") %>% 
  mutate(q = lubridate::quarter(date, fiscal_start = 12))

# Isolate each season as an indexed variable.
wint <- as.numeric(as.vector(seasons[seasons$q == 1, "index"]))
spri <- as.numeric(as.vector(seasons[seasons$q == 2, "index"]))
summ <- as.numeric(as.vector(seasons[seasons$q == 3, "index"]))
fall <- as.numeric(as.vector(seasons[seasons$q == 4, "index"]))

slist <- list(wint, spri, summ, fall)


# # For formatting subsequent base plots.
# par(mfrow = c(3,1), mar = c(5,5,1,1))
# 
# # Calculate RMSE for h = 1:4 and plot.
# (cv_rmse <- apply(forcv, 2, FUN = function(x) sqrt(mean(x^2, na.rm = TRUE)))) 
# plot(cv_rmse, xlab = "Forecast horizon (weeks)", ylab = "RMSE", type = "b")
# 
# # Calculate absolute error for  h = 1:4 and plot.
# (cv_ae <- apply(forcv, 2, FUN = function(x) mean(abs(x), na.rm = TRUE)))
# plot(cv_ae, xlab = "Forecast horizon (weeks)", ylab = "MAE", type = "b")
# 
# # Calculate mean absolute percent error for  h = 1:4 and plot.
# (cv_ma <- apply(forcv, 2, FUN = function(x) 100*mean(abs(x)/STS, na.rm = TRUE)))
# plot(cv_ae, xlab = "Forecast horizon (weeks)", ylab = "MAPE", type = "b")


# Make an empty list.
# For each season, calculate cross-validation statistics. 
fl <- list()
for (i in 1:4) {
  
  # Populate list.
  season_sub <- slist[[i]]
  
  # Root mean square error, absolute error and mean absolute percent error.
  (cv_rmse <- apply(forcv[season_sub,], 2, FUN = function(x) sqrt(mean(x^2, na.rm = TRUE)))) 
  (cv_ae   <- apply(forcv[season_sub,], 2, FUN = function(x) mean(abs(x), na.rm = TRUE)))
  (cv_ma   <- apply(forcv[season_sub,], 2, FUN = function(x) 100*mean(abs(x)/STS, na.rm = TRUE)))
  
  # Bind into a dataframe 
  tb <- as.data.frame(rbind(cv_rmse, cv_ae, cv_ma)) %>% 
    rownames_to_column("metric") %>% 
    mutate_if(is.numeric, round, 3)
  
  # Add a factor for which season stats are for.
  tb$sind <- i
  tb$season <- case_when(i == 1 ~ "Winter",
                         i == 2 ~ "Spring",
                         i == 3 ~ "Summer",
                         i == 4 ~ "Fall")
  
  # Reformat dataframe.
  fl[[i]] <- tb[-6] %>% 
    pivot_longer(cols = c("h=1", "h=2", "h=3", "h=4"),
                 names_to = "Horizon") %>% 
    pivot_wider(values_from = value, names_from = metric)
  
}

# Read everything together and write to directory.
(fpl <- do.call(rbind, fl)); write.csv(fpl, "cross_val_seas.csv")

# How are errors distributed by season and FH?
summary(aov(cv_ma ~ season, data = fpl))
summary(aov(cv_ma ~ Horizon, data = fpl))



# Performance: Test ------------------------------------------------------------

# Run optimal selected ARIMA model on 2022 data. 
# Uses model parameters from training data, which are applied to test dataset.
(arm22 <- Arima(model = optMod, STStest, 
          xreg = as.matrix(cbind(harmTest, airTest[,2]))))

df22 <- data.frame(                        # Put input data and fitted test values in one dataframe. 
  date = airTest[,1],                      # Dates for forecasted values.       
  obs  = as.numeric(STStest),              # Observed values for 2022. 
  h1   = as.numeric(fitted(arm22, h = 1)), # FH = 1 week.
  h2   = as.numeric(fitted(arm22, h = 2)), # FH = 2 weeks.
  h3   = as.numeric(fitted(arm22, h = 3)), # FH = 3 weeks.
  h4   = as.numeric(fitted(arm22, h = 4))  # FH = 4 weeks.
) %>% 
  pivot_longer(cols = c(h1, h2, h3, h4),
               names_to  = "h",
               values_to = "Predicted") %>% 
  # Tidy labels for later plotting.
  mutate(residuals = obs - Predicted,
         seas = quarter(date, fiscal_start = 12),
         upper = Predicted + 1.96*sqrt(arm22$sigma2),
         lower = Predicted - 1.96*sqrt(arm22$sigma2),
         fh  = case_when(h == "h1" ~ "FH = 1 week",
                         h == "h2" ~ "FH = 2 weeks",
                         h == "h3" ~ "FH = 3 weeks",
                         h == "h4" ~ "FH = 4 weeks")) 

# Plot.
ggplot(data = df22, 
       aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              colour = NA, alpha = 2/5,
              fill = "skyblue") +
  geom_line(aes(y = Predicted), colour = "skyblue2",
            size = 1, alpha = 1, linetype  = 1) +
  geom_point(aes(y = obs), shape = 21, size = 1.5,
             colour = "gray50", fill = "gray90") +
  facet_wrap(~fh) +
  mytheme +
  scale_x_date(date_labels = "%b") +
  labs(x = NULL, y = "Somass River temperature (°C)")

ggsave("plots/accuracy2022_predobs.png", units = "px",
       width = 2500, height = 2000)
  
# Seasonal measurements of accuracy.
(acc22 <- df22 %>% 
    group_by(seas, fh) %>% 
    summarise(
      RMSE = round(sqrt(mean(residuals^2, na.rm = TRUE)), 3),
      MAE  = round(mean(abs(residuals), na.rm = TRUE), 3),
      MAPE = round(100*mean(abs(residuals)/obs, na.rm = TRUE), 3)))
write.csv(acc22, "accuracy2022.csv", row.names = FALSE)

# Year-round measurements of accuracy.
accuracy(STStest, as.numeric(fitted(arm22, h = 1)))
accuracy(STStest, as.numeric(fitted(arm22, h = 2)))
accuracy(STStest, as.numeric(fitted(arm22, h = 3)))
accuracy(STStest, as.numeric(fitted(arm22, h = 4)))

# Nonlinear model --------------------------------------------------------------

# Combine all relevant variables into one object.
all.vars <- merge(impDF, airtemp) %>% 
  merge(., flows[flows$name == "Sproat",]) %>% 
  mutate(month = month(date)) %>% 
  filter(year != 2022)

# Fit non linear least squares with automatic parameter selection.
fit <- nls(data = all.vars,
           wSom ~ SSlogis(mAirT, Asym, xmid, scal))


# Combine outputs into a single data frame.
jtest <- all.vars[,c("wSom", "mAirT", "year", "value", "date")] %>% 
  # Eq. 1 of Mohseni et al., 1999: Water Resources Research Vol. 34.
  mutate(Tw = coef(fit)[1]/(1 + exp((coef(fit)[2] - mAirT)/coef(fit)[3])),
         fit = fitted(fit))

# Test models above with polynomial of sproat flows too.
(mod0 <- lm(data = jtest, wSom ~ Tw)); summary(mod0)
(mod1 <- lm(data = jtest, wSom ~ Tw + value)); summary(mod1)
(mod2 <- lm(data = jtest, wSom ~ Tw + poly(value, 2))); summary(mod2)
(mod3 <- lm(data = jtest, wSom ~ Tw + poly(value, 3))); summary(mod3)
(mod4 <- lm(data = jtest, wSom ~ Tw + poly(value, 4))); summary(mod4)
(mod5 <- lm(data = jtest, wSom ~ value)); summary(mod5)
(mod6 <- lm(data = jtest, wSom ~ poly(value, 2))); summary(mod6)
(mod7 <- lm(data = jtest, wSom ~ poly(value, 3))); summary(mod7)
(mod8 <- lm(data = jtest, wSom ~ poly(value, 4))); summary(mod8)

# Combine models into list.
lmlist <- list(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
lapply(lmlist, AIC)

# Combine model outputs in tidy format.
lmDF <- as.matrix(lapply(lmlist, FUN = broom::glance) %>% 
  do.call(rbind, .) %>% 
  mutate(deltaAIC = round(AIC - min(AIC), 1),
         call = lapply(lmlist, FUN = function(x) x$call)) %>% 
  relocate(call, AIC, deltaAIC))

write.csv(lmDF[,-1], "multireg_summaries.csv", row.names = FALSE)

# Assess if residuals are distributed normally. 
hist(mod2$residuals); shapiro.test(mod2$residuals)

# Assess accuracy of nonlinear model.
nlmacc <- data.frame(res = mod2$model$wSom - mod2$fitted.values,
                     date = jtest$date) %>% 
                     mutate(q = quarter(date, fiscal_start = 12),
                     # Separate accuracy measurements by meteorological season.
                     season = case_when(q == 1 ~ "Winter", q == 2 ~ "Spring",
                                        q == 3 ~ "Summer", q == 4 ~ "Fall")) %>% 
  select(c("res", "season")) %>% group_by(season) %>% 
  summarise(RMSE = sqrt(mean(res^2, na.rm = TRUE)),
            ABSE = mean(abs(res), na.rm = TRUE),
            MAPE = 100*mean(abs(res)/mod2$model$wSom, na.rm = TRUE))
head(nlmacc); dim(nlmacc)

# Plot the derivation of parameters a, b and y.
# Have to input them manually for some reason otherwise it throws an error?
ggplot(data = jtest, aes(x = mAirT, y = wSom)) +
  geom_point(size = 2, alpha = 1/5) + mytheme  +
  labs(x = "Air temperature (°C)",
       y = "Somass temperature (°C)") +
  geom_segment(aes(x = 10.64905, xend = 10.64905,
                   y = 12.7, yend = 26.7)) +
  geom_segment(aes(x = 10.64905, xend = 10.64905,
                   y = 2.2, yend = 12.7),
               linetype = "dotted") +
  geom_segment(aes(x = 0, xend = 10.64905,
                   y = 20, yend = 12.7)) +
  geom_segment(aes(x = -5.4, xend = 26,
                   y = 25.45254, 
                   yend = 25.45254)) +
  geom_smooth(method = "nls", se = FALSE,
              formula = y ~ SSlogis(x, Asym, xmid, scal),
              colour = "blue2")  +
  coord_cartesian(clip = "off", 
                  ylim = c(3, 25),
                  xlim = c(-4, 24)) +
  theme(plot.margin = margin(t = 30, r = 20, 
                             l = 5, b = 5)) +
  annotate("text", label = "β", size = 5, 
           x = 10.64905, y = 27.8) +
  annotate("text", label = "α", size = 5,
           x = 26.5, y = coef(fit)[[1]] + 0.1) +
  annotate("text", label = "1/γ", size = 5,
           x = -1, y = 20.5)
  
ggsave("plots/mohseni_logistic.png", units = "px",
       width = 2000, height = 1500)

# Isolate outputs from optimal model. 
modout <- data.frame(obs = mod2$model$wSom,
                     fit = mod2$fitted.values,
                     dat = as.Date(jtest$date),
                     lwrPI = predict(mod2, interval = "prediction")[,2],
                     uprPI = predict(mod2, interval = "prediction")[,3],
                     lwrCI = predict(mod2, interval = "confidence")[,2],
                     uprCI = predict(mod2, interval = "confidence")[,3]) %>% 
  mutate(year = as.factor(year(dat)),
         # Calculate residuals. 
         res = -1*(obs - fit))

# Residual mean and standard deviation. 
round(mean(modout$res), 20); round(sd(modout$res), 3)

# Plot residuals and predicted/observed values. 
ggplot(data = modout) +
  geom_ribbon(aes(x = dat, ymin = lwrPI, 
                  ymax = uprPI, fill = year),
                  alpha = 1/5, colour = NA) +
  geom_line(aes(x = dat, y = fit,
                color = year),
            size = 1, alpha = 2/3) +
  geom_point(aes(x = dat, y = obs),
             size = 1.5, shape = 21, 
             fill = "gray90", colour = "black") +
  facet_wrap(~year, scales = "free") +
  labs(y = "Somass temperature (°C)", x = NULL) +
  scale_x_date(date_labels = "%b") + mytheme +
  theme(legend.position = "none")

ggsave("plots/nlm_ts_obs_fitted.png", width = 10, height = 6)

# Nonlinear model diagnostics plots.
# Fitted vs. observed values first.
(qq <- ggplot(data = modout, 
             aes(x = obs, y = fit)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(size = 2, shape = 21,
             colour = "black", fill = "gray90") + 
    mytheme + labs(x = "Observed temperatures (°C)",
                   y = "Fitted temperatures (°C)"))

# Histogram of residuals second. 
(rhis <- ggplot(data = modout) +
    geom_histogram(aes(res), colour = "black",
                   fill = "gray92") + mytheme +
    labs(x = "Residual (°C)", y = NULL))

# Combine and save.
cowplot::plot_grid(qq, rhis, align = "vh", ncol = 1)
ggsave("plots/nlm_diagnostics.png", width = 2000, 
       height = 2500, units = "px")



# 2022, nlm test ---------------------------------------------------------------

# Isolate 2022 data for modelling test series. 
som22 <- merge(impDF, dat[,c("date", "rAir")], by = "date") %>% 
  merge(., flows[flows$name == "Sproat",], by = "date") %>% 
  mutate(month = month(date)) %>% 
  filter(year == 2022)

# Fit logistic regression to 2022 data only. 
fit22 <- nls(data = som22, 
             wSom ~ SSlogis(rAir, Asym, xmid, scal))
coef(fit22); summary(fit22) # Retrieve parameters.

# Isolate variables and calculate Tw. 
j22 <- som22[,c("wSom", "rAir", "year", "value", "date")] %>% 
  # Eq. 1 of Mohseni et al., 1999: Water Resources Research Vol. 34.
  mutate(Tw = coef(fit22)[1]/(1 + exp((coef(fit22)[2] - rAir)/coef(fit22)[3])))

# Predicted values - mean and prediction interval. 
som22$pred <- as.numeric(predict(mod2, newdata = j22))
som22$lwr  <- as.numeric(predict(mod2, newdata = j22, interval = "prediction")[,2])
som22$upr  <- as.numeric(predict(mod2, newdata = j22, interval = "prediction")[,3])
# Residuals.
som22$res  <- som22$pred - som22$wSom
# Factor for year. 
som22$year <- as.factor(som22$year)
  
# Plot simple correlation. 
ggplot(data = j22, aes(x = wSom, y = Tw)) + geom_point() +
  labs(x = "Observed", y = "Fitted (Mohseni et al., 1998)")
summary(lm(data = j22, Tw ~ wSom))

# Residual histograms and other summaries. 
hist(abs(som22$res))
mean(abs(som22$res), na.rm = TRUE)
100*mean(abs(som22$res)/som22$wSom, na.rm = TRUE)

# Plot of 2022 observed vs. predicted.
ggplot(data = som22) +
  geom_ribbon(aes(x = date, ymin = lwr, ymax=upr),
              fill = "skyblue2", colour = NA, alpha = 1/5) +
  geom_line(aes(x = date, y = pred),
            size = 1, color = "skyblue2") +
  geom_point(aes(x = date, y = wSom),
             size = 2, shape = 21, 
             fill = "gray90", colour = "black") +
  mytheme + labs(x = NULL, y = "Somass temperature (°C)") +
  scale_x_date(date_labels = "%B") 

ggsave("plots/nlm_fitobs2022.png", units = "px",
       width = 2000, height = 1250)
  
# Accuracy metrics for test run on 2022 data.
(nlm22acc <- som22 %>% 
    mutate(q = quarter(date, fiscal_start = 12),
           # Separate accuracy measurements by meteorological season.
           season = case_when(q == 1 ~ "Winter", q == 2 ~ "Spring",
                              q == 3 ~ "Summer", q == 4 ~ "Fall")) %>% 
    group_by(season) %>% 
    summarise(ABSE = mean(abs(res), na.rm = TRUE),
              RMSE = sqrt(mean(res^2, na.rm = TRUE)),
              MAPE = 100*mean(abs(res)/wSom, na.rm = TRUE)))



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# Prediction intervals at thresholds -------------------------------------------

# Using ARIMA model from before to make forecast distributions.

# Prediction intervals for ARIMA model using simulations. 
# Not directly relevant to current manuscript, but likely relevant for future work.

nsim <- 1000L # 1k simulations. 
fh            # = 4 weeks. Defined earlier.

# Empty matrix to populate w/ for-loop.
future <- matrix(NA, nrow = fh, ncol = nsim) 

# For each of 1000 simulations, do the following:
for(i in seq(nsim)) {
  
  # Simulate the best-performing model. 
  future[,i] <- simulate(Arima(y = as.numeric(STS),
                               order    = order,
                               seasonal = FALSE,
                               xreg     = harmonics),
                         # 4 weeks of forecasted harmonics (~ newxreg).
                         xreg   = fourier(STS,
                                          K = bf,
                                          h = fh), 
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

# Visualize simulations as histograms.
(simhist <- ggplot(data = future_sims, aes(x = temp)) +
    geom_histogram(fill  = "gray95",
                   color = "gray30",
                   bins  = 50) +
    facet_wrap(. ~ h, scales = "free_x") +
    mytheme +
    labs(x = "Temperature (°C)", y = NULL))

ggsave("plots/simulated_tempdists.png", units = "px",
       width = 2000, height = 1500)

(sim_summ <- future_sims %>% 
    group_by(h) %>% 
    # For each forecasted week, get mean temperature,
    # and % of days (from simulation) above 18, 19, 20C. 
    summarise(mean = mean(temp, na.rm = TRUE),
              p18  = round(sum(temp > 18, na.rm = TRUE)/nsim * 100, 1),
              p19  = round(sum(temp > 19, na.rm = TRUE)/nsim * 100, 1),
              p20  = round(sum(temp > 20, na.rm = TRUE)/nsim * 100, 1)) %>% 
    mutate(date = max(impDF$date) + seq(7, 7*fh, 7)))

# Plot forecasts and time series together.
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


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################