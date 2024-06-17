# Somass River temperature forecasting project

Time series analyses of Somass River temperature data. 
This project includes two main analyses:

## 1. Pre-season Forecasting 
This work focuses on identifying the potential drivers of annual temperature patterns that may be related to the severity of in-river conditions throughout the summer.

The directory `clim` contains analyses that investigate relationships between large-scale climatic variables and summer conditions in the Somass River. These analyses identify weak relationships between average summer river temperatures and several climatic indices, including the [Pacific Northwest Index](https://www.cbr.washington.edu/dart/pni), [Mount Cokely snow pack data](https://aqrt.nrs.gov.bc.ca/Data/Location/Summary/Location/3B02A/Interval/Latest) and several oceanographic indices courtesy of [NOAA](https://www.ncei.noaa.gov/access/monitoring/enso/sst). 

## 2. In-season Forecasting
The folder `weekly` includes models of weekly river temperatures as a function of i) seasonal terms and ii) air temperature data. An autoregressive integrated moving average ([ARIMA](https://otexts.com/fpp2/arima.html)) model is used to characterize five years of time series data and forecast future river temperatures. Accuracy tests and cross-validation analyses are also included to quantify model performance.

## 3. Using the tool
The `model` directory contains a simplified version of the script in `weekly`. All of the model fitting steps, diagnostics and comparative analyses have been removed to speed up the analysis. The intention here is for the `somass_tsmodel.R` script can be run from the command line without any R coding, where the script will output a figure consisting of observed temperatures up to the latest data provided and simulated future river temperatures over 4 weeks based on air temperature projections. To update the analysis, simply modify the files in the `data` directory. The file `alberni_air_temps.csv` is used to develop the model and can be updated by re-downloading data from various sources and appending those data to the bottom of this file. Similarly, the file `Spot and Mean Daily Water Temp Data At Depth 21.09.21.csv` contains data from Catalyst Paper Inc and must be manually updated. The file `future_air_temps` has weekly average air temperature estimates, which are available from a variety of sources all differing in reliability. 
