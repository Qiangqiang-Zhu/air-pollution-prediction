# Supplementary materials to *A comparison of statistical and machine learning models for spatio-temporal prediction of ambient air pollutant concentrations in Scotland*

## Data

### Abstract

The *Data* folder contains monitoring data, monthly aggregated data for modelling, model comparison results, and prediction results. The PCM modelled concentrations and data of other predictors can be downloaded from publicly available sources, which are hard to upload here due to the storage limitation.

### Description

- *Scotland air pollution monitor locations.csv* contains geographic information and type of the monitoring sites.
- *Monitoring Data.csv* is the raw data of daily measurements of NO2, PM10 and PM2.5.
- *Data - NO2.csv* , *Data - PM10.csv*, *Data - PM25.csv* contain monthly aggregated measured concentrations and corresponding predictors, which are used for modelling.
- *model comparison.RData* contains model performance metrics and cross-validation results of models compared.
- *Prediction Results.csv* is the prediction output dataset, covering predicted monthly concentrations across Scotland from 2016 to 2020.

## Code

The *Code* includes *R* scripts to initially explore the data, compare statistical models and machine learning methods, and use the best performing model to predict the monthly concentrations.
