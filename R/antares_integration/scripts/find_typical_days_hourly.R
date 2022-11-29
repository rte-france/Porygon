# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())

library(lubridate)
library(antaresRead)
library(data.table)
library(randomForest)
library(dplyr)

### Definition of the second member ###
ERAA_study_path <- "D:/Users/user/MyPath/ERAA_NTC_results/"
rf_winter <- readRDS("R/clustering/outputs/rf_winter_2022.rds")
rf_summer <- readRDS("R/clustering/outputs/rf_summer_2022.rds")
clusters <- readRDS("R/clustering/outputs/clusters_ERAA_2022.RDS")
variables <- rownames(rf_winter$importance)
areas <- unique(sapply(strsplit(variables, split = "_"), function(X){X[1]}))

# Extraction of simulation data
opts <- setSimulationPath(ERAA_study_path, simulation = "input")
opts$timeIdMax <- 8760
inputData <- readInputTS(load = areas, ror = areas, wind = areas, solar = areas, opts = opts)
non_variable_columns <- colnames(inputData$load)[!colnames(inputData$load) == "load"]
inputData <- Reduce(function(x, y) merge(x, y, by = non_variable_columns, all = TRUE), inputData)
inputData[, time := as.Date(time)]

adequacy_variables <- c("load", "wind", "solar", "ror")

inputData_daily <- inputData[, lapply(.SD, mean), by = list(area, timeId, tsId), .SDcols = adequacy_variables]
inputData_daily <- dcast(inputData_daily, timeId+tsId~area, value.var =  adequacy_variables)

colnames(inputData_daily) <- sapply(colnames(inputData_daily), function(X){
  if(!grepl("_", X)){
    X
  } else {
    split <- strsplit(X, split = "_")[[1]]
    variable <- split[1]
    country <- split[2]
    if (variable == "ror") {variable <- "h_ror"}
    paste0(country, "_", variable)
  }
})

country_variables <- colnames(inputData_daily)[grepl(paste(adequacy_variables, collapse = "|"), colnames(inputData_daily))]
inputData_daily[, paste0(country_variables, "_normalized") := lapply(.SD, scale), .SDcols = country_variables]
inputData_daily[is.na(inputData_daily)] <- 0

cols_to_keep <- colnames(inputData_daily)[grepl(paste(c(non_variable_columns, "normalized"), collapse = "|"), colnames(inputData_daily))]
inputData_for_rf <- inputData_daily[, .SD, .SDcols = cols_to_keep]

# Application of the RF models on the input data
clusters[, date := as.POSIXct(date, format = "%Y-%m-%d_%H")]
idTypDays <- fread("R/antares_integration/inputs/IdTypDays.csv")
idTypDays[, Date := as.POSIXct(Date, format = "%d/%m/%Y")]
idTypDays <- unique(merge(idTypDays, clusters[, .(date, clustering)], by.x = "Date", by.y = "date"))

summer_days <- inputData_for_rf[timeId >= 2161 & timeId <= 6552]
summer_prediction <- predict(object = rf_summer, newdata = summer_days)
summer_days <- summer_days[, cluster := summer_prediction]
summer_days <- merge(summer_days, idTypDays[, .(clustering, idDayType)], by.x = "cluster", by.y = "clustering")
summer_days <- summer_days[order(tsId, timeId)]

winter_days <- inputData_for_rf[timeId < 2161 | timeId > 6552]
winter_prediction <- predict(object = rf_winter, newdata = winter_days)
winter_days <- winter_days[, cluster := winter_prediction]
winter_days <- merge(winter_days, idTypDays[, .(clustering, idDayType)], by.x = "cluster", by.y = "clustering")
winter_days <- winter_days[order(tsId, timeId)]

all_days <- rbind(summer_days, winter_days)
all_days <- all_days[, .(tsId, time, idDayType)] %>% .[order(tsId, time)]
saveRDS(all_days, "R/antares_integration/outputs/ts.rds")
