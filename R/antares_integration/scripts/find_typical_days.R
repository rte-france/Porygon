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
library(tidyr)

### Definition of the second member ###
ERAA_study_path <- "D:/Users/user/MyPath/ERAA_NTC_results/"
rf_winter <- readRDS("R/clustering/outputs/rf_winter.rds")
rf_summer <- readRDS("R/clustering/outputs/rf_summer.rds")
clusters <- readRDS("R/clustering/outputs/clusters_ERAA_2021.RDS")
variables <- rownames(rf_winter$importance)
variables <- variables[!grepl("mean", variables)]
areas <- unique(sapply(strsplit(variables, split = "_"), function(X){X[1]}))

# Extraction of simulation data
opts <- setSimulationPath(ERAA_study_path, simulation = "input")
opts$timeIdMax <- 8760

areas_with_swell <- getAreas(select = "hydro_swell")
areas_with_swell <- areas_with_swell[grepl(paste(areas, collapse = "|"), areas_with_swell)]
areas_tot <- c(areas, areas_with_swell)

inputData <- readInputTS(load = areas_tot, ror = areas_tot, wind = areas_tot, solar = areas_tot, opts = opts)
non_variable_columns <- colnames(inputData$load)[!colnames(inputData$load) == "load"]
inputData <- Reduce(function(x, y) merge(x, y, by = non_variable_columns, all = TRUE), inputData)
inputData[, time := as.Date(time)]

# Adding swell areas to the real areas
inputData[grepl("hydro_swell", area), area := sapply(area, function(X){
  strsplit(X, "_")[[1]][2]
})]
inputData <- inputData[, .(load = sum(load, na.rm = T), ror = sum(ror, na.rm = T), wind = sum(wind, na.rm = T), solar = sum(solar, na.rm = T)), by = non_variable_columns]


adequacy_variables <- c("load", "wind", "solar", "ror")

inputData_daily <- inputData[, lapply(.SD, mean), by = list(area, time, tsId), .SDcols = adequacy_variables]
inputData_daily <- dcast(inputData_daily, time+tsId~area, value.var =  adequacy_variables)

colnames(inputData_daily) <- sapply(colnames(inputData_daily), function(X){
  if(!grepl("_", X)){
    X
  } else {
    split <- strsplit(X, split = "_")[[1]]
    variable <- split[1]
    if(variable == "ror"){variable <- "h_ror"}
    country <- split[2]
    paste0(country, "_", variable)
  }
})

country_variables <- colnames(inputData_daily)[grepl(paste(adequacy_variables, collapse = "|"), colnames(inputData_daily))]
inputData_daily[, paste0(country_variables, "_normalized") := lapply(.SD, scale), .SDcols = country_variables]

for(variable in adequacy_variables){
  cols_variables <- colnames(inputData_daily)[grepl(variable, colnames(inputData_daily)) & grepl("normalized", colnames(inputData_daily))]
  inputData_daily[, paste0(variable, "_mean_core_normalized") := rowMeans(.SD), .SDcols = cols_variables]
  inputData_daily[, paste0(variable, "_mean_core_normalized") := lapply(.SD, scale), .SDcols = paste0(variable, "_mean_core_normalized")]
}
setnames(inputData_daily, "ror_mean_core_normalized", "h_ror_mean_core_normalized")
cols_to_keep <- colnames(inputData_daily)[grepl(paste(c(non_variable_columns, "normalized"), collapse = "|"), colnames(inputData_daily))]
inputData_for_rf <- inputData_daily[, .SD, .SDcols = cols_to_keep]

# Application of the RF models on the input data
idTypDays <- fread("R/antares_integration/inputs/IdTypDays.csv")
idTypDays[, Date := as.POSIXct(Date, format = "%d/%m/%Y")]
idTypDays <- unique(merge(idTypDays, clusters[, .(date, clustering)], by.x = "Date", by.y = "date"))

summer_days <- inputData_for_rf[time >= "2018-04-01" & time <= "2018-09-30"]
summer_prediction <- predict(object = rf_summer, newdata = summer_days)
summer_days <- summer_days[, cluster := summer_prediction]
summer_days <- merge(summer_days, idTypDays[, .(clustering, idDayType)], by.x = "cluster", by.y = "clustering")
summer_days <- summer_days[order(tsId, time)]

winter_days <- inputData_for_rf[time < "2018-04-01" | time > "2018-09-30"]
winter_prediction <- predict(object = rf_winter, newdata = winter_days)
winter_days <- winter_days[, cluster := winter_prediction]
winter_days <- merge(winter_days, idTypDays[, .(clustering, idDayType)], by.x = "cluster", by.y = "clustering")
winter_days <- winter_days[order(tsId, time)]

all_days <- rbind(summer_days, winter_days)
all_days <- all_days[, .(tsId, time, idDayType)] %>% .[order(tsId, time)]

all_days_txt <- dcast(all_days, formula = time~tsId, value.var = "idDayType")
setnames(all_days_txt, "time", "Date")

saveRDS(all_days, "R/antares_integration/outputs/ts.rds")
fwrite(all_days_txt, "R/antares_integration/outputs/ts.txt", sep = "\t")
