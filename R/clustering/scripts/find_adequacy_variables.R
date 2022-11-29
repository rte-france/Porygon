# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())

library(data.table)
library(randomForest)
library(plotly)
library(dplyr)

source("R/clustering/funs/extract_adequacy_variables.R")

#clusters <- readRDS("R/clustering/outputs/clusters_ERAA_2022.RDS")

adequacy_variables <- c("load", "wind", "solar", "h. ror")
core_countries <- c("fr", "be", "de", "nl", "hu", "be", "pl", "si", "sk", "ro", "cz", "hr")

# ERAA21
#eod_input_path <- "data/ERAA/year26/20210415-1024eco-year_26.csv.gz"
#raw_data <- fread(eod_input_path)
#eod_data <- extract_adequacy_variables(raw_data, core_countries, adequacy_variables, daily_mean = F)

# ERAA22
eod_input_path <- "data/ERAA22/"
eod_data <- rbind(fread(paste0(eod_input_path, 'ERAA21_EVA_green_y7.csv.gz')) %>% extract_adequacy_variables(core_countries, adequacy_variables, daily_mean = F),
                  fread(paste0(eod_input_path, 'ERAA21_EVA_green_y29.csv.gz')) %>% extract_adequacy_variables(core_countries, adequacy_variables, daily_mean = F),
                  fread(paste0(eod_input_path, 'ERAA21_EVA_green_y33.csv.gz')) %>% extract_adequacy_variables(core_countries, adequacy_variables, daily_mean = F)
)

eod_data <- eod_data %>% rename(date = Time)

clusters_eod <- clusters %>% 
  mutate(date = as.POSIXct(date, tz = "UTC", format = "%Y-%m-%d_%H")) %>%
  select(date, Version, clustering) %>% 
  left_join(eod_data, by=c('Version', 'date'))

clusters_reduced <- clusters_eod[, .(date, clustering, variable, sum)]
clusters_formatted <- dcast(data = clusters_reduced, fun.aggregate = sum, formula = date + clustering ~ variable, value.var = "sum")
clusters_formatted <- janitor::clean_names(clusters_formatted)

cols_to_keep <- colnames(clusters_formatted)[!grepl("balance", colnames(clusters_formatted))]
clusters_formatted <- clusters_formatted[, ..cols_to_keep]

clusters_for_rf <- copy(clusters_formatted)
clusters_for_rf$date <- NULL
variables <- colnames(clusters_for_rf)[colnames(clusters_for_rf) != "clustering"]

# Addition of CORE variables
# for(variable in janitor::make_clean_names(adequacy_variables)) {
#   cols_variables <- colnames(clusters_for_rf)[grepl(variable, colnames(clusters_for_rf))]
#   clusters_for_rf[, paste0(variable, "_core") := rowSums(.SD), .SDcols = cols_variables]
#   clusters_for_rf[, paste0(variable, "_core_normalized") := lapply(.SD, scale), .SDcols = paste0(variable, "_core")]
# }

clusters_for_rf[, paste0(variables, "_normalized") := lapply(.SD, scale), .SDcols = variables]

plot_ly(clusters_for_rf %>% pivot_longer(cols=all_of(variables), names_to = "variable") %>% filter(grepl('load', variable)), 
        x = ~clustering, y = ~value, color = ~variable, type = "box") %>%
  layout(title = 'Load distribution within clusters', boxmode = "group", yaxis = list(title = "Value"), xaxis = list(title = "Cluster name"))

plot_ly(clusters_for_rf %>% pivot_longer(cols=all_of(variables), names_to = "variable") %>% filter(grepl('wind', variable)), 
        x = ~clustering, y = ~value, color = ~variable, type = "box") %>%
  layout(title = 'Wind generation distribution within clusters', boxmode = "group", yaxis = list(title = "Value"), xaxis = list(title = "Cluster name"))

plot_ly(clusters_for_rf %>% pivot_longer(cols=all_of(variables), names_to = "variable") %>% filter(grepl('solar', variable)), 
        x = ~clustering, y = ~value, color = ~variable, type = "box") %>%
  layout(title = 'Solar generation distribution within clusters', boxmode = "group", yaxis = list(title = "Value"), xaxis = list(title = "Cluster name"))

cols_to_keep <- colnames(clusters_for_rf)[grepl("normalized|clustering", colnames(clusters_for_rf))]
clusters_for_rf <- clusters_for_rf[, ..cols_to_keep]

non_na_cols <- lapply(clusters_for_rf, function(X){!all(is.na(X))})
non_na_cols <- which(non_na_cols == T)
clusters_for_rf <- clusters_for_rf[, .SD, .SDcols = non_na_cols]


# Random forest for one given season
season = "summer"
season = "winter"
clusters_for_rf_season <- clusters_for_rf[grepl(season, clustering)]
clusters_for_rf_season$clustering <- as.factor(clusters_for_rf_season$clustering)

# #Addition of residual load
# country_list <- unique(substr(colnames(clusters_for_rf)[grepl("_load", colnames(clusters_for_rf))], start = 1, stop = 4))
# for(country in country_list){
#   cols_country <- colnames(clusters_for_rf)[grepl(country, colnames(clusters_for_rf))]
#   load_col <- cols_country[grepl("load", cols_country)]
#   non_load_cols <- cols_country[cols_country != load_col]
#   clusters_for_rf[, paste0(country, "_residual_load_normalized") := get(load_col) - rowSums(.SD), .SDcols = non_load_cols]
#   clusters_for_rf[, paste0(country, "_residual_load_normalized") := lapply(.SD, scale), .SDcols = paste0(country, "_residual_load_normalized")]
# }

set.seed(50)
rf <- randomForest(formula = clustering ~ ., data = clusters_for_rf_season)
saveRDS(rf, paste0("R/clustering/outputs/rf_",season,"_2022.rds"))

variables <- colnames(clusters_for_rf)[colnames(clusters_for_rf) != "clustering"]
importance_variables <- data.table(variable = variables, importance = c(rf$importance)) %>% .[order(-importance)]
importance_variables$variable <- factor(as.character(importance_variables$variable), levels = unique(as.character(importance_variables$variable))[order(importance_variables$importance, decreasing = T)])

varImpPlot(rf, type=2)

plot_ly(data = importance_variables, x = ~variable, y = ~importance, type = "bar") %>% 
  layout(title = paste0("Relative importance of the adequacy variables for ", season))

confusion <- data.table(class = rownames(rf$confusion), error = rf$confusion[, ncol(rf$confusion)])
plot_ly(data = confusion, x = ~class, y = ~error, type = "bar") %>% 
  layout(title = paste0("Mean error rate per class in ", season))

#plot_rf_confusion(rf)
