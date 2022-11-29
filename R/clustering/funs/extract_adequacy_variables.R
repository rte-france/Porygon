# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

daily_means <- function(ts_variables) {
  # Mean is performed on UTC hours
  mean_daily_variables <- ts_variables
  mean_daily_variables <- mean_daily_variables[, date := as.POSIXct(format(Time, format = "%Y-%m-%d"), tz = "UTC")]
  mean_daily_variables <- mean_daily_variables %>% group_by(date, variable) %>% summarise(mean = mean(sum))
  
  # Relative mean = relative to other values
  mean_daily_variables <- mean_daily_variables %>% group_by(variable) %>% mutate(relative_mean := mean/mean(mean)) %>% na.omit()
  
  return(mean_daily_variables)
}


extract_adequacy_variables <- function(input, country_variables, adequacy_variables, daily_mean = T) {
  columns = apply(expand.grid(paste0(country_variables, "00"), toupper(adequacy_variables)), 1, paste, collapse="_")

  ts_variables <- input %>% 
    select(any_of(c("Time", "Version", columns)))
  
  ts_variables <- rbindlist(lapply(columns, function(X){
    country <- strsplit(X, "_")[[1]][1]
    variable <- strsplit(X, "_")[[1]][2]
    
    # Extraction of the nodes corresponding to the studied country
    #relevant_nodes <- nodes[grepl(paste0(".*", country, ".*"), nodes) & !grepl(".*hvdc.*", nodes)]
    #relevant_columns <- paste0(relevant_nodes, paste0("_",toupper(variable)))
    extracted_data <- input[, .(Time, Version, variable = X, sum = rowSums(.SD)), .SDcols = X]
    extracted_data
  }))
  
  if(daily_mean){
    daily_means(ts_variables)
  } else {
    return(ts_variables)
  }
}