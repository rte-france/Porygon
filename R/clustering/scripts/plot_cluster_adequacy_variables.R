# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

library(data.table)
library(stringr)
library(lubridate)
library(plotly)
source("R/clustering/funs/extract_adequacy_variables.R")
source("R/clustering/funs/clusterize_days.R")

# CLUSTERING #
path_flows <- "data/ERAA/year26/calcul_metrix_y26.csv.gz"
seasons <- data.table(season = c("winter", "summer"),
                      season_debut = c("2001-10-01", "2001-04-01"),
                      season_end = c("2001-03-31", "2001-09-30"),
                      nb = c(2, 2))
# read flows
flows <- fread(path_flows, dec = ".", encoding="UTF-8") %>% as.data.frame()
flows$Time <- with_tz(flows$Time, tzone = "UTC")

clusters <- clusterize_days(flows, nb_cluster = seasons, seasons = seasons)
clusters[, date := as.POSIXct(substring(date, 1, unlist(lapply(gregexpr(pattern = '_y', date), max))), tz = "UTC")]


# ADEQUACY DATA #

season = "winter"

input_path <- "data/ERAA/year26/20210415-1024eco-year_26.csv.gz"
adequacy_variables <- c("load", "wind", "solar", "balance", "h. ror")
countries <- c("fr", "be", "de", "nl", "hu", "be", "pl", "si", "sk", "ro", "cz", "hr")

input <- fread(input_path)

input <- input %>% 
  mutate(CORE00_LOAD = fr00_LOAD + be00_LOAD + de00_LOAD + at00_LOAD + nl00_LOAD + hr00_LOAD + cz00_LOAD + hu00_LOAD + pl00_LOAD + ro00_LOAD + si00_LOAD + sk00_LOAD,
         CORE00_WIND = fr00_WIND + be00_WIND + de00_WIND + at00_WIND + nl00_WIND + hr00_WIND + cz00_WIND + hu00_WIND + pl00_WIND + ro00_WIND + si00_WIND + sk00_WIND,
         CORE00_SOLAR = fr00_SOLAR + be00_SOLAR + de00_SOLAR + at00_SOLAR + nl00_SOLAR + hr00_SOLAR + cz00_SOLAR + hu00_SOLAR + pl00_SOLAR + ro00_SOLAR + si00_SOLAR + sk00_SOLAR,
         'CORE00_H. ROR' = `fr00_H. ROR` + `be00_H. ROR` + `de00_H. ROR` + `at00_H. ROR` + `nl00_H. ROR` + `hr00_H. ROR` + `cz00_H. ROR` + `hu00_H. ROR` + `pl00_H. ROR` + `ro00_H. ROR` + `si00_H. ROR` + `sk00_H. ROR`) %>% 
  mutate(CORE00_NETLOAD = CORE00_LOAD - CORE00_SOLAR - CORE00_WIND - `CORE00_H. ROR`)


# Extract daily variables
daily_variables <- extract_adequacy_variables(input, countries, adequacy_variables, daily_mean = T) %>% as.data.table()
daily_variables_CORE <- extract_adequacy_variables(input, 'CORE', c("wind", "solar", "netload"), daily_mean = T)


# Match clusters and adequacy variables
clusters_variables <- merge(clusters, daily_variables, by="date") 
clusters_variables_CORE <- merge(clusters, daily_variables_CORE, by="date")


# Boxplots
clusters_variables <- clusters_variables %>% filter(grepl(season, clustering))
clusters_variables_CORE <- clusters_variables_CORE %>% filter(grepl(season, clustering))

clusters_variables_draw <- clusters_variables %>% filter(grepl("_LOAD", variable))
clusters_variables_draw <- clusters_variables_CORE %>% filter(grepl("_WIND", variable)|grepl("_NETLOAD", variable)|grepl("_SOLAR", variable))

plot_ly(clusters_variables_draw, x = ~clustering, y = ~relative_mean, color = ~variable, type = "box") %>%
  layout(boxmode = "group", yaxis = list(title = "Relative mean (p.u.)"), xaxis = list(title = "Cluster name"))


