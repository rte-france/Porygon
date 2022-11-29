# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

library(data.table)
library(stringr)
library(lubridate)
library(FactoMineR)
source("R/clustering/funs/clusterize_days.R")
source("R/clustering/funs/clustering_statistics.R")

#path_flows <- "R/clustering/inputs/calcul_metrix.csv"
path_flows <- "data/ERAA/year26/calcul_metrix_y26.csv.gz"


seasons <- data.table(season = c("winter", "summer"),
                      season_debut = c("2001-10-01", "2001-04-01"),
                      season_end = c("2001-03-31", "2001-09-30"))


# read flows
flows <- fread(path_flows, dec = ".", encoding="UTF-8") %>% as.data.frame()
flows$Time <- with_tz(flows$Time, tzone = "CET")

# DEBUG
#.create_flow(flows, seasons[season=="winter"])

# Compute clusters
nb_cluster_per_season <- data.table(season = c("winter", "summer"), nb = c(3, 3))
clusters <- clusterize_days(flows, nb_cluster = nb_cluster_per_season, seasons = seasons)

# print typical days
clusters %>% filter(date == tday)

# Plot typical days
typical_days_plot(clusters) 

# save clustering
saveRDS(clusters, "R/clustering/outputs/clusters_ERAA_2021.RDS")


# ############
#  STATISTICS
# ############
# compute clustering statistics for one season
clusterize_days_stats(flows, nb_cluster_max = 5, season = seasons[season == 'winter'])
clusterize_days_stats(flows, nb_cluster_max = 5, season = seasons[season == 'summer'])

# Plot and save clustering statistics for all seasons
output_dir_clustering <- "R/clustering/outputs/clustering_statistics_year_26/"
plot_all_clustering_statistics(output_dir_clustering, path_flows, seasons)


# Plot monthly relevance of clusters
plot_ly(clusters %>% 
          mutate(Time = as.POSIXct(date, format = c("%Y-%m-%d_%H"), tz="UTC")) %>% 
          mutate(month = month(Time)) %>%
          group_by(clustering, month) %>% 
          summarise(count = n()) %>% 
          pivot_wider(names_from = clustering, values_from = count), 
        x = ~month, y = ~winter1, type = 'bar', name = "winter1") %>%
  add_trace(y = ~winter2, name = "winter2") %>%
  add_trace(y = ~winter3, name = "winter3") %>%
  layout(yaxis = list(title = 'Nb situations'), barmode = 'stack')
