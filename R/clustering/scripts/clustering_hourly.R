# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(FactoMineR)
library(dplyr)
source("R/clustering/funs/clusterize_days.R")
source("R/clustering/funs/clustering_statistics.R")

#path_flows <- "R/clustering/inputs/calcul_metrix.csv"
#path_flows <- "data/ERAA/year26/calcul_metrix_y26.csv.gz"


path_flows <- "data/ERAA22/"

seasons <- data.table(season = c("winter", "summer"),
                      season_debut = c("2001-10-01", "2001-04-01"),
                      season_end = c("2001-03-31", "2001-09-30"))

# concatenate Metrix flows
# flows <- rbind(fread(paste0(path_flows, "LF_metrix_ERAA22_y7.csv.gz"), dec = ".", encoding = "UTF-8"),
#                fread(paste0(path_flows, "LF_metrix_ERAA22_y29.csv.gz"), dec = ".", encoding = "UTF-8"),
#                fread(paste0(path_flows, "LF_metrix_ERAA22_y33.csv.gz"), dec = ".", encoding = "UTF-8")) %>% as.data.frame()
# flows = flows %>% select(Time, Version, starts_with('FLOW_'), HVDC_ALEGRO, -c('FLOW_HVDC.FRES1', 'FLOW_HVDC.FRIT1'))
# write.csv(flows, gzfile(paste0(path_flows, "LF_metrix_ERAA22.csv.gz")), quote = F, row.names = F)

# read flows
flows <- fread(paste0(path_flows, "LF_metrix_ERAA22.csv.gz"), dec = ".", encoding = "UTF-8") %>% as.data.frame()
flows$Time <- with_tz(flows$Time, tzone = "UTC")
flows['FLOW_ALEGRO'] = flows['HVDC_ALEGRO']
flows['HVDC_ALEGRO'] = NULL

# keep only CNE
cne = read.csv(paste0(path_flows, "ERAA_CNE.txt"), encoding = "UTF-8", header = F, quote = "'")[,1]
cne_flows = unlist(lapply(cne, function(x) paste0("FLOW_", x)))
#flows = flows %>% select(any_of(c('Time', 'Version', 'FLOW_ALEGRO', cne_flows)))
flows = flows %>% select(any_of(c('Time', 'Version', cne_flows)))

# cne_fr = read.csv(paste0(path_flows, "FR_CNE.txt"), encoding = "UTF-8", header = F, quote = "'")[,1]
# cne_fr_flows = unlist(lapply(cne_fr, function(x) paste0("FLOW_", x)))
# flows = flows %>% select(Time, Version, any_of(cne_fr_flows))

# normalise
flows = flows %>% 
#  mutate(across(.cols = starts_with('FLOW_'), .fns = function(x) x/max(x)))
 mutate(across(.cols = starts_with('FLOW_'), .fns = function(x) (x-min(x))/(max(x)-min(x))))

# remove duplicates
flows = flows[, !duplicated(t(flows))] # remove duplicate columns

# Compute clusters
nb_cluster_per_season <- data.table(season = c("winter", "summer"), nb = c(2, 2))
clusters <- clusterize_days(flows, nb_cluster = nb_cluster_per_season, dist = "euclidean", seasons = seasons, granularity='hourly', dist_centroid=T)

# print typical days
clusters %>% 
  group_by(Version) %>% 
  mutate(id = row_number(Time)) %>% 
  filter(date == tday) %>% 
  select(id, Time, heure, Version, clustering, tday) %>% 
  mutate(Time = with_tz(Time), weekday = weekdays(Time))

clusters %>% group_by(clustering, tday) %>% summarise(count=n(), avg=mean(dist_centroid)) %>% 
  mutate(Time = with_tz(as.POSIXct(tday, format = c("%Y-%m-%d_%H"), tz="UTC")), weekday = weekdays(Time))

# Plot clusters
cluster_plot(flows, nb_cluster = 2, season = seasons[season == 'winter'], granularity='hourly')
cluster_plot(flows, nb_cluster = 2, season = seasons[season == 'summer'], granularity='hourly')


# Plot typical days
typical_days_plot(clusters) 

# Save clustering results
saveRDS(clusters, "R/clustering/outputs/clusters_ERAA_2022.RDS")

# PLOT CLUSTER DISTRIBUTION
# #########################
plot_ly(clusters %>% group_by(clustering) %>% arrange(dist_centroid) %>% mutate(id = row_number(dist_centroid)),
        x = ~id, y = ~dist_centroid, color = ~clustering, type = 'scatter', mode = 'lines'
)


# ############
#  STATISTICS
# ############
# compute clustering statistics for one season
clusterize_days_stats(flows, nb_cluster_max = 10, season = seasons[season == 'winter'], granularity='hourly')
clusterize_days_stats(flows, nb_cluster_max = 10, season = seasons[season == 'summer'], granularity='hourly')

clusterize_days_nbclust(flows, nb_cluster_max = 10, season = seasons[season == 'winter'], granularity='hourly')

# Plot and save clustering statistics for all seasons
output_dir_clustering <- "R/clustering/outputs/clustering_statistics_year_26/"
plot_all_clustering_statistics(output_dir_clustering, path_flows, seasons)


# Plot hourly relevance of clusters
plot_ly(clusters %>% 
          mutate(Time = as.POSIXct(date, format = c("%Y-%m-%d_%H"), tz="UTC")) %>% 
          mutate(hour = hour(Time)) %>%
          group_by(clustering, hour) %>% 
          summarise(count = n()), 
        x = ~hour, y =~count, color=~clustering, type = 'bar') %>%
  layout(title = 'Hours distribution in clusters', yaxis = list(title = 'Nb situations'), barmode = 'stack')

# Plot weekday relevance of clusters
plot_ly(clusters %>% 
          mutate(Time = as.POSIXct(date, format = c("%Y-%m-%d_%H"), tz="UTC")) %>% 
          mutate(weekday = weekdays(Time)) %>%
          group_by(clustering, weekday) %>% 
          summarise(count = n()), 
        x =~weekday, y =~count, color=~clustering, type = 'bar') %>%
  layout(title = 'Weekdays distribution in clusters', yaxis = list(title = 'Nb situations'), barmode = 'stack')

# Plot year relevance of clusters
plot_ly(clusters %>% 
          mutate(Version = as.factor(Version)) %>% 
          group_by(clustering, Version) %>% 
          summarise(count = n()), 
        x =~Version, y =~count, color=~clustering, type = 'bar') %>%
  layout(title = 'Years distribution in clusters', yaxis = list(title = 'Nb situations'), barmode = 'stack')

# Plot monthly relevance of clusters
plot_ly(clusters %>% 
          mutate(Time = as.POSIXct(date, format = c("%Y-%m-%d_%H"), tz="UTC")) %>% 
          mutate(month = month(Time)) %>%
          group_by(clustering, month) %>% 
          summarise(count = n()), 
        x =~month, y =~count, color=~clustering, type = 'bar') %>%
  layout(title = 'Months distribution in clusters', yaxis = list(title = 'Nb situations'), barmode = 'stack')

# Plot flows relevance of clusters
plot_ly(clusters %>%
          pivot_longer(cols=starts_with('FLOW_'), names_to='CNE', values_to='flow'),
        x =~clustering, y =~flow, type = 'box') %>%
  layout(title = 'CNE flows distribution in clusters', yaxis = list(title = 'CNE flows (normalised)'))


