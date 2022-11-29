# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

require(data.table)
require(dplyr)
require(lubridate)
require(NbClust)
require(factoextra)
require(plotly)

source("R/clustering/funs/clustering_statistics.R")


#' Perform clustering on all classes
#'
#' @param path_list_intercos Path of the file containing the list of CNEs
#' @param path_flow_output  Path of the file containing the load flow output
#' @param heures_clust Hours on which the clustering is performed
#' @param cluster_nb A data table containing to columns:
#' - "class" contains the class names
#' - "nb" contains the number of clusters to be created per class
#' @return A table containing all the days of the period, the cluster they belong to, and the associated typical day
#' @export
#'
#' @examples
clusterize_days <- function(flows, 
                            nb_cluster = NULL, 
                            seasons = NULL,
                            granularity='daily',
                            heures_clust = c(0:24), 
                            is_weekend = "all",
                            dist = "euclidean",
                            wss=F, 
                            dist_centroid=F) {
  
  clusters <- rbindlist(lapply(seasons[,season], function(s) {
    season_data <- seasons[season == s]
    cluster_nb_season <- nb_cluster[season == s, nb]
    clusters_season <- clusterize_days_raw(flows, season_data, cluster_nb = cluster_nb_season, granularity = granularity, dist = dist, 
                                           is_weekend = is_weekend, heures_clust = heures_clust, wss = wss, dist_centroid = dist_centroid)
    clusters_season <- clusters_season[, clustering := paste0(s, clustering)]
  }))
  
  return(clusters)
}

clusterize_days_stats <- function(flows, 
                                  nb_cluster_max = 10, 
                                  season = NULL,
                                  gap=F,
                                  dist='euclidean',
                                  heures_clust = c(0:24), 
                                  granularity='daily', 
                                  is_weekend = "all") {
  
  flow = .create_temporal_index(flows %>% as.data.table(), granularity)
  flow_season_type <- .create_flow(flow, season, is_weekend, heures_clust, granularity)
  flow_season_type_aplat <- .create_flow_aplat(flow_season_type, granularity)
  visualize_clustering_statistics(flow_season_type_aplat[, -c("date")], season, nmax = nb_cluster_max, dist=dist, gap = gap, is_weekend)
}

clusterize_days_nbclust <- function(flows,  
                        nb_cluster_max = 10, 
                        season = NULL,
                        heures_clust = c(0:24),
                        granularity='daily',
                        is_weekend = "all") {
  
  flow = .create_temporal_index(flows %>% as.data.table(), granularity)
  flow = flow %>% na.omit()
  flow_season_type <- .create_flow(flow %>% as.data.table(), season=season, granularity=granularity)
  flow_season_type_aplat <- .create_flow_aplat(flow_season_type, granularity=granularity)
  fviz_nbclust(NbClust(flow_season_type_aplat %>% 
                  select(-date) %>% rename_with(.fn = function(x) str_replace(x, " ", "_")) %>% 
                  na.omit(), 
                distance = "euclidean", min.nc = 2, max.nc = nb_cluster_max, method = "kmeans",
                index=c("kl", "ch", "hartigan", 
                        "cindex", "db", "silhouette", "duda", "pseudot2", 
                        "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", 
                        "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", 
                        "sdindex", "dindex", "sdbw")))
}


cluster_plot <- function(flows, 
                         nb_cluster = NULL, 
                         season = NULL,
                         granularity='daily', 
                         is_weekend = "all", 
                         heures_clust = c(0:24)) {
  
  flow = .create_temporal_index(flows %>% as.data.table(), granularity)
  flow_season_type <- .create_flow(flow %>% as.data.table(), season, is_weekend, heures_clust, granularity)
  flow_season_type_aplat <- .create_flow_aplat(flow_season_type, granularity)
  
  fviz_cluster(cluster::pam(flow_season_type_aplat, nb_cluster), geom = "point", ellipse.type = "norm")
}


typical_days_plot <- function(cluster_data) {
  plot_ly(cluster_data %>% 
            mutate(Time = as.POSIXct(date, format = c("%Y-%m-%d_%H"))) %>% 
            filter(yday(Time)<=360) %>% 
            mutate(date = round(yday(Time)-1 + hour(Time)/24, 2)),
          type = 'barpolar') %>%
    add_trace(r = 1,
              theta =  ~date,
              marker = list(line = list(width=0)),
              color = ~clustering,
              base = 0.6) %>% 
    layout(polar = list(
      radialaxis = list(
        visible = F
      ),
      angularaxis = list(
        rotation = 90,
        direction = 'clockwise',
        showgrid = F, 
        tickvals = list(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330),
        ticktext = list('Jan.', 'Feb.', 'Mar.', 'Apr.',  'May', 'Jun.', 'Jul.',  'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.')
      )
    )
    )
}


clusterize_days_raw <- function(flow, season, cluster_nb, dist, granularity='daily', wss=F, dist_centroid=F, heures_clust = c(0:24), is_weekend = "all") {
  
  ###### create index between date + hour and timeId to find the timestep to import from Imagrid
  #####################################################################################################
  flow = .create_temporal_index(flow %>% as.data.table(), granularity)
  #index_temporel <- flow[, c(date)]
  flow_season_type <- .create_flow(flow, season, is_weekend, heures_clust, granularity)
  flow_season_type_aplat <- .create_flow_aplat(flow_season_type, granularity)
  
  ###### clustering and selection of medoids
  #####################################################################################################
  
  clustering_data = flow_season_type_aplat
  vect <- cluster::pam(clustering_data[, -c("date")], k=cluster_nb,  metric=dist)
  # debug
  # vect$clustering
  # vect$medoids
  
  ###### Typical days
  #tdays <- sapply(vect$medoids, function(X){flow_season_type_aplat$date[X]})
  tdays <- flow_season_type_aplat[vect$id.med, date]
  
  ###### index (timestamp) of the typical days to import
  #id_tdays <- lapply(tdays, function(X){index_temporel[date == X & heure %in% heures_clust,]})
  
  ###### clustering application : addition of the cluster to which each timestamp belong
  #####################################################################################################
  flow_season_type_aplat[, ":="(clustering = vect$clustering, tday = tdays[vect$clustering])]
  clustering_final <- flow_season_type_aplat[, c("date", "clustering", "tday")]
  
  clustering_final <- merge(flow_season_type, clustering_final, by = "date")
  
  if (wss | dist_centroid) {
    # Distance between matrix rows
    distances <- dist(flow_season_type_aplat[, -c("date")]) %>% as.matrix()
    if (wss) {
      clustering_final[, wss := sapply(date, function(X) {
        
        current_cluster <- clustering_final[date == X, clustering]
        current_date_index <- match(X, flow_season_type_aplat$date)
        other_cluster_members <- clustering_final[clustering == current_cluster, date]
        other_cluster_members_indexes <- sapply(other_cluster_members, match, table = flow_season_type_aplat$date)
        
        distances_other_members <- distances[current_date_index, other_cluster_members_indexes]
        wss <- sum(distances_other_members)/length(other_cluster_members_indexes)
      })]
    }
    if (dist_centroid) {
      clustering_final[, dist_centroid := sapply(date, function(X){
        
        row <- clustering_final[date == X]
        current_cluster <- row[,clustering]
        current_date_index <- match(X, flow_season_type_aplat$date)
        centroid_index <- match(row[,tday], flow_season_type_aplat$date)
        dist_centroid <- distances[current_date_index, centroid_index]
      })]
    }
  }
  
  return(clustering_final)
  
}


.create_temporal_index <- function(data, granularity) {
  data = data %>% mutate(heure = as.numeric(substr(Time, 12,13)))
  if (granularity == 'hourly') {
    data = data %>% mutate(date = paste0(substr(Time, 1,10),"_",heure,"_y", Version))
  } else {
    data = data %>% mutate(date = paste0(substr(Time, 1,10),"_0_y", Version))
  }
  data
}

.create_flow <- function(flow, season, is_weekend='all', heures_clust = c(0:24), granularity='daily') {
  
  season_debut = season[,season_debut]
  season_end = season[,season_end]
  
  ###### extraction of the lines on which the clustering will be performed from the results
  #####################################################################################################
  #flow_selec <- select(flow, c("Time", "Version", paste0("FLOW_", as.vector(ouvrages$ouvrages))))
  ouv <- colnames(flow)[grepl("FLOW_", colnames(flow))]
  
  #################################################################################################################
  
  flow_selec <- select(flow, c("Time", "date", "heure", "Version", which(names(flow) %in% ouv)))
  
  #ouvrages_final <- names(flow_selec)[-c('Time', 'data', 'Version')]
  
  ###### removing days with clock change, adding type of day
  #####################################################################################################
  
  flow_selec <- flow_selec[, jour:=substr(Time, 1,10)]
  
  # remove DST days
  if (granularity == 'daily') {
    flow_selec[, nb := length(heure), by = c('jour', 'Version')]
    flow_selec <- flow_selec[(nb == 24),]  
    flow_selec$nb <- NULL
  }
  #flow_selec <- flow_selec[!(jour %in% c("2000-10-29", "2001-03-25")),]
  flow_selec <- flow_selec[, type_jour:=wday(jour, label = TRUE, abbr = FALSE) ]
  
  
  ###### separation of seasons
  #####################################################################################################
  weekday <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi")
  weekend <- c("samedi", "dimanche")

  
  if(is_weekend == "all"){
    days_considered <- c(weekday, weekend)
  } else if(is_weekend == T){
    days_considered <- weekend
  } else {
    days_considered <- weekday
  }
  
  if(season_debut > season_end) {
    flow_season_type <- flow_selec[((jour >= season_debut) | (jour <= season_end)) & 
                                     (type_jour %in% days_considered),]
  } else {
    flow_season_type <- flow_selec[((jour >= season_debut) & (jour <= season_end)) &
                                     (type_jour %in% days_considered),]
  }
  
  flow_season_type <- flow_season_type[heure %in% heures_clust,]
  
  #####################################################################################################
  ###### removing useless columns
  #####################################################################################################
  #flow_season_type <- flow_season_type[, -c("Time","Version", "jour", "type_jour")]
  flow_season_type <- flow_season_type[, -c("jour", "type_jour")]
  return(flow_season_type)
  
}

.create_flow_aplat <- function(flow_season_type, granularity='daily') {

  ouvrages_final <- colnames(flow_season_type)[!colnames(flow_season_type) %in% c("Time", "date", "heure", "Version")]
 
   #####################################################################################################
  if (granularity == 'hourly') {
    flow_season_type_aplat <- flow_season_type[, -c('Time','heure','Version')]
  }
  else {
    flow_season_type_aplat <- dcast(flow_season_type[, -c('Time', 'Version')], date ~ heure, value.var = ouvrages_final)
  }
    # removing lines (days) with NAs
  flow_season_type_aplat <- na.omit(flow_season_type_aplat)
  
  return(flow_season_type_aplat)
}


# tday_to_date <-function(tday) {
#   if (length(str_locate_all(tday[[1]], pattern = "_")[[1]]) == 2) {
#     return(as.POSIXct(tday, tz = "UTC", format = "%Y-%m-%d"))
#   } else {
#     return(as.POSIXct(tday, tz = "UTC", format = "%Y-%m-%d_%H"))
#   }
#     
# }
