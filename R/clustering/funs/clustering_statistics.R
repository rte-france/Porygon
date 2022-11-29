# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

visualize_clustering_statistics <- function(table, season, nmax, dist, gap=F, is_weekend = "all"){
  require(plotly)
  require(factoextra)
  require(crosstalk)
  require(stringr)
  require(stats)
  
  if(is_weekend == "all"){
    str_week <- ""
  } else if(is_weekend == T){
    str_week <- "weekend"
  } else {
    str_week <- "week"
  }
  title <- paste0(str_to_title(season), " ", str_week, " clustering: ")
  
  diss_matrix = stats::dist(table, method = dist) 
  
  # Elbow method
  message("Elbow method")
  fig1 <- ggplotly(fviz_nbclust(table, cluster::pam, method = "wss", diss=diss_matrix, k.max = nmax) +
                     labs(title = paste0(title, "Elbow method")))
  
  # Silhouette method
  message("Silhouette method")
  fig2 <- ggplotly(fviz_nbclust(table, cluster::pam, method = "silhouette", diss=diss_matrix, k.max = nmax) +
                     labs(title = paste0(title,"Silhouette method")))
  
  fig3 <- NULL
  if (gap) {
    # Gap statistic
    # nboot = 50 to keep the function speedy. 
    # recommended value: nboot= 500 for your analysis.
    # Use verbose = FALSE to hide computing progression.
    set.seed(123)
    fig3 <- ggplotly(fviz_nbclust(table, cluster::pam, method = "gap_stat", nboot = 100, diss=diss_matrix, k.max = nmax) +
                       labs(title = paste0(title, "Gap statistic method")))
  }
  if (is.null(fig3)) {
    fig <- bscols(fig1, fig2)
  }
  else {
    fig <- bscols(fig1, bscols(fig2, fig3))
  }
  return(fig)
  
}

plot_all_clustering_statistics <- function(output_dir, path_flows, seasons){
  
  flows <- fread(path_flows, dec = ".", encoding="UTF-8") %>% as.data.frame()
  flows$Time <- with_tz(flows$Time, tzone = "UTC")
  
  for(s in seasons[,season]){
    all_view <- clusterize_days_stats(flows, nb_cluster_max = 10, season = seasons[season == s], gap=T)
    output_path_all <- file.path(output_dir, paste0(season, ".html"))
    htmltools::save_html(all_view, output_path_all)
  }
  
}
