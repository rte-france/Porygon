# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())

library(fbClust)
library(fbAntares)
library(data.table)
library(dplyr)
#library(qpcR)
#library(hitandrun)
library(xlsx)
library(lubridate)
library(stringr)
library(tictoc)
source("R/projection/functions/order_vertices.R")
source("R/projection/functions/extract_flows.R")
source("R/projection/functions/remove_dimensions.R")
source("R/projection/functions/set_diff_flows.R")
source("R/projection/functions/domain_to_plan.R")
source("R/projection/functions/aggregate_nps.R")

# to use or not CB_ONLY domains
cb_only = ""
cb_only = "_CBONLY"
cb_only = "_CBONLY_380KV"

pattern = paste0("ERAA_PTDF", cb_only,"_ahc")


dirs <- list.dirs("data/ERAA/year26/", recursive = F)
adequacy_data_path <- "R/clustering/inputs/20210415-1024eco-year_26.csv"

areas_all <- c('al00', 'at00', 'ba00', 'be00', 'bg00', 'ch00', 'cy00', 'cz00', 'de00', 'dekf', 'dke1', 'dkkf', 'dkw1', 'ee00', 'es00', 
               'fi00', 'fr00', 'fr15', 'gr00', 'gr03', 'hr00', 'hu00', 'ie00', 'is00', 'itca', 'itcn', 'itcs', 'itn1', 'its1', 'itsa', 'itsi', 
               'lt00', 'lv00', 'me00', 'mk00', 'mt00', 'nl00', 'nom1', 'non1', 'nos0', 'pl00', 'pt00', 'ro00', 'rs00', 'se01', 'se02', 'se03', 'se04', 'si00', 'sk00', 'tn00', 'tr00', 'ua01', 'uk00', 'ukni' #, 'lub1', 'luf1', 'lug1', 'luv1', 'pli0', 'ple0'
)

areas_fb <- c('at00', 'be00', 'cz00', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ro00', 'si00', 'sk00' #, "pli0", "ple0"
)


elia_areas_to_remove <- c("at00", "pl00", "hr00", "si00", "cz00", "sk00", "hu00", "ro00", "itn1", "es00", "dkw1", "ch00",
                          "rs00", "bg00", "ba00", "se04-pl00", "dke1-de00", "dkw1-nl00", "nos0-nl00", "se04-de00", "nos0-de00",
                          "ua01-ro00", "ua01-sk00", "lt00-pl00", "ua01-hu00", "uk00-de00")

ahc_areas_to_remove <- c("itn1", "es00", "dkw1", "ch00", "rs00", "bg00", "ba00", "uk00-be00", "uk00-fr00", "uk00-nl00", "uk00-de00", "se04-pl00", 
                         "dke1-de00", "dkw1-nl00", "nos0-nl00", "se04-de00", "nos0-de00", "ua01-ro00", "ua01-sk00", "lt00-pl00", "ua01-hu00")

ch_areas_to_remove <- c('ba00', 'bg00', 'cz00', 'sk00', 'es00', 'hr00', 'hu00', 'pl00', 'ro00', 'rs00', 'dkw1',
                        "uk00-be00", "uk00-fr00", "uk00-nl00", "uk00-de00", "se04-pl00", 
                        "dke1-de00", "dkw1-nl00", "nos0-nl00", "se04-de00", "nos0-de00", "ua01-ro00", "ua01-sk00", "lt00-pl00", "ua01-hu00")

areas_to_remove = elia_areas_to_remove
#areas_to_remove = ch_areas_to_remove


adequacy_data <- fread(adequacy_data_path)
flows_tot <- extract_flows(adequacy_data)

### Calculation of reduced dimensions PLANs ###
for(dir in dirs){
  
  file_name <- list.files(dir, pattern = pattern)
  if (length(file_name) == 0) {
    next
  }
  
  message("processing domain file :", file_name)
  
  domains <- fread(file.path(dir, file_name), dec =",")
  
  if(!is.numeric(domains$FR.hub)){
    ptdf_cols <- colnames(domains)[grepl("hub|ahc", colnames(domains))]
    domains[, (ptdf_cols) := lapply(.SD, str_replace, ",", "."), .SDcols = ptdf_cols]
    domains[, (ptdf_cols) := lapply(.SD, as.numeric), .SDcols = ptdf_cols]
  }
  
  PLAN_raw <- domain_to_plan(domains, dir)
  
  domain_id <- strsplit(file_name, "_")[[1]][1]
  
  flows <- flows_tot[timeId == domain_id]
  
  # dekf re-integrated with de00
  flows[, link := str_replace(link, "dekf", "de00")]
  flows[link == "de00 - de00", `FLOW LIN.` := 0]
  
  # ple0 and pli0 re-integrated with pl00
  flows[, link := str_replace(link, "ple0", "pl00")]
  flows[, link := str_replace(link, "pli0", "pl00")]
  flows[link == "pl00 - pl00", `FLOW LIN.` := 0]
  
  # dkw1 re-integrated with dke1
  # flows[, link := str_replace(link, "dkw1", "dke1")]
  # flows[link == "dke1 - dke1", `FLOW LIN.` := 0]
  
  PLAN_less_dimensions <- fix_dimensions(PLAN_raw, flows, areas_to_remove = areas_to_remove, areas_fb, areas_all)
  
  
  # PLAN_shc <- fread(list.files(dir, pattern = "shc", full.names = T), dec = ",")
  # PLAN_shc <- PLAN_shc[, .(id = V1, ram_shc = Margin)]
  # diff_ahc_shc <- merge(PLAN_less_dimensions, PLAN_shc, all.x = T)
  # diff_ahc_shc[, diff_ahc_shc := ram - ram_shc]
  # plotly::plot_ly(diff_ahc_shc, x = ~diff_ahc_shc, type = "histogram")
  
  PLAN_diff <- set_diff_flows(PLAN_less_dimensions, flows, areas_fb)
  # PLAN_diff <- PLAN_diff[ram > 0]
  

  fwrite(PLAN_diff, file.path(dir, paste0("PLAN_diff_elia.csv")))
  
}

### Vertices calculation ###

for(dir in dirs){
  
  file_name <- list.files(dir, pattern = "ahc")
  domain_id <- strsplit(file_name, "_")[[1]][1]
  
  PLAN_diff_name <- list.files(dir, pattern = "PLAN_diff_elia")
  PLAN_diff <- fread(file.path(dir, PLAN_diff_name))
  
  cols_ptdf <- colnames(PLAN_diff)[grepl("ptdf", colnames(PLAN_diff))]
  PLAN_diff_ptdfs <- PLAN_diff[, ..cols_ptdf]
  PLAN_diff_ram <- PLAN_diff$ram
  
  constr <- list(constr = as.matrix(PLAN_diff_ptdfs), rhs = PLAN_diff_ram, dir = rep("<=", times = length(PLAN_diff_ram)))
  
  tic(paste0("Méthode vertexenum, domaine: ", domain_id))
  vertices <- vertexenum::enumerate.vertices(A = constr$constr, b = constr$rhs, warn_if_open = T) # Max : 7 dimensions
  toc()
  
  tic(paste0("Méthode Hitandrun, domaine: ", domain_id))
  vertices <- hitandrun::findVertices(constr) # Max : 7 dimensions
  toc()
  
  fwrite(vertices, file.path(dir, "vertices.csv"))
  
}

### Projection ###

removed_hub <- "ptdfNL"
aggregate_np <- TRUE

dirs <- c(dirs[1], dirs[7])

for(dir in dirs){
  
  print(dir)
  
  vertices_file <- list.files(dir, pattern = "elia_CBONLY_vertices.csv$")
  
  if(length(vertices_file) == 0){
    print("No vertices file for this dir")
    next
  }
  
  vertices <- fread(file.path(dir, vertices_file), dec = ".")
  if("V1" %in% colnames(vertices)){
    vertices$V1 <- NULL
    vertices$NL <- NULL
    colnames(vertices) <- sapply(colnames(vertices), function(X){
      if(X == "ALEGRO"){
        "ptdfALEGRO"
      } else {
        paste0("ptdf", X, "-", removed_hub)
      }
    })
  }
  
  if(file.exists(file.path(dir, "projections_elia_CBONLY.xlsx"))){
    file.remove(file.path(dir, "projections_elia_CBONLY.xlsx"))
  }
  
  if(!paste0(removed_hub, "-", removed_hub) %in% colnames(vertices)){ # On ajoute les sommets liés à NL
    related_ptdfs <- colnames(vertices)[grepl(paste0("-", removed_hub), colnames(vertices))]
    vertices[, paste0(removed_hub, "-", removed_hub) := -rowSums(.SD), .SDcols = related_ptdfs]
  }
  
  if(aggregate_np){
    file_name <- list.files(dir, pattern = "PTDF_ahc_presolved")
    domain_id <- strsplit(file_name, "_")[[1]][1]
    flows <- flows_tot[timeId == domain_id]
    
    # dekf re-integrated with de00
    flows[, link := str_replace(link, "dekf", "de00")]
    flows[link == "de00 - de00", `FLOW LIN.` := 0]
    
    # ple0 and pli0 re-integrated with pl00
    flows[, link := str_replace(link, "ple0", "pl00")]
    flows[, link := str_replace(link, "pli0", "pl00")]
    flows[link == "pl00 - pl00", `FLOW LIN.` := 0]
    
    vertices <- aggregate_nps(vertices, flows, removed_areas = elia_areas_to_remove, areas_fb, areas_all, adjust_fixed_nps = T, add_alegro_to_be = F)
    ptdfs_core <- colnames(vertices)[colnames(vertices) != "ptdfALEGRO"]
    
  } else {
    
    ptdfs_core <- colnames(vertices)[grepl(paste0("-", removed_hub), colnames(vertices))]
  }
  
  projection_vectors <- combn(ptdfs_core, 2, simplify = F)
  
  for(projection_vector in projection_vectors){
    
    projection_vector <- unlist(projection_vector)
    
    vertices_reduced <- vertices[, .SD, .SDcols = projection_vector]
    hull <- chull(vertices_reduced)
    hull_reduced <- vertices_reduced[hull,]
    hull_reduced <- order_vertices(hull_reduced)
    hull_reduced <- rbind(hull_reduced, hull_reduced[1])
    
    write.xlsx(hull_reduced, file = file.path(dir, "projections_elia_CBONLY.xlsx"), sheetName = paste(projection_vector, collapse = "_"), append = T, row.names = F)
  }
}

