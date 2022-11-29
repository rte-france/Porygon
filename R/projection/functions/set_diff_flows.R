# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

set_diff_flows <- function(PLAN, flows, areas_fb, hubDrop = "nl00", ptdf_to_ignore = "ptdfALEGRO"){
  
  PLAN <- copy(PLAN)
  correspondance_ptdf <- .get_correspondance_ptdf()
  
  # Core PTDF calculation by removing the hubdrop
  ptdf_fb <- correspondance_ptdf[area %in% areas_fb, ptdf]
  ptdf_fb <- intersect(ptdf_fb, colnames(PLAN))
  ptdf_to_remove <- correspondance_ptdf[area == hubDrop, ptdf]
  
  columns_fb <- PLAN[, lapply(.SD, function(X) X - get(ptdf_to_remove)), .SDcols = setdiff(ptdf_fb, ptdf_to_remove)]
  colnames(columns_fb) <- paste0(colnames(columns_fb), "-", ptdf_to_remove)
  columns_fb <- cbind(columns_fb, PLAN[, ..ptdf_to_remove]) #We re-add the PTDF which will be removed later, it can be used to calculated adjusted AHC PTDFs
  
  ahc_ptdfs <- setdiff(colnames(PLAN)[grepl("ptdf", colnames(PLAN))], c(ptdf_fb, ptdf_to_ignore))
  ahc_countries <- correspondance_ptdf[ptdf %in% ahc_ptdfs]
  
  # Core PTDF calculation by removing AHC areas
  ahc_countries[, core_country := sapply(area, function(X){
    
    if(grepl("^-", X)){
      str_remove(X, "-")
    } else if(grepl("-", X)){
      strsplit(X, split = "-")[[1]][2]
    } else {
      linked_areas <- get_areas_linked_to_area(X, areas_all, areas_fb, flows)
      if(any(grepl(paste0(X, "-"), ahc_countries$area))){ # We remove areas which have another bilateral PTDF
        bilateral_areas <- ahc_countries[grepl(paste0(X, "-"), area), area]
        bilateral_areas <- sapply(bilateral_areas, function(Y){
          strsplit(Y, "-")[[1]][2]
        })
        linked_areas <- setdiff(linked_areas, bilateral_areas)
      }
      linked_areas
    }

  })]
  
  columns_ahc <- data.table()
  
  for(i in 1:nrow(ahc_countries)){
    ahc_country <- ahc_countries[i, area]
    ahc_ptdf <- ahc_countries[i, ptdf]
    core_countries <- unlist(ahc_countries[i, core_country])
    core_ptdfs <- correspondance_ptdf[area %in% core_countries, ptdf]
    core_ptdfs <- intersect(core_ptdfs, ptdf_fb)
    
    for(core_ptdf in core_ptdfs){
      core_ptdf_subtracted <- PLAN[, ..core_ptdf] - PLAN[, ..ahc_ptdf]
      columns_ahc <- cbind(columns_ahc, core_ptdf_subtracted)
      colnames(columns_ahc)[ncol(columns_ahc)] <- paste0(core_ptdf, "-", ahc_ptdf)
    }
  }
  
  # Concatenation
  not_ptdf_cols <- colnames(PLAN)[!grepl("ptdf", colnames(PLAN))]
  PLAN_diff <- PLAN[, .SD, .SDcols = not_ptdf_cols]
  PLAN_diff <- cbind(PLAN_diff, columns_fb, columns_ahc, PLAN[, ..ptdf_to_ignore])
  PLAN_diff <- PLAN_diff[, !..ptdf_to_remove]
  
  return(PLAN_diff)
  
}
