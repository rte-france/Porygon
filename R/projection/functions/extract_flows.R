# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

extract_flows <- function(adequacy_data) {
  require(data.table)

  cols_to_keep <- c("Time", "Version", colnames(adequacy_data)[grepl("FLOW LIN", colnames(adequacy_data))])
  
  flows <- adequacy_data[, .SD, .SDcols = cols_to_keep]
  flows[, ":="(timeId = seq(from = 1, length.out = nrow(flows)), mcYear = 26, Time = with_tz(Time, "Europe/Paris"))]
  flows <- melt(flows, id.vars = c("Time", "Version", "mcYear", "timeId"), value.name = "FLOW LIN.", variable.name = "link", variable.factor = F)
  flows[, link := substr(link, start = 1, stop = nchar(link) - 10)]
  setnames(flows, "Time", "time")
  
  return(flows)
  
}

getAreas_adequacy <- function(adequacy_data, select = NULL, exclude = NULL){
  
  all_areas <- colnames(adequacy_data)[grepl("BALANCE", colnames(adequacy_data))]
  all_areas <- unique(sapply(all_areas, function(X){
    substr(X, start = 1, stop = nchar(X) - 8)
  }, USE.NAMES = F))
  
  if(is.null(select)){
    subset_areas <- all_areas
  } else {
    subset_areas <- all_areas[grepl(paste(paste0("^", areas, "$"), collapse = "|"), all_areas)]
  }
  
  if(!is.null(exclude)){
    areas_to_exclude <- all_areas[grepl(paste(paste0("^", exclude, "$"), collapse = "|"), all_areas)]
    subset_areas <- subset_areas[!subset_areas %in% areas_to_exclude]
  }
  
  return(subset_areas)
}

getLinks_adequacy <- function(flows, areas = NULL, exclude = NULL, internalOnly = F){
  
  all_links <- unique(flows$link)
  
  if(is.null(areas)){
    subset_links <- all_links
  } else {
    subset_links <- all_links[grepl(paste(areas, collapse = "|"), all_links)]
  }
  
  if(!is.null(exclude)){
    links_to_exclude <- all_links[grepl(paste(exclude, collapse = "|"), all_links)]
    subset_links <- subset_links[!subset_links %in% links_to_exclude]
  }
  
  if(internalOnly){
    strsplit_links <- strsplit(subset_links, split = " - ")
    grepl_strsplit <- sapply(strsplit_links, function(X){
      grepl(paste(paste0("^", areas, "$"), collapse = "|"), X[1]) & grepl(paste(paste0("^", areas, "$"), collapse = "|"), X[2])
    })
    subset_links <- subset_links[grepl_strsplit]
  }
  
  return(subset_links)
}

extract_balance <- function(countries, flows, links){
  
  balance <- data.table(country = countries)
  flow_reduced <- flows[link %in% links]
  
  balance[, balance := sapply(country, function(X){
    
    flow_reduced_country <- flow_reduced[grepl(X, link)]
    flow_reduced_country[, `FLOW LIN.` := ifelse(grepl(paste0("^", X), link), `FLOW LIN.`, -`FLOW LIN.`)]
    sum(flow_reduced_country$`FLOW LIN.`)
    
  })]
  
  return(balance)
  
}

get_links_to_area <- function(area, areas_all, areas_fb, flows){
  
  if(area %in% areas_fb){
    setdiff(getLinks_adequacy(flows, areas = areas_all, internalOnly = T),
            getLinks_adequacy(flows, areas = areas_all[areas_all != area], internalOnly = T))
  } else {
    setdiff(getLinks_adequacy(flows, areas = c(areas_fb, area), internalOnly = T),
            getLinks_adequacy(flows, areas = areas_fb, internalOnly = T))
  }
  
}

get_areas_linked_to_area <- function(area, areas_all, areas_fb, flows){
  
  links_to_area <- get_links_to_area(area, areas_all, areas_fb, flows)
  corresponding_areas <- unique(unlist(sapply(links_to_area, strsplit, " - ")))
  corresponding_areas <- corresponding_areas[corresponding_areas != area]
  return(corresponding_areas)
  
}
