# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

#The "areas_to_remove" must be area names, or in the zoneahc-zonecore format for bilateral ptdfs
#WARNING :HVDC handling is not effective

fix_dimensions <- function(PLAN, flows, areas_to_remove, areas_fb, areas_all){
  
  PLAN_adjusted_margins <- recalculate_margins(PLAN, flows, areas_to_remove, areas_fb, areas_all)
  PLAN_removed_dimensions <- remove_fixed_dimensions(PLAN_adjusted_margins, areas_to_remove)
  return(PLAN_removed_dimensions)
  
}

recalculate_margins <- function(PLAN, flows, areas_to_remove, areas_fb, areas_all, substract_bilateral_exchanges = F){
  require(data.table)
  
  PLAN <- copy(PLAN)
  correspondance_ptdf <- .get_correspondance_ptdf()
  
  for(current_area in areas_to_remove){
    print(current_area)
    ptdf <- correspondance_ptdf[area == current_area, ptdf]
    print(ptdf)
    # Checking if this is a bilateral exchange ptdf
    if(grepl("-", current_area)){
      bilateral_ptdf <- T
      related_area <- strsplit(current_area, split = "-")[[1]]
      ahc_area <- related_area[1]
      core_area <- related_area[2]
    } else {
      bilateral_ptdf <- F
    }
    
    # Recalculate the RAMs according to the NP of the area to remove
    if(current_area %in% areas_fb){
      print("in FB")
      links_all <- get_links_to_area(current_area, areas_all, areas_fb, flows)
      np_tot_area <- extract_balance(countries = current_area, links = links_all, flows = flows)$balance #Balance relative to all other areas
      
      PLAN[, ram := ram - np_tot_area * get(ptdf)]
      
    } else if(current_area == "uk00-fr00"){ # Specific UK-FR case
      print("uk-fr")
      links_uk_fr <- getLinks_adequacy(flows, areas = c("uk00", "fr00"), internalOnly = T)
      exchange_uk_fr <- extract_balance(countries = "uk00", links = links_uk_fr, flows = flows)$balance
      
      PLAN[, ram := ram - exchange_uk_fr * 3/4 * ptdfIFA1 - exchange_uk_fr * 1/4 * ptdfIFA2]
      
    } else if(bilateral_ptdf){
      print("bilateral")
      bilateral_link <- getLinks_adequacy(flows, areas = c(ahc_area, core_area), internalOnly = T)
      exchange_bilateral_area <- extract_balance(countries = ahc_area, links = bilateral_link, flows = flows)$balance #Balance relative to Core
      
      PLAN[, ram := ram - exchange_bilateral_area * get(ptdf)]
      
    } else {
      print("no FB, no bilateral")
      links_to_core <- getLinks_adequacy(flows, areas = c(current_area, areas_fb), internalOnly = T)
      np_ahc_area <- extract_balance(countries = current_area, links = links_to_core, flows = flows)$balance #Balance relative to all Core countries
      
      # If there is also a bilatéral ptdf for the area to remove and if the option is activated, the corresponding exchange is substracted from the total NP
      if(substract_bilateral_exchanges & any(grepl(paste0(current_area, "-"), correspondance_ptdf$area))){ 
        bilateral_area <- strsplit(correspondance_ptdf[grepl(paste0(current_area, "-"), area), area], "-")[[1]][2]
        bilateral_link <- getLinks_adequacy(flows, areas = c(current_area, bilateral_area), internalOnly = T)
        bilateral_exchange <- extract_balance(countries = current_area, links = bilateral_link, flows = flows)$balance
        np_ahc_area <- np_ahc_area - bilateral_exchange
      }
      
      PLAN[, ram := ram - np_ahc_area * get(ptdf)]
    
    }
    
    print(PLAN[id %in% c(253, 263), ram])
    
  }
  
  # Recalculating NPs of the areas which are kept
  
  removed_areas <- sapply(areas_to_remove, function(X){
    if(grepl("-", X)){
      return(strsplit(X, "-")[[1]][1])
    } else {
      return(X)
    }
  })
  
  for(removed_area in areas_to_remove){
    
    print(paste0("Examining remaining areas related to ", removed_area))
    
    if(grepl("-", removed_area)){ # If this is a bilateral area, only the NP of the linked area is adjusted
      first_area <- strsplit(removed_area, "-")[[1]][1]
      other_area <- strsplit(removed_area, "-")[[1]][2]
      links_to_other_countries <- getLinks_adequacy(flows, areas = c(first_area, other_area), internalOnly = T)
      balances_other_countries <- extract_balance(countries = setdiff(other_area, removed_areas), flows = flows, links = links_to_other_countries)
      balances_other_countries <- balances_other_countries[balance != 0]
      external_countries <- balances_other_countries$country
      
    } else { # If this is not a bilateral area, all related areas are retrieved
      links_to_other_countries <- get_links_to_area(area = removed_area, areas_all, areas_fb, flows)
      balances_other_countries <- extract_balance(countries = setdiff(areas_all, removed_areas), flows = flows, links = links_to_other_countries)
      balances_other_countries <- balances_other_countries[balance != 0]
      external_countries <- balances_other_countries$country
      
      # If a bilateral area also exist, we don't change the corresponding flows
      if(any(grepl(paste0(removed_area, "-"), correspondance_ptdf$area))){ 
        other_bilateral_area <- strsplit(correspondance_ptdf[grepl(paste0(removed_area, "-"), area), area], "-")[[1]][2]
        external_countries <- external_countries[external_countries != other_bilateral_area]
      }
      
    }
    
    for(external_country in external_countries){
      
      if(paste0(external_country, "-", removed_area) %in% correspondance_ptdf$area){ #If the external area has a bilateral link with the removed area
        
        print(paste0("Adjusting balance for bilateral external area ", external_country))
        
        bilateral_link <- getLinks_adequacy(flows, areas = c(external_country, removed_area), internalOnly = T)
        partial_balance <- extract_balance(countries = external_country, links = bilateral_link, flows = flows)$balance #Balance of the external area with the removed area
        
        if(external_country == "uk00" & removed_area == "fr00"){ #Specific case of UK and FR
          PLAN[, ram := ram - partial_balance * 3/4 * ptdfIFA1]
          PLAN[, ram := ram - partial_balance * 1/4 * ptdfIFA2]
          
        } else {
          ptdf <- correspondance_ptdf[area == paste0(external_country, "-", removed_area), ptdf] #Bilateral PTDF
          PLAN[, ram := ram - partial_balance * get(ptdf)]
          
        }
        
      } else if(external_country %in% correspondance_ptdf$area){ #If the external area has a PTDF
        
        print(paste0("Adjusting balance for non-bilateral external area ", external_country))
        
        partial_balance <- balances_other_countries[country == external_country, balance] #Balance with all the non-removed areas
        ptdf <- correspondance_ptdf[area == external_country, ptdf] #PTDF of the area
        PLAN[, ram := ram - partial_balance * get(ptdf)]
        
      } else {
        warning(paste0("Error in finding external ptdfs for country: ", external_country))
      }
      
      print(PLAN[id %in% c(253, 263), ram])
      
    }
  }
  
  #Removing the areas for which all exchanges have been fixed
  
  countries_linked_to_removed_areas <- sapply(removed_areas, get_areas_linked_to_area, areas_all, areas_fb, flows)
  
  external_countries <- unique(unlist(countries_linked_to_removed_areas))
  for(external_country in external_countries){
    
    #If the area has a bilateral link with a removed area
    if(any(grepl(paste(paste0(external_country, "-", removed_areas), collapse = "|"), correspondance_ptdf$area))){
      
      ptdf_external_country <- correspondance_ptdf[grepl(paste(paste0(external_country, "-", removed_areas), collapse = "|"), area), ptdf]
      PLAN <- PLAN[, !..ptdf_external_country]
      
    } else {
      
      other_countries <- get_areas_linked_to_area(area = external_country, areas_all, areas_fb, flows)
      
      if(all(other_countries %in% removed_areas)){
        ptdf_external_country <- correspondance_ptdf[area == external_country, ptdf]
        PLAN <- PLAN[, !..ptdf_external_country]
      }
    }
  }
  
  return(PLAN)
}

remove_fixed_dimensions <- function(PLAN, areas_to_remove){
  
  PLAN <- copy(PLAN)
  
  correspondance_ptdf <- .get_correspondance_ptdf()
  columns_to_remove <- intersect(correspondance_ptdf[area %in% areas_to_remove, ptdf], colnames(PLAN))
  return(PLAN[, !..columns_to_remove])
  
}

.get_correspondance_ptdf <- function(){
  fread("R/projection/inputs/correspondance_ptdf.csv")
}

