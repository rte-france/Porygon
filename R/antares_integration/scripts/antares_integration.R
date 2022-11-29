# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())

library(antaresRead)
library(antaresEditObject)
library(data.table)
library(stringr)

ERAA_study_path <- "D:/Users/user/MyPath"

weights <- fread("R/antares_integration/outputs/green/FBProjection_200_V2/weight.txt")
second_members <- fread("R/antares_integration/outputs/green/FBProjection_200_V2/second_member.txt")
ts <- readRDS("R/antares_integration/outputs/ts.rds")

### Creation of new zones and the associated links ###
setSimulationPath(ERAA_study_path, simulation = "input")

links_weights <- fread("R/antares_integration/inputs/correspondance_links_weights.csv")

all_areas <- getAreas()
countries_with_ie <- unlist(sapply(all_areas, function(X){
  if(any(grepl(paste0(X, "i"), all_areas))){
    X
  }
}))
if("ple0" %in% all_areas){
  countries_with_ie <- c(countries_with_ie, "pl00")
}

# Adding Alegro areas if non-existing
if(!"alegro1" %in% all_areas){
  createArea("alegro1")
  createArea("alegro2")
  createArea("alegro3")
  createLink(from = "alegro1", to = "alegro2", propertiesLink = propertiesLinkOptions(hurdles_cost = F, transmission_capacities = "infinite"))
  createLink(from = "alegro1", to = "alegro3", propertiesLink = propertiesLinkOptions(hurdles_cost = F, transmission_capacities = "infinite"))
  createLink(from = "alegro2", to = "alegro3", propertiesLink = propertiesLinkOptions(hurdles_cost = F, transmission_capacities = "infinite"))
}

# If connstraints with Alegro exist, they are moved to the Alegro1-Alegro2 link
all_bc <- readBindingConstraints()
bede_bc_condition <- sapply(all_bc, function(X){
  if("be00%de00" %in% names(X$coefs)){T} else {F}
})
bede_bc <- all_bc[bede_bc_condition]

for(i in 1:length(bede_bc)){
  name <- names(bede_bc)[i]
  bc <- bede_bc[[i]]
  coefs <- bc$coefs
  names_coef <- names(coefs)
  names_coef[names_coef == "be00%de00"] <- "alegro1%alegro2"
  names(coefs) <- names_coef
  editBindingConstraint(name = name, coefficients = coefs)
}

# Manual removal of be-de coefficients
bc_ini <- readIniFile(file.path(ERAA_study_path, "input/bindingconstraints/bindingconstraints.ini"))
bc_ini_new <- lapply(bc_ini, function(X){
  if("be00%de00" %in% names(X)){
    res <- copy(X)
    res$"be00%de00" <- NULL
    res
  } else {
    X
  }
})
writeIni(bc_ini_new, file.path(ERAA_study_path, "input/bindingconstraints/bindingconstraints.ini"), overwrite = T)

# Adding The ZZ_flowbased area if non-existing
if(!"zz_flowbased" %in% all_areas){
  countries_linked_to_fb <- links_weights[grepl("zz_flowbased", link), link]
  countries_linked_to_fb <- sapply(countries_linked_to_fb, str_remove, " - zz_flowbased")
  countries_linked_to_fb <- c(countries_linked_to_fb, "nl00")
  
  # Special case of areas with an import/export virtual area
  countries_linked_to_fb <- unlist(sapply(countries_linked_to_fb, function(X){
    if(X %in% countries_with_ie){
      if(X == "pl00"){
        getAreas(select = c("ple0", "pli0"))
      } else {
        getAreas(select = c(paste0(X, "i"), paste0(X, "e")))
      }
    } else {
      X
    }
  }))
  
  createArea("zz_flowbased")
  for(country in countries_linked_to_fb){
    if(grepl("00e|ple0", country)){
      datalink <- matrix(c(rep(99999, 8760), rep(0, 8760*7)), ncol = 8)
      createLink(from = country, to = "zz_flowbased", propertiesLink = propertiesLinkOptions(hurdles_cost = F), dataLink = datalink)
    } else if(grepl("00i|pli0", country)){
      datalink <- matrix(c(rep(0, 8760), rep(99999, 8760), rep(0, 8760*6)), ncol = 8)
      createLink(from = country, to = "zz_flowbased", propertiesLink = propertiesLinkOptions(hurdles_cost = F), dataLink = datalink)
    } else {
      createLink(from = country, to = "zz_flowbased", propertiesLink = propertiesLinkOptions(hurdles_cost = F, transmission_capacities = "infinite"))
    }
  }
  links_between_fb_countries <- getLinks(countries_linked_to_fb, internalOnly = T)
  for(link in links_between_fb_countries){
    from_country <- strsplit(link, " - ")[[1]][1]
    to_country <- strsplit(link, " - ")[[1]][2]
    removeLink(from = from_country, to = to_country)
  }
}

# Modification of constraints related to NL
bc_ini <- readIniFile(file.path(ERAA_study_path, "input/bindingconstraints/bindingconstraints.ini"))
bc_ini_new <- lapply(bc_ini, function(X){
  if("be00%nl00" %in% names(X) | "de00%nl00" %in% names(X)){
    res <- copy(X)
    coef <- sign(res$"be00%nl00")
    res$"be00%nl00" <- NULL
    res$"de00%nl00" <- NULL
    res$"nl00%zz_flowbased" <- -coef
    res
  } else {
    X
  }
})
writeIni(bc_ini_new, file.path(ERAA_study_path, "input/bindingconstraints/bindingconstraints.ini"), overwrite = T)


# Adding The model_description_fb area if non-existing
if(!"model_description_fb" %in% all_areas){
  createArea("model_description_fb")
}

### Creation of virtual clusters ###
# Removing existing clusters associated to model_description_fb
clusters_desc <- readClusterDesc()
if("model_description_fb" %in% clusters_desc$area){
  clusters_to_delete <- clusters_desc[area == "model_description_fb", cluster]
  for(current_cluster in clusters_to_delete){
    removeCluster(area = "model_description_fb", cluster = current_cluster, add_prefix = F)
  }
}

days_to_pick <- dcast(ts, time~tsId)
days_to_pick$time <- NULL
days_to_pick <- as.data.table(days_to_pick[, sapply(.SD, rep, each = 24)])

for(fb_constraint in weights$Name){
  associated_second_members <- second_members[Name == fb_constraint, .(Id_day, vect_b)]
  second_members_ts <- days_to_pick[, sapply(.SD, function(X){
    merge <- merge(data.table(Id_day = unlist(X)), associated_second_members, by = "Id_day", sort = F)
    merge[, vect_b]
  })]
  
  nominal_capacity <- max(second_members_ts)
  
  createCluster(area  = "model_description_fb", cluster_name = fb_constraint, nominalcapacity = nominal_capacity, unitcount = 1L, time_series = second_members_ts, prepro_modulation = matrix(rep(1, 8760 * 4), ncol = 4), add_prefix = F)
 }


### Creation of binding constraints ###

cleanlinks_weights <- copy(links_weights)
cleanlinks_weights <- cleanlinks_weights[, link := sapply(link, function(X){
  sort <- lapply(strsplit(X, " - "), sort)
  sapply(sort, function(Y){paste0(Y[1], "%", Y[2])})
})]
# Checking if links are in the correct direction to stick to the Antares convention
cleanlinks_weights <- cleanlinks_weights[, direct := sapply(weight, function(X){
  clean_link <- cleanlinks_weights[weight == X, link]
  first_country_clean <- strsplit(clean_link, "%")[[1]][1]
  initial_link <- links_weights[weight == X, link]
  first_country_initial <- strsplit(initial_link, " - ")[[1]][1]
  first_country_clean == first_country_initial
})]


for(fb_constraint in weights$Name){
  current_weights <- weights[Name == fb_constraint]
  current_weights <- melt(current_weights, id.vars = "Name", variable.name = "weight")
  weighting_coefficients <- merge(current_weights, cleanlinks_weights, by = "weight")
  weighting_coefficients[direct == F, value := -value]
  
  # Special case of FR-UK
  weighting_coefficient_fr_uk <- 3/4 * weighting_coefficients[weight == "FR-IFA1.ZZ_flowbased", value] + 1/4 * weighting_coefficients[weight == "FR-IFA2.ZZ_flowbased", value]
  weighting_dt_fr_uk <- data.table(weight = "FR-UK.ZZ_flowbased", Name = fb_constraint, value = weighting_coefficient_fr_uk, link = "fr00%uk00", direct = T)
  weighting_coefficients <- rbind(weighting_coefficients, weighting_dt_fr_uk)
  weighting_coefficients <- weighting_coefficients[!weight %in% c("FR-IFA1.ZZ_flowbased", "FR-IFA2.ZZ_flowbased")]
  
  # Special case of areas with import/export virtual areas
  weighting_coefficients_corrected <- rbindlist(lapply(weighting_coefficients$weight, function(X){
    current_value <- weighting_coefficients[weight == X, value]
    current_link <- weighting_coefficients[weight == X, link]
    countries <- strsplit(current_link, "%")[[1]]
    country1 <- countries[1]
    country2 <- countries[2]
    if(country1 %in% countries_with_ie & country2 %in% countries_with_ie){
      data.table(weight = X, Name = fb_constraint, value = current_value, link = c(paste0(country1, "e%", country2, "i"), paste0(country1, "i%", country2, "e")))
    } else if(country1 %in% countries_with_ie){
      data.table(weight = X, Name = fb_constraint, value = current_value, link = c(paste0(country1, "e%", country2), paste0(country1, "i%", country2)))
    } else if(country2 %in% countries_with_ie){
      data.table(weight = X, Name = fb_constraint, value = current_value, link = c(paste0(country1, "%", country2, "e"), paste0(country1, "%", country2, "i")))
    } else{
      data.table(weight = X, Name = fb_constraint, value = current_value, link = current_link)
    }
  }))
  
  #Special case of PL00 and PLE0/PLI0
  all_links <- getLinks()
  weighting_coefficients_corrected[grepl("pl00", link), link := sapply(link, function(X){
    countries <- strsplit(X, "%")[[1]]
    countries <- str_replace(countries, "pl00e", "pl00")
    countries <- str_replace(countries, "pl00i", "pl00")
    link_new <- X
    if(!paste0(countries[1], " - ", countries[2]) %in% all_links){ # If there is a link between PL00 et the other area in the initial study, the PTDF is applied on this link
      link_new <- str_replace(link_new, "pl00e", "ple0")
      link_new <- str_replace(link_new, "pl00i", "pli0")
    } else { # Otherwise, the PTDF is applied on the link between PLE0/PLI0 and the other area
      link_new <- str_replace(link_new, "pl00e", "pl00")
      link_new <- str_replace(link_new, "pl00i", "pl00")
    }
    link_new
  })]
  weighting_coefficients_corrected <- unique(weighting_coefficients_corrected)
  
  weighting_vector <- c(weighting_coefficients_corrected$value, -1)
  names(weighting_vector) <- c(weighting_coefficients_corrected$link, paste0("model_description_fb.", tolower(fb_constraint)))
  
  # Temporarily removing DE-UK
  weighting_vector <- weighting_vector[names(weighting_vector) != "de00%uk00e"]
  weighting_vector <- weighting_vector[names(weighting_vector) != "de00%uk00i"]
  
  createBindingConstraint(name = fb_constraint, values = data.table(less = rep(0, 8760)), timeStep = "hourly", operator = "less", coefficients = weighting_vector, overwrite = T)
}

# Update of the scenario builder

nb_climate_ts <- 35
nb_mc_years <- simOptions()$parameters$general$nbyears

sb <- readIniFile(file.path(ERAA_study_path, "settings/scenariobuilder.dat"))
sb <- sb[["Default Ruleset"]]
thermal_fb_elements <- which(grepl("t,model_description_fb", names(sb)))

if(length(thermal_fb_elements) != 0){
  sb_clean <- sb[-thermal_fb_elements]
} else {
  sb_clean <- sb
}

names_ts <- paste0("t,model_description_fb,", rep(0:(nb_mc_years - 1)))
names_ts_complete <- apply(expand.grid(names_ts, tolower(weights$Name)), 1, paste, collapse=",")

scenario_fb_dt <- data.table(name = names_ts_complete)
scenario_fb_dt[, year := rep(1:35, length.out = length(names_ts_complete))]

vector_sb <- c(sb_clean, scenario_fb_dt$year)
names(vector_sb) <- c(names(sb_clean), scenario_fb_dt$name)
sb_new <- list("Default Ruleset" = vector_sb)
writeIni(sb_new, file.path(ERAA_study_path, "settings/scenariobuilder.dat"), overwrite = T)
