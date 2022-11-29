# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())

library(antaresRead)
library(antaresEditObject)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)


study_path <- "D:/Users/user/MyPath"

setSimulationPath(study_path, simulation = -1)

# all_areas <- getAreas()
# all_links <- getLinks()
# is_fb_area <- sapply(all_areas, function(X){
#   paste0(X, " - zz_flowbased") %in% all_links
# })
# fb_areas <- all_areas[is_fb_area]
# fb_links <- c(getLinks(areas = "zz_flowbased"), "alegro1 - alegro2")
# 
# hourly_output <- readAntares(areas = fb_areas, links = fb_links, mcYears = "all")
# hourly_output_links <- hourly_output$links
# 
# min_max_exchange <- hourly_output_links[, .(min_exchange = min(`FLOW LIN.`), max_exchange = max(`FLOW LIN.`)), by = link]

# Checking that output NPs of Antares are within the FB domains

weights <- fread("R/antares_integration/outputs/FBProjection/weight.txt")
second_members <- fread("R/antares_integration/outputs/FBProjection/second_member.txt")
ts <- readRDS("R/antares_integration/outputs/ts.rds")

original_weights <- fread("R/antares_integration/inputs/all_domains.csv")


links_weights <- fread("R/antares_integration/inputs/correspondance_links_weights.csv")
links_weights <- links_weights[links_weights$weight %in% colnames(weights)]


links_weights[, link_clean := sapply(link, function(X){
  sort <- lapply(strsplit(X, " - "), sort)
  sapply(sort, function(Y){paste0(Y[1], " - ", Y[2])})
})]
links_weights[, reversed_link := (link != link_clean)]

hourly_output_links <- readAntares(links = "all", mcYears = "all")

hourly_output_links[, link_clean := str_replace(link, "00e|00i", "00")]
hourly_output_links[, link_clean := str_replace(link_clean, "ple0|pli0", "pl00")]

hourly_flows <- hourly_output_links[, .(flow = sum(`FLOW LIN.`)), by = list(link_clean, mcYear, timeId, time)]
hourly_flows_merged <- merge(hourly_flows, links_weights[, .(link_clean, weight, reversed_link)], by = "link_clean")
hourly_flows_merged[, flow_for_weight := ifelse(reversed_link, -flow, flow)]
hourly_flows_merged[, day_syn := paste0(mcYear, "_", as.Date(time))]


points_in_domain <- data.table(idDayType = unique(second_members$Id_day))

points_in_domain[, percent_points_in_domain := sapply(idDayType, function(typDay){
  
  corresponding_days <- ts[idDayType == typDay, paste0(tsId, "_", time)]
  corresponding_flows <- hourly_flows_merged[day_syn %in% corresponding_days]
  
  flows_per_weight <- dcast(corresponding_flows, formula = weight~mcYear+timeId, value.var = "flow_for_weight")
  corresponding_weights <- copy(weights)
  if ('DE-UK_DE.ZZ_flowbased' %in% colnames(corresponding_weights)) {
    corresponding_weights$`DE-UK_DE.ZZ_flowbased` <- NULL
  }
  setcolorder(corresponding_weights, c("Name", flows_per_weight$weight))
  
  # Treatment for UK
  flows_per_weight <- flows_per_weight[weight != "FR-IFA2.ZZ_flowbased"]
  if ('FR-IFA1.ZZ_flowbased' %in% colnames(corresponding_weights)) {
    corresponding_weights[, `FR-IFA1.ZZ_flowbased` := 3/4 * `FR-IFA1.ZZ_flowbased` + 1/4 * `FR-IFA2.ZZ_flowbased`]
    corresponding_weights$`FR-IFA2.ZZ_flowbased` <- NULL
  }
  
  corresponding_weights$Name <- NULL
  flows_per_weight$weight <- NULL
  
  res_matrix <- as.matrix(corresponding_weights) %*% as.matrix(flows_per_weight)
  res_matrix <- as.data.table(res_matrix)
  corresponding_ram <- second_members[Id_day == typDay, .(Name, vect_b)] %>% .[order(Name)]
  
  is_in_domain <- res_matrix[, sapply(.SD, function(X){
    diff <- corresponding_ram$vect_b - X
    all(diff >= -5)
  })]
  
  total_nb_points <- length(is_in_domain)
  points_in_domain_percent <- length(is_in_domain[is_in_domain == T]) / total_nb_points * 100
  points_in_domain_percent
  
})]

points_in_domain[, percent_points_in_original_domain := sapply(idDayType, function(typDay){
  
  corresponding_days <- ts[idDayType == typDay, paste0(tsId, "_", time)]
  corresponding_flows <- hourly_flows_merged[day_syn %in% corresponding_days]
  
  flows_per_weight <- dcast(corresponding_flows, formula = weight~mcYear+timeId, value.var = "flow_for_weight")
  corresponding_weights <- original_weights[idDayType == typDay]
  corresponding_ram <- corresponding_weights$ram
  
  cols_to_keep <- colnames(corresponding_weights)[grepl("ptdf", colnames(corresponding_weights))]
  corresponding_weights <- corresponding_weights[, .SD, .SDcols = cols_to_keep]
  
  if ('ptdfDE-UK_DE' %in% colnames(corresponding_weights)) {
    corresponding_weights$`ptdfDE-UK_DE` <- NULL
  }
  colnames(corresponding_weights) <- sapply(colnames(corresponding_weights), function(X){
    X <- str_remove(X, "ptdf")
    X <- paste0(X, ".ZZ_flowbased")
  })
  
  setcolorder(corresponding_weights, flows_per_weight$weight)
  
  # Treatment for UK
  flows_per_weight <- flows_per_weight[weight != "FR-IFA2.ZZ_flowbased"]
  if ('FR-IFA1.ZZ_flowbased' %in% colnames(corresponding_weights)) {
    corresponding_weights[, `FR-IFA1.ZZ_flowbased` := 3/4 * `FR-IFA1.ZZ_flowbased` + 1/4 * `FR-IFA2.ZZ_flowbased`]
    corresponding_weights$`FR-IFA2.ZZ_flowbased` <- NULL
  }
  flows_per_weight$weight <- NULL
  
  res_matrix <- as.matrix(corresponding_weights) %*% as.matrix(flows_per_weight)
  res_matrix <- as.data.table(res_matrix)

  is_in_domain <- res_matrix[, sapply(.SD, function(X){
    diff <- corresponding_ram - X
    all(diff >= -.04*corresponding_ram)
  })]
  
  total_nb_points <- length(is_in_domain)
  points_in_domain_percent <- length(is_in_domain[is_in_domain == T]) / total_nb_points * 100
  points_in_domain_percent
  
})]


# comparison study ntc
ntc_study_path <- "D:/Users/user/MyPath"
setSimulationPath(ntc_study_path, simulation = -1)
ntc_hourly_output_links <- readAntares(links = "all", mcYears = "all")

# compute core balances 
area_fb <- c('at00', 'be00', 'cz00', 'cz00e', 'cz00i', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ple0', 'pli0', 'ro00', 'si00', 'sk00', 'sk00e', 'sk00i')

links_fb = c()
all_links = as.character(unique(ntc_hourly_output_links$link))
for (i in area_fb) {
  links_fb = c(links_fb, all_links[grepl(i, all_links)])
}
links_fb <- unique(links_fb)

balance_fb <- data.table()

for (country_fb in area_fb) {
  links_vers_country_fb <- links_fb[which((substring(links_fb, 1, str_locate(links_fb, "-")-2) %in% area_fb & substring(links_fb, str_locate(links_fb, "-")+2) == country_fb))] %>% na.omit()
  links_de_country_fb <- links_fb[which((substring(links_fb, 1, str_locate(links_fb, "-")-2) == country_fb & substring(links_fb, str_locate(links_fb, "-")+2) %in% area_fb))] %>% na.omit()

  ## xx_country : negative balance
  balance_vers_country_fb <- ntc_hourly_output_links[ntc_hourly_output_links$link %in% links_vers_country_fb,]
  balance_vers_country_fb <- balance_vers_country_fb %>% group_by(timeId) %>% summarise(Balance_fb = (-sum(`FLOW LIN.`)))
  ## country_xx : positive balance
  balance_de_country_fb <- ntc_hourly_output_links[ntc_hourly_output_links$link %in% links_de_country_fb,]
  balance_de_country_fb <- balance_de_country_fb %>% group_by(timeId) %>% summarise(Balance_fb = sum(`FLOW LIN.`))
  
  ## summing both balances
  if(length(links_vers_country_fb)!=0 & length(links_de_country_fb)!=0){
    balance_country_fb <- copy(balance_vers_country_fb)
    balance_country_fb$Balance_fb <- balance_country_fb$Balance_fb + balance_de_country_fb$Balance_fb
  } else if (length(links_vers_country_fb) !=0) {
    balance_country_fb <- copy(balance_vers_country_fb)
  } else {
    balance_country_fb <- copy(balance_de_country_fb)
  }
  
  balance_country_fb <- as.data.table(balance_country_fb)
  balance_country_fb[, area := rep(country_fb)]
  
  setcolorder(balance_country_fb, c("area", "timeId", "Balance_fb"))
  
  balance_fb <- rbind(balance_fb, balance_country_fb)
}


balance_fb_flatten = balance_fb %>% pivot_wider(id_cols=c(timeId, area), names_from = area, values_from = Balance_fb)

balance_fb_flatten[, "sk00"] <- balance_fb_flatten[, "sk00"] + balance_fb_flatten[, "sk00e"]
balance_fb_flatten[, "sk00e"] <- NULL
balance_fb_flatten[, "sk00i"] <- NULL
balance_fb_flatten[, "ple0"] <- NULL
balance_fb_flatten[, "pli0"] <- NULL
balance_fb_flatten[, "cz00e"] <- NULL
balance_fb_flatten[, "cz00i"] <- NULL

# check
#balance_fb_flatten %>% select(-timeId) %>% rowSums() %>% sum()
#fwrite(balance_fb_flatten, "D:/Users/user/MyPath/balance_fb_ntc.csv")

balance_fb_flatten[, "nl00"] <- NULL

balance_fb_flatten = balance_fb_flatten %>% rename_at(vars(-timeId), ~ paste0(toupper(substring(.,1,2)), "-NL.ZZ_flowbased"))

balance_fb_flatten["ALEGRO.ZZ_flowbased"] = ntc_hourly_output_links[link == "be00 - de00", `FLOW LIN.`]
balance_fb_flatten["FR-IT.ZZ_flowbased"] = ntc_hourly_output_links[link == "fr00 - itn1", `FLOW LIN.`]
balance_fb_flatten["AT-IT.ZZ_flowbased"] = ntc_hourly_output_links[link == "at00 - itn1", `FLOW LIN.`]
balance_fb_flatten["SI-IT.ZZ_flowbased"] = -ntc_hourly_output_links[link == "itn1 - si00", `FLOW LIN.`]
balance_fb_flatten["FR-ES.ZZ_flowbased"] = -ntc_hourly_output_links[link == "es00 - fr00", `FLOW LIN.`]
balance_fb_flatten["DE-DK.ZZ_flowbased"] = ntc_hourly_output_links[link == "de00 - dkw1", `FLOW LIN.`]
balance_fb_flatten["FR-CH.ZZ_flowbased"] = -ntc_hourly_output_links[link == "ch00i - fr00", `FLOW LIN.`] - ntc_hourly_output_links[link == "ch00e - fr00", `FLOW LIN.`]
balance_fb_flatten["DE-CH.ZZ_flowbased"] = -ntc_hourly_output_links[link == "ch00i - de00", `FLOW LIN.`] - ntc_hourly_output_links[link == "ch00e - de00", `FLOW LIN.`]
balance_fb_flatten["AT-CH.ZZ_flowbased"] = ntc_hourly_output_links[link == "at00 - ch00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "at00 - ch00e", `FLOW LIN.`]
balance_fb_flatten["HR-RS.ZZ_flowbased"] = ntc_hourly_output_links[link == "hr00 - rs00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "hr00 - rs00e", `FLOW LIN.`]
balance_fb_flatten["HU-RS.ZZ_flowbased"] = ntc_hourly_output_links[link == "hu00 - rs00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "hu00 - rs00e", `FLOW LIN.`]
balance_fb_flatten["RO-RS.ZZ_flowbased"] = ntc_hourly_output_links[link == "ro00 - rs00i", `FLOW LIN.`] +ntc_hourly_output_links[link == "ro00 - rs00e", `FLOW LIN.`]
balance_fb_flatten["RO-BG.ZZ_flowbased"] = -ntc_hourly_output_links[link == "bg00i - ro00", `FLOW LIN.`] - ntc_hourly_output_links[link == "bg00e - ro00", `FLOW LIN.`]
balance_fb_flatten["HR-BA.ZZ_flowbased"] = -ntc_hourly_output_links[link == "ba00 - hr00", `FLOW LIN.`]
balance_fb_flatten["BE-NEMO.ZZ_flowbased"] = ntc_hourly_output_links[link == "be00 - uk00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "be00 - uk00e", `FLOW LIN.`]
balance_fb_flatten["FR-IFA1.ZZ_flowbased"] = ntc_hourly_output_links[link == "fr00 - uk00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "fr00 - uk00e", `FLOW LIN.`]
balance_fb_flatten["NL-BRITNED.ZZ_flowbased"] = ntc_hourly_output_links[link == "nl00 - uk00i", `FLOW LIN.`] + ntc_hourly_output_links[link == "nl00 - uk00e", `FLOW LIN.`]
balance_fb_flatten["PL-SE_PL.ZZ_flowbased"] = ntc_hourly_output_links[link == "pl00 - se04", `FLOW LIN.`]
balance_fb_flatten["DE-DKE_DE.ZZ_flowbased"] = ntc_hourly_output_links[link == "de00 - dke1", `FLOW LIN.`]
balance_fb_flatten["NL-DKW_NL.ZZ_flowbased"] = -ntc_hourly_output_links[link == "dkw1 - nl00", `FLOW LIN.`]
balance_fb_flatten["NL-NO_NL.ZZ_flowbased"] = ntc_hourly_output_links[link == "nl00 - nos0", `FLOW LIN.`]
balance_fb_flatten["DE-SE_DE.ZZ_flowbased"] = ntc_hourly_output_links[link == "de00 - se04", `FLOW LIN.`]
balance_fb_flatten["DE-NO_DE.ZZ_flowbased"] = ntc_hourly_output_links[link == "de00 - nos0", `FLOW LIN.`]
balance_fb_flatten["RO-UA_RO.ZZ_flowbased"] = ntc_hourly_output_links[link == "ro00 - ua01", `FLOW LIN.`]
balance_fb_flatten["SK-UA_SK.ZZ_flowbased"] = ntc_hourly_output_links[link == "sk00i - ua01", `FLOW LIN.`] + ntc_hourly_output_links[link == "sk00e - ua01", `FLOW LIN.`]
balance_fb_flatten["PL-LT_PL.ZZ_flowbased"] = -ntc_hourly_output_links[link == "lt00 - pl00", `FLOW LIN.`]
balance_fb_flatten["HU-UA_HU.ZZ_flowbased"] = ntc_hourly_output_links[link == "hu00 - ua01", `FLOW LIN.`]


balance_fb_flatten = merge(balance_fb_flatten,
                           hourly_flows_merged %>% filter(link_clean == "alegro1 - alegro2" & mcYear == 4) %>% select(c(timeId, day_syn)),
                           by = "timeId")

points_in_domain[, percent_points_in_domain_with_ntc := sapply(idDayType, function(typDay){
  
  corresponding_days <- ts[idDayType == typDay, paste0(tsId, "_", time)]
  corresponding_flows <- balance_fb_flatten %>% filter(day_syn %in% corresponding_days)
  
  corresponding_weights <- original_weights[idDayType == typDay]
  corresponding_ram <- corresponding_weights$ram
  
  cols_to_keep <- colnames(corresponding_weights)[grepl("ptdf", colnames(corresponding_weights))]
  corresponding_weights <- corresponding_weights[, .SD, .SDcols = cols_to_keep]

  if ('ptdfDE-UK_DE' %in% colnames(corresponding_weights)) {
    corresponding_weights$`ptdfDE-UK_DE` <- NULL
  }
  colnames(corresponding_weights) <- sapply(colnames(corresponding_weights), function(X){
    X <- str_remove(X, "ptdf")
    X <- paste0(X, ".ZZ_flowbased")
  })
  
  
  # Treatment for UK
  corresponding_flows["FR-IFA2.ZZ_flowbased"] <- NULL
  if ('FR-IFA1.ZZ_flowbased' %in% colnames(corresponding_weights)) {
    corresponding_weights[, `FR-IFA1.ZZ_flowbased` := 3/4 * `FR-IFA1.ZZ_flowbased` + 1/4 * `FR-IFA2.ZZ_flowbased`]
    corresponding_weights$`FR-IFA2.ZZ_flowbased` <- NULL
  }

  corresponding_flows <- t(corresponding_flows %>% select(-c("timeId", "day_syn")))
  setcolorder(corresponding_weights, rownames(corresponding_flows))
  
  res_matrix <- as.matrix(corresponding_weights) %*% as.matrix(corresponding_flows)
  res_matrix <- as.data.table(res_matrix)
  
  is_in_domain <- res_matrix[, sapply(.SD, function(X){
    diff <- corresponding_ram - X
    all(diff >= -.04*corresponding_ram)
  })]
  
  total_nb_points <- length(is_in_domain)
  points_in_domain_percent <- length(is_in_domain[is_in_domain == T]) / total_nb_points * 100
  points_in_domain_percent
  
})]

fwrite(points_in_domain, "D:/Users/user/MyPath/stats_purple_shc.csv")
