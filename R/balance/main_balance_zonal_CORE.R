# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

##################################### Balance calculation for min RAM #############################################

library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### selection date : ERAA22
timeids <- list(list(33, 7182), list(33, 7503), list(33, 3957), list(7, 6169))

eod_input_path <- "data/ERAA22/"
eod_data_raw <- rbind(fread(paste0(eod_input_path, 'ERAA21_EVA_green_y7.csv.gz')),
                  fread(paste0(eod_input_path, 'ERAA21_EVA_green_y29.csv.gz')),
                  fread(paste0(eod_input_path, 'ERAA21_EVA_green_y33.csv.gz')))
eod_data <- eod_data_raw %>% 
  select('Time', 'Version', ends_with("FLOW LIN.")) %>%
  group_by(Version) %>% 
  mutate('timeId' = row_number(Time)) %>% 
  pivot_longer(-c(timeId, Version, Time), names_to= "link", values_to="FLOW LIN.") %>% 
  filter(!str_detect(link, "^[X|0|1|2|3|4|5|6]")) %>%
  mutate(link_from = substr(link, 1, regexpr(" - ", link, fixed = TRUE) - 1),
         link_to = substr(link, regexpr(" - ", link, fixed = TRUE) + 3, regexpr("_FLOW", link, fixed = TRUE) - 1))
  #mutate(link = substr(link,1,11))

versions = unique(data$Version)
 
# ############### 
# Balance per country
# Calculation by adding/removing the flows of all links to ext, except with virtual areas hvdc, psp, p2g, nsd
# ###############

countries= c('al00', 'at00', 'ba00', 'be00', 'bg00', 'bg00e', 'bg00i', 'ch00', 'ch00e', 'ch00i', 'cy00', 'cz00', 'cz00e', 'cz00i', 'de00', 'dke1', 'dkkf', 'dkw1', 'ee00', 'es00', 
             'fi00', 'fr00', 'fr15', 'gr00', 'gr03', 'hr00', 'hu00', 'ie00', 'is00', 'itca', 'itcn', 'itcs', 'itn1', 'its1', 'itsa', 'itsi', 
             'lt00', 'lv00', 'me00', 'mk00', 'mt00', 'nl00', 'nom1', 'non1', 'nos0', 'pl00', 'ple0', 'pli0',
             'pt00', 'ro00', 'rs00', 'rs00e', 'rs00i', 'se01', 'se02', 'se03', 'se04', 'si00', 'sk00', 'sk00e', 'sk00i', 'tn00', 'tr00', 'ua01', 'uk00', 'uk00e', 'uk00i', 'ukni',
             'zz_flowbased' #, 'lub1', 'luf1', 'lug1', 'luv1' 
             )

balance_data <- data.table()

message("Total balance")
for (v in versions) {
  message('Version:',v)
  data = eod_data %>% filter(Version == v)
  for (country in countries) {
    links_vers_country <- data %>% filter(link_from %in% countries & link_to == country)
    links_vers_country <- unique(links_vers_country$link)
    
    links_de_country <- data %>% filter(link_to %in% countries & link_from == country)
    links_de_country <- unique(links_de_country$link)
    
    ## Balance calculation
    
    ## xx_country : negative balance
    balance_vers_country <- data[data$link %in% links_vers_country,]
    balance_vers_country <- balance_vers_country %>% group_by(timeId) %>% summarise(BALANCE = (-sum(`FLOW LIN.`)))
    ## country_xx : positive balance
    balance_de_country <- data[data$link %in% links_de_country,]
    balance_de_country <- balance_de_country %>% group_by(timeId) %>% summarise(BALANCE = sum(`FLOW LIN.`))
    
    ## Summing the two balances if they exist
    if (length(links_vers_country) !=0 & length(links_de_country) !=0){
      balance_country <- copy(balance_vers_country)
      balance_country$BALANCE <- balance_country$BALANCE + balance_de_country$BALANCE
    } else if (length(links_vers_country) != 0) {
      balance_country <- copy(balance_vers_country)
    } else {
      balance_country <- copy(balance_de_country)
    }
    
    balance_country <- as.data.table(balance_country)
    balance_country[, area := rep(country)]
    balance_country[, Version := rep(v)]
    
    setcolorder(balance_country, c("area", "Version", "timeId", "BALANCE"))
    
    balance_data <- rbind(balance_data, balance_country)
  }  
}

rm(balance_country) ; rm(balance_de_country); rm(balance_vers_country)


# ############### 
# Balance FB area
# ###############

area_fb <- c('at00', 'be00', 'cz00', 'cz00e', 'cz00i', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ple0', 'pli0', 'ro00', 'si00', 'sk00', 'sk00e', 'sk00i', 'zz_flowbased')

balance_fb <- data.table()

message("CORE balance")
for (v in versions) {
  message('Version:',v)
  data = eod_data %>% filter(Version == v)

  #country_fb <- "be"
  for (country_fb in area_fb) {
    links_vers_country_fb <- data %>% filter(link_from %in% area_fb & link_to == country_fb)
    links_vers_country_fb <- unique(links_vers_country_fb$link)
    
    links_de_country_fb <- data %>% filter(link_to %in% area_fb & link_from == country_fb)
    links_de_country_fb <- unique(links_de_country_fb$link)
    
    ## xx_country : negative balance
    balance_vers_country_fb <- data[data$link %in% links_vers_country_fb,]
    balance_vers_country_fb <- balance_vers_country_fb %>% group_by(timeId) %>% summarise(Balance_fb = (-sum(`FLOW LIN.`)))
    ## country_xx : positive balance
    balance_de_country_fb <- data[data$link %in% links_de_country_fb,]
    balance_de_country_fb <- balance_de_country_fb %>% group_by(timeId) %>% summarise(Balance_fb = sum(`FLOW LIN.`))
    
    ## Summing the two balances if they exist
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
    balance_country_fb[, Version := rep(v)]
    
    setcolorder(balance_country_fb, c("area", "Version", "timeId", "Balance_fb"))
    
    balance_fb <- rbind(balance_fb, balance_country_fb)
  }
}
rm(balance_country_fb) ; rm(balance_de_country_fb); rm(balance_vers_country_fb)
## balance_fb = balance within FB area


### Merge of both balances, areas outside FB put to zero
balance_tot <- merge(balance_data, balance_fb, by = c("area", "Version","timeId"), all.x = TRUE)
balance_tot[!(area %in% area_fb)]$Balance_fb <- 0


# ############### 
# Balance AHC area
# ###############

area_ahc <- c('ba00', 'bg00', 'bg00e', 'bg00i', 'ch00', 'ch00e', 'ch00i', 'dke1', 'dkw1', 'es00', 'itn1', 'nos0', 'uk00', 'uk00e', 'uk00i', 'ua01', 'lt00', 'rs00', 'rs00e', 'rs00i', 'se04')

balance_ahc <- data.table()

message("AHC balance")
for (v in versions) {
  message('Version:',v)
  data = eod_data %>% filter(Version == v)
  for (country_fb in area_fb) {
    links_vers_country_fb <- data %>% filter(link_from %in% area_ahc & link_to == country_fb)
    links_vers_country_fb <- unique(links_vers_country_fb$link)
    
    links_de_country_fb <- data %>% filter(link_to %in% area_ahc & link_from == country_fb)
    links_de_country_fb <- unique(links_de_country_fb$link)
    
    ## xx_country : negative balance
    balance_vers_country_fb <- data[data$link %in% links_vers_country_fb,]
    balance_vers_country_fb <- balance_vers_country_fb %>% group_by(timeId) %>% summarise(Balance_ahc = -sum(`FLOW LIN.`))
    ## country_xx : positive balance
    balance_de_country_fb <- data[data$link %in% links_de_country_fb,]
    balance_de_country_fb <- balance_de_country_fb %>% group_by(timeId) %>% summarise(Balance_ahc = sum(`FLOW LIN.`))
    
    ## Summing the two balances if they exist
    if(length(links_vers_country_fb) != 0 & length(links_de_country_fb) != 0) {
      balance_country_ahc <- copy(balance_vers_country_fb)
      balance_country_ahc$Balance_ahc <- balance_country_ahc$Balance_ahc + balance_de_country_fb$Balance_ahc
    } else if (length(links_vers_country_fb) != 0) {
      balance_country_ahc <- copy(balance_vers_country_fb)
    } else {
      balance_country_ahc <- copy(balance_de_country_fb)
    }
    
    balance_country_ahc <- as.data.table(balance_country_ahc)
    balance_country_ahc[, area := rep(country_fb)]
    balance_country_ahc[, Version := rep(v)]
    
    setcolorder(balance_country_ahc, c("area", "Version", "timeId", "Balance_ahc"))
    
    balance_ahc <- rbind(balance_ahc, balance_country_ahc)
    
  }
  rm(balance_country_ahc) ; rm(balance_de_country_fb); rm(balance_vers_country_fb)
  
  for (country_ahc in area_ahc) {
    links_de_country_fb <- data %>% filter(link_from %in% area_fb & link_to == country_ahc)
    links_de_country_fb <- unique(links_de_country_fb$link)
    
    links_vers_country_fb <- data %>% filter(link_to %in% area_fb & link_from == country_ahc)
    links_vers_country_fb <- unique(links_vers_country_fb$link)
    
    ## xx_country : negative balance
    balance_vers_country_fb <- data[data$link %in% links_vers_country_fb,]
    balance_vers_country_fb <- balance_vers_country_fb %>% group_by(timeId) %>% summarise(Balance_ahc = sum(`FLOW LIN.`))
    ## country_xx : positive balance
    balance_de_country_fb <- data[data$link %in% links_de_country_fb,]
    balance_de_country_fb <- balance_de_country_fb %>% group_by(timeId) %>% summarise(Balance_ahc = -sum(`FLOW LIN.`))
    
    ## Summing the two balances if they exist
    if(length(links_vers_country_fb) != 0 & length(links_de_country_fb) != 0) {
      balance_country_ahc <- copy(balance_vers_country_fb)
      balance_country_ahc$Balance_ahc <- balance_country_ahc$Balance_ahc + balance_de_country_fb$Balance_ahc
    } else if (length(links_vers_country_fb) != 0) {
      balance_country_ahc <- copy(balance_vers_country_fb)
    } else {
      balance_country_ahc <- copy(balance_de_country_fb)
    }
    
    balance_country_ahc <- as.data.table(balance_country_ahc)
    balance_country_ahc[, area := rep(country_ahc)]
    balance_country_ahc[, Version := rep(v)]
    
    setcolorder(balance_country_ahc, c("area", "Version", "timeId", "Balance_ahc"))
    
    balance_ahc <- rbind(balance_ahc, balance_country_ahc)
  }
}
rm(balance_country_ahc) ; rm(balance_de_country_fb); rm(balance_vers_country_fb)

### Merge of both balances, areas outside FB put to zero
balance_tot <- merge(balance_tot, balance_ahc, by = c("area", "Version", "timeId"), all.x = TRUE)
balance_tot[is.na(balance_tot$Balance_ahc)]$Balance_ahc <- 0


# Correction for DEKF
#balance_tot[area=="de00"]$BALANCE = balance_tot[area=="de00"]$BALANCE - balance_tot[area=="dekf"]$Balance_ahc
#balance_tot[area=="de00"]$Balance_ahc = balance_tot[area=="de00"]$Balance_ahc - balance_tot[area=="dekf"]$Balance_ahc
#balance_tot[area=="dekf",] <- NA

#balance_tot[,"mcYear"] = mcYear


#### filter data
for (timeid in timeids) {
  balance_core <- balance_tot[Version == timeid[[1]] & timeId==timeid[[2]],]
  balance_core <- balance_core[, c("area", "Version", "timeId", "BALANCE", "Balance_fb", "Balance_ahc")]
  
  #### csv writing
  standard <- fread("R/balance/balance_standard_CORE.csv")
  
  standard$timeId <- rep(balance_core$timeId[1])
  standard$mcYear <- rep(balance_core$Version[1])
  #standard$time <- rep (paste0(balance_core$time[1]))
  
  for (i in 1: nrow(balance_core)){
    ar <- balance_core[i,]$area
    standard[area==ar,]$BALANCE <- balance_core[i,]$BALANCE
    standard[area==ar,]$Balance_fb <- balance_core[i,]$Balance_fb
    standard[area==ar,]$Balance_ahc <- balance_core[i,]$Balance_ahc
  }
  
  standard
  write.csv2(standard, paste0("balance_CORE_timeId_", timeid[[2]], ".csv"), row.names = FALSE)
}
####################################################################################################################
# OTHER EXPORTS
###############

balance_tot %>%
  #mutate('Date' = format(time, "%Y%m%d"), 'Hour' = hour(time)) %>%
  select(area, timeId, Balance_fb) %>%
  filter(area %in% c('at00', 'be00', 'cz00', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ro00', 'si00', 'sk00')) %>%
  pivot_wider(names_from = area, values_from = Balance_fb) %>%
  cbind(data[data$link == "be00...de00", "FLOW LIN."]) %>%
  rename('AT' = 'at00', 'BE' = 'be00', 'CZ' = 'cz00', 'DE' = 'de00', 'FR' = 'fr00', 'HR' = 'hr00',
         'HU' = 'hu00', 'NL' = 'nl00', 'PL' = 'pl00', 'RO' = 'ro00', 'SI' = 'si00', 'SK' = 'sk00',
         'ALEGRO' = 'FLOW LIN.') %>%
  write.csv2(paste0("net_positions_shc_maf20_y", mcYear,".csv"), row.names = FALSE)

balance_tot %>%
  #mutate('Date' = format(time, "%Y%m%d"), 'Hour' = hour(time)) %>%
  select(area, timeId, BALANCE) %>%
  filter(area %in% c('at00', 'be00', 'cz00', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ro00', 'si00', 'sk00')) %>%
  pivot_wider(names_from = area, values_from = BALANCE) %>%
  cbind(setNames(data[data$link == "be00...de00", "FLOW LIN."], "ALEGRO")) %>%
  rename('AT' = 'at00', 'BE' = 'be00', 'CZ' = 'cz00', 'DE' = 'de00', 'FR' = 'fr00', 'HR' = 'hr00',
         'HU' = 'hu00', 'NL' = 'nl00', 'PL' = 'pl00', 'RO' = 'ro00', 'SI' = 'si00', 'SK' = 'sk00') %>%
  merge(balance_tot %>%
          select(area, timeId, Balance_ahc) %>%
          filter(area %in% c('ba00', 'bg00', 'ch00', 'dkw1', 'es00', 'itn1', 'rs00')) %>% 
          pivot_wider(names_from = area, values_from = Balance_ahc) %>% 
          cbind(setNames(data[data$link == "be00...uk00", "FLOW LIN."]*-1, "NEMO"),
                setNames(data[data$link == "fr00...uk00", "FLOW LIN."]*-1, "IFA"),
                setNames(data[data$link == "nl00...uk00", "FLOW LIN."]*-1, "BRITNED"),
                setNames(data[data$link == "de00...uk00", "FLOW LIN."]*-1, "UK_DE"),
                setNames(data[data$link == "de00...se04", "FLOW LIN."]*-1, "SE_DE"),
                setNames(data[data$link == "pl00...se04", "FLOW LIN."]*-1, "SE_PL"),
                setNames(data[data$link == "de00...dke1", "FLOW LIN."]*-1, "DKE_DE"),
                setNames(data[data$link == "dkw1...nl00", "FLOW LIN."],    "DKW_NL"),
                setNames(data[data$link == "nl00...nos0", "FLOW LIN."]*-1, "NO_NL"),
                setNames(data[data$link == "de00...nos0", "FLOW LIN."]*-1, "NO_DE"),
                setNames(data[data$link == "ro00...ua01", "FLOW LIN."]*-1, "UA_RO"),
                setNames(data[data$link == "sk00...ua01", "FLOW LIN."]*-1, "UA_SK"),
                setNames(data[data$link == "lt00...pl00", "FLOW LIN."],    "LT_PL"),
                setNames(data[data$link == "hu00...ua01", "FLOW LIN."]*-1, "UA_HU")) %>% 
          rename('BA' = 'ba00', 'BG' = 'bg00', 'CH' = 'ch00', 'DK' = 'dkw1', 'ES' = 'es00', 'IT' = 'itn1', 'RS' = 'rs00')) %>% 
  write.csv2(paste0("net_positions_ahc_maf20_y", mcYear, ".csv"), row.names = FALSE)


balance_tot %>%
  #mutate('Date' = format(time, "%Y%m%d"), 'Hour' = hour(time)) %>%
  select(area, timeId, Balance_ahc) %>%
  filter(area %in% c('at00', 'be00', 'cz00', 'de00', 'fr00', 'hr00', 'hu00', 'nl00', 'pl00', 'ro00', 'si00', 'sk00')) %>%
  pivot_wider(names_from = area, values_from = Balance_ahc) %>%
  rename('AT' = 'at00', 'BE' = 'be00', 'CZ' = 'cz00', 'DE' = 'de00', 'FR' = 'fr00', 'HR' = 'hr00',
         'HU' = 'hu00', 'NL' = 'nl00', 'PL' = 'pl00', 'RO' = 'ro00', 'SI' = 'si00', 'SK' = 'sk00') %>%
  merge(balance_tot %>%
          select(area, timeId, Balance_ahc) %>%
          filter(area %in% c('ba00', 'bg00', 'ch00', 'dkw1', 'es00', 'itn1', 'rs00')) %>%
          pivot_wider(names_from = area, values_from = Balance_ahc) %>%
          cbind(setNames(data[data$link == "be00...uk00", "FLOW LIN."]*-1, "NEMO"),
                setNames(data[data$link == "fr00...uk00", "FLOW LIN."]*-1, "IFA"),
                setNames(data[data$link == "nl00...uk00", "FLOW LIN."]*-1, "BRITNED"),
                setNames(data[data$link == "de00...uk00", "FLOW LIN."]*-1, "UK_DE"),
                setNames(data[data$link == "de00...se04", "FLOW LIN."]*-1, "SE_DE"),
                setNames(data[data$link == "pl00...se04", "FLOW LIN."]*-1, "SE_PL"),
                setNames(data[data$link == "de00...dke1", "FLOW LIN."]*-1, "DKE_DE"),
                setNames(data[data$link == "dkw1...nl00", "FLOW LIN."],    "DKW_NL"),
                setNames(data[data$link == "nl00...nos0", "FLOW LIN."]*-1, "NO_NL"),
                setNames(data[data$link == "de00...nos0", "FLOW LIN."]*-1, "NO_DE"),
                setNames(data[data$link == "ro00...ua01", "FLOW LIN."]*-1, "UA_RO"),
                setNames(data[data$link == "sk00...ua01", "FLOW LIN."]*-1, "UA_SK"),
                setNames(data[data$link == "lt00...pl00", "FLOW LIN."],    "LT_PL"),
                setNames(data[data$link == "hu00...ua01", "FLOW LIN."]*-1, "UA_HU")) %>%
          rename('BA' = 'ba00', 'BG' = 'bg00', 'CH' = 'ch00', 'DK' = 'dkw1', 'ES' = 'es00',
                 'IT' = 'itn1', 'RS' = 'rs00')) %>%
  write.csv2(paste0("net_positions_ahc2_maf20_y", mcYear,".csv"), row.names = FALSE)




