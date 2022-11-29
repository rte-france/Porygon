# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

rm(list = ls())
library(data.table)
library(fbAntares)
library(stringr)
library(lubridate)
library(antaresRead)
source("R/projection/functions/extract_flows.R")
source("R/projection/functions/set_diff_flows.R")
source("R/projection/functions/remove_dimensions.R")
source("R/projection/functions/domain_to_plan.R")


### Projection of the domains ###
FB <- computeFB(
  PTDF = "R/antares_integration/inputs/green/all_domains.csv",
  outputName = "R/antares_integration/outputs/green/FBProjection_200_V2/",
  areaName = "cwe_at",
  dayType = "All",
  hour = "All",
  clusteringDayType = "All",
  clusteringHours = "All",
  reports = T,
  nbFaces = 200,
  nbLines = 1e+06,
  hubDrop = NULL,
  fixFaces = NULL,
  virtualFBarea = T,
  verbose = 2,
  maxiter = 10,
  draw_range = c(-1500, 1500),
  useVertices = F,
  thresholdIndic = 98,
  quad = F
)

### Output analysis ###
fbdomains <- readRDS("R/antares_integration/outputs/green/FBProjection_200_V2/domainesFB.RDS")
for(current_id_day in fbdomains$idDayType){
  modelled_domain <- fbdomains[idDayType == current_id_day, PTDFDetails][[1]]
  fwrite(modelled_domain, paste0("R/antares_integration/outputs/green/FBProjection_200_V2/modelled_domain_id_", current_id_day, ".csv"))
}

