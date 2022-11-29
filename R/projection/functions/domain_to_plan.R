# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

domain_to_plan <- function(domain, dir){
  
  domain <- copy(domain)
  domain <- janitor::clean_names(domain)
  
  if(!"v1" %in% colnames(domain)){
    domain[, v1:= seq(from = 0, to = (nrow(domain) - 1))]
  }
  
  domain <- domain[presolved == T]
  
  date_raw <- strsplit(dir, "_")[[1]][2]
  date <- as.Date(date_raw, "%Y%m%d")
  period <- strsplit(dir, "_")[[1]][3]
  period <- as.numeric(substr(period, 1, 2))
  
  cols_to_keep <- c(colnames(domain)[grepl("hub|ahc",colnames(domain))], "margin", "v1")
  domain <- domain[, .SD, .SDcols = cols_to_keep]
  
  colnames(domain) <- str_remove(colnames(domain), "_ahc|_hub")
  colnames(domain) <- paste0("ptdf", toupper(colnames(domain)))
  setnames(domain, c("ptdfMARGIN", "ptdfV1"), c("ram", "id"))
  domain[, ":="(ROW_ID = 1:nrow(domain), Period = period, Date = date)]
  
  return(domain)
  
}
