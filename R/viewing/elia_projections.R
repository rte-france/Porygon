# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

library(dplyr)
library(xlsx)
library(plotly)

gap = 1000

rte_vertex_file = "PLAN_diff_elia_vertices.csv"
rte_vertex_file = "PLAN_diff_CBONLY_ahc_vertices.csv"

# 2058
elia_domain = "5-winter & workingday-peak-9D_slice"
rte_domain = "2058_20010327_1600"

# 282
elia_domain = "5-winter & workingday-peak-9D_slice"
rte_domain = "282_20010112_1600"


#8005
elia_domain = "4-winter & workingday-peak-9D_slice"
rte_domain = "8005_20011130_1100"


orderVertex <- function(edges) {
  
  center = c((max(edges[,1])+min(edges[,1]))/2, (max(edges[,2])+min(edges[,2]))/2)
  
  # reorder edges
  edges %>% 
    mutate_all(as.numeric) %>% 
    mutate(angle = atan2(.[[2]]-center[2], .[[1]]-center[1])) %>% 
    arrange(angle) %>% 
    select(-angle)
}

axes = list(c('DE', 'FR'), c('BE','FR'), c('BE','DE'), c('BE','NL'))

domain_rte = read.csv(paste0("D:/Users/user/MyPath/",rte_domain,"/PLAN_diff_elia_vertices.csv")) %>% 
  rename('DE' = 'ptdfDE.ptdfNL', 'FR' = 'ptdfFR.ptdfNL', 'BE' = 'ptdfBE.ptdfNL') %>% 
  mutate('NL' = -(DE+FR+BE)) %>% 
  mutate(FR = FR + ptdfFR.ptdfIFA1 + ptdfFR.ptdfIFA2,
         BE = BE + ptdfBE.ptdfNEMO + ptdfALEGRO,
         DE = DE - ptdfALEGRO,
         NL = NL + ptdfNL.ptdfBRITNED)

plot_list = vector("list", length(axes))
layout_list = vector("list", length(axes))

for (i in 1:length(axes)) {

  a = axes[[i]]
  
  projection = paste(a, collapse="-")
  
  domains_elia = read.xlsx("Path/MyPath/Vertices_SG3_ERAA_ELIA.xlsx", projection)

  data_elia = domains_elia %>% filter(full_identifier == elia_domain) %>% 
    select(-full_identifier)

  vertex_projection1 = orderVertex(data_elia)
  vertex_projection1 = rbind(vertex_projection1, vertex_projection1 %>% head(1))

  data_rte = domain_rte %>% select(a)
  rte_enveloppe = chull(data_rte)

  vertex_projection2 = orderVertex(data_rte %>% slice(rte_enveloppe))
  vertex_projection2 = rbind(vertex_projection2, vertex_projection2 %>% head(1))

  plot_list[[i]] = plot_ly() %>%  
    add_polygons(x = vertex_projection1[[a[[1]]]], y = vertex_projection1[[a[[2]]]], line=list(color="blue", width="3"), fillcolor="transparent", text="Domain enveloppe", hoverinfo = 'none') %>% 
    add_polygons(x = vertex_projection2[[a[[1]]]], y = vertex_projection2[[a[[2]]]], line=list(color="red", width="3"), fillcolor="transparent", text="Domain enveloppe", hoverinfo = 'none') %>% 
    layout(xaxis = list(title = "DE", range = c(min(vertex_projection1[[a[[1]]]], vertex_projection2[[a[[1]]]])-gap, max(vertex_projection1[[a[[1]]]], vertex_projection2[[a[[1]]]])+gap), zerolinewidth=2), 
           yaxis = list(title = "FR", range = c(min(vertex_projection1[[a[[2]]]], vertex_projection2[[a[[2]]]])-gap, max(vertex_projection1[[a[[2]]]], vertex_projection2[[a[[2]]]])+gap), zerolinewidth=2))
  layout_list[[i]] = list(text = projection, x = (1 - (i%%2))/2, y = 1 - (i%/%3)/2,  xanchor = "middle", yanchor = "top", xref= "paper", yref= "paper", showarrow = F, font=list(size=18))
    
}

subplot(plot_list, nrows = 2) %>% layout(annotations = layout_list, showlegend = FALSE)
