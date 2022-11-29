# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

######################################  Plot domain result  ######################################

###################################   load ptdf  ###################################   
#lignes <- c(1:27,29:463)
domain <- getPTDF_solo_min(fichier_csv = paste0(path_resultats, "post-traitement_FB0_", nom_domaine, ".csv"), 
                           rm_NA = TRUE,
                           date = "2001-01-01",
                           period = 1)

###################################   Plot  ###################################   

hubDrop = list(NL = c("BE", "DE", "FR", "AT"))
title <- nom_domaine
#ax_lim <- 4000000
ax_lim <- 20000


# square plot
domain_square <- plotFlowbased(PLAN = domain, country1 = "FR", country2 = "BE", 
                               hours = 1, dates = c("2001-01-01"), 
                               main = title,
                               xlim = c(-ax_lim, ax_lim), ylim = c(-ax_lim, ax_lim),
                               hubDrop = hubDrop)
domain_square

html_name <- paste0(nom_domaine, ".html")
htmlwidgets::saveWidget(domain_square, html_name)


##########################################################################################################
############################################  MACZT plot ##########################################
##########################################################################################################

###################################   load ptdf  ###################################   
domain_maczt <- getPTDF_solo_min(fichier_csv = paste0(path_resultats, "post-traitement_MACZT_", nom_domaine, ".csv"), 
                           rm_NA = TRUE,
                           date = "2001-01-01",
                           period = 1)



######## Real FB domain
domain_maczt <- read.csv2(paste0("D:/Users/user/MyPath/",
                           "17XTSO-CS------W_10XFR-RTE------Q_CWE-FB-Z03A01-206_2020112321-F206-01.csv"), dec=".") %>% 
  rename(ram = Margin,
         ptdfFR = FR.hub,
         ptdfBE = BE.hub,
         ptdfAT = AT.hub,
         ptdfDE = DE.hub,
         ptdfNL = NL.hub) %>% 
  mutate(Date = "2001-01-01",
         Period = 1) %>% 
  filter(Presolved == "True") %>% 
  select("BranchId", "ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR", "ptdfNL", "ram", "Date", "Period") %>% as.data.table()


#domain[ram<0]$ram<-0
###################################   Plot  ###################################   
hubDrop = list(NL = c("BE", "DE", "FR", "AT"))
title <- nom_domaine
ax_lim <- 10000
ax_lim2 <- 25000

# FR - BE
plotFlowbased(PLAN = domain_maczt, country1 = "BE", country2 = "FR", 
                hours = 1, dates = c("2001-01-01"), 
                main = title,
                xlim = c(-ax_lim, ax_lim), ylim = c(-ax_lim, ax_lim),
                hubDrop = hubDrop)

# FR - DE
plotFlowbased(PLAN = domain_maczt, country1 = "FR", country2 = "DE", 
              hours = 1, dates = c("2001-01-01"), 
              main = title,
              xlim = c(-ax_lim, ax_lim), ylim = c(-ax_lim2, ax_lim2),
              hubDrop = hubDrop)

# BE - NL
plotFlowbased(PLAN = domain_maczt, country1 = "BE", country2 = "NL", 
              hours = 1, dates = c("2001-01-01"), 
              main = title,
              xlim = c(-ax_lim, ax_lim), ylim = c(-ax_lim, ax_lim),
              hubDrop = hubDrop)

# DE - AT
plotFlowbased(PLAN = domain_maczt, country1 = "DE", country2 = "AT", 
              hours = 1, dates = c("2001-01-01"), 
              main = title,
              xlim = c(-ax_lim2, ax_lim2), ylim = c(-ax_lim, ax_lim),
              hubDrop = hubDrop)

# DE - BE
plotFlowbased(PLAN = domain_maczt, country1 = "DE", country2 = "BE", 
              hours = 1, dates = c("2001-01-01"), 
              main = title,
              xlim = c(-ax_lim2, ax_lim2), ylim = c(-ax_lim, ax_lim),
              hubDrop = hubDrop)


# DE - NL
plotFlowbased(PLAN = domain_maczt, country1 = "DE", country2 = "NL", 
              hours = 1, dates = c("2001-01-01"), 
              main = title,
              xlim = c(-ax_lim2, ax_lim2), ylim = c(-ax_lim, ax_lim),
              hubDrop = hubDrop)
