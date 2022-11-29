# Copyright (c) 2020-2022, RTE (www.rte-france.com)
# See AUTHORS.txt
# SPDX-License-Identifier: MPL-2.0
# This file is part of the Porygon project.

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(fbClust)
library(shiny)
library(shinybusy)
library(dplyr)
library(plotly)

## load ptdf from csv for 1 domain, adapt format for plot
# fichier_csv = path
# rm_NA = bool, TRUE = remove rows with NA in PTDF
# date, period = carac domain, date in character
getPTDF_solo_min <- function (fichier_csv, rm_NA, date, period, liste_hubs=c("AT","BE","DE","FR","NL")) {
    
    # load csv
    domain_comp <- read.csv2(fichier_csv, dec = ",")
    
    liste_hubs = lapply(liste_hubs, function(x) paste(x,".hub", sep = "")) %>% unlist()
    
    # extraction colonnes + valeur MIN
    #### A MODIFIER ICI POUR CHANGER CRITERE ###
    domain <- domain_comp[, c("BranchId","Margin", unlist(liste_hubs))]
    domain <- domain %>% group_by_at(vars(one_of("BranchId", unlist(liste_hubs)))) %>% summarise(RAM = min(Margin))
    
    liste_ptdf = lapply(liste_hubs, function(x) paste("ptdf", strsplit(x, ".", fixed=T)[[1]][1],sep = "")) %>% unlist()
    
    # change noms
    domain <- as.data.table(domain)
    names(domain) <- c("BranchId", liste_ptdf, "ram") %>% unlist()
    
    
    ######  Check NA
    # noms des BC qui ont au moins une valeur NA
    listeNA <- rbind(domain[(is.na(domain$ptdfAT)),1],
                     domain[(is.na(domain$ptdfBE)),1],
                     domain[(is.na(domain$ptdfDE)),1],
                     domain[(is.na(domain$ptdfFR)),1],
                     domain[(is.na(domain$ptdfNL)),1])
    listeNA <- unique(listeNA)
    paste0(nrow(listeNA), " BC ont des valeurs NA : ", listeNA$BranchId)
    
    # suppression 
    if (rm_NA) {
        domain <- domain[!BranchId %in% listeNA$BranchId,]
    }
    
    #####  Completer format tableau : date et period 
    domain[ , Date := rep(date)]
    domain[ , Period := rep(period)]
    
    return(domain)
} 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Flowbased domain"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose PTDF File (CSV format)",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),        
            
            # Horizontal line ----
            tags$hr(),
            
            numericInput(inputId = "nb_cnec",
                         "Number of CNEC to display on each axis",
                         value = 1),
            sliderInput(inputId = "bilanDE",
                        "DE balance",
                        min = -10000,
                        max = 10000,
                        step = 100,
                        value = -2914),
            sliderInput(inputId = "bilanNL",
                         "NL balance",
                         min = -10000,
                         max = 10000,
                         step = 100,
                         value = 1468),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select a file ----
            numericInput(inputId = "antares_FR",
                         "FR balance in ANTARES",
                         value = 5000),
            numericInput(inputId = "antares_BE",
                         "BE balance in ANTARES",
                         value = -3000),
            
        ),

        # Show a plot
        mainPanel(
           plotlyOutput(outputId = "fbPlot")
        )
    )
)

cnecY <- function(x, data) {
    # Y < a * x + b
    data[["a"]] * x + data[["b"]]
}


# Define server logic
server <- function(input, output) {
    
    domain_maczt_new = reactive({
        
        req(input$file1)
        
        tryCatch(
            {
                domain_maczt <- getPTDF_solo_min(fichier_csv = input$file1$datapath, 
                                                 rm_NA = TRUE,
                                                 date = "2001-01-01",
                                                 period = 1) 
                message(nrow(domain_maczt), " CNEC read")
                
                domain_maczt = setDT(domain_maczt)[, list(BranchId = paste(BranchId, collapse = '\r\n')), 
                                                   by = c("ptdfAT", "ptdfBE", "ptdfDE", "ptdfFR", "ptdfNL", "ram", "Date", "Period")]
                
                message(nrow(domain_maczt), " CNEC left after duplicate removal")
                
                show_modal_spinner(spin = "fading-circle", text = "Enumerating domain vertex...")
                hubDrop = list(AT = c("BE", "DE", "FR", "NL"))
                
                PLAN <- setDiffNotWantedPtdf(PLAN = domain_maczt, hubDrop = hubDrop)
                start_time = Sys.time()
                vertex = getVertices(PLAN) %>% select(-c(Date, Period))
                end_time = Sys.time()
                message(nrow(vertex), " Vertex found (", round(difftime(end_time, start_time, units = "secs"), 0), " sec)")
                
                message("Presolving CNECs...")
                
                domain_maczt = cbind(domain_maczt[rep(seq_len(nrow(domain_maczt)), each = nrow(vertex)),],
                                     vertex[rep(seq_len(nrow(vertex)),nrow(domain_maczt)),]) %>% 
                    mutate(s = BE*ptdfBE + DE*ptdfDE + FR*ptdfFR + NL*ptdfNL - (BE+DE+FR+NL)*ptdfAT - ram) %>% 
                    mutate(Presolved = ifelse(s < 1 & s > -1, T, F)) %>% 
                    select(-c("BE", "DE", "FR", "NL", "s")) %>% 
                    distinct()
                
                message("Done (", nrow(domain_maczt %>% filter(Presolved == T))," CNECs presolved)")
                remove_modal_spinner() # remove it when done
                
                domain_maczt
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    })
    
    nb_cnec = reactive({ nb_cnec = pmax(input$nb_cnec-1, 0)})
    
    bilanDE = reactive({ bilanDE = input$bilanDE })
    bilanNL = reactive({ bilanNL = input$bilanNL })
    
    antaresFR = reactive({ antaresFR = as.numeric(input$antares_FR) })
    antaresBE = reactive({ antaresBE = as.numeric(input$antares_BE) })
    
    output$fbPlot <- renderPlotly({
        
        domain_maczt = domain_maczt_new()
        domain_maczt_presolved = domain_maczt %>% filter(Presolved == TRUE)
        
        # Find cut edges
        nodes = data.frame(BranchId = domain_maczt_presolved[["BranchId"]],
                           FR = domain_maczt_presolved[["ptdfFR"]] - domain_maczt_presolved[["ptdfAT"]],
                           BE = domain_maczt_presolved[["ptdfBE"]] - domain_maczt_presolved[["ptdfAT"]],
                           f0 = domain_maczt_presolved[["ram"]] - (domain_maczt_presolved[["ptdfDE"]] * bilanDE() + domain_maczt_presolved[["ptdfNL"]] * bilanNL() - domain_maczt_presolved[["ptdfAT"]] * (bilanDE()+bilanNL()))) 
        
        A = nodes %>% select(BE, FR) %>% as.matrix()
        B = nodes %>% select(f0) %>% as.matrix()
        
        edges = vertexenum::enumerate.vertices(A, B)
        
        if (length(edges) == 0) {
            stop(
                paste('FB Domain in empty')
            )
        }
        
        center = c((max(edges[,1])+min(edges[,1]))/2, (max(edges[,2])+min(edges[,2]))/2)
        
        # reorder edges
        edges = edges %>% 
            as.data.table() %>% 
            mutate_all(as.numeric) %>% 
            mutate(angle = atan2(.[[2]]-center[2], .[[1]]-center[1])) %>% 
            arrange(angle) %>% 
            select(-angle)
        
        colnames(edges) = c("BE", "FR")
        
        
        # Compute CNEC equations
        data = data.frame(BranchId = domain_maczt[["BranchId"]],
                          f0 = domain_maczt[["ptdfDE"]] * bilanDE() + domain_maczt[["ptdfNL"]] * bilanNL() - domain_maczt[["ptdfAT"]] * (bilanDE()+bilanNL())) %>% 
            mutate(a = -(domain_maczt[["ptdfBE"]] - domain_maczt[["ptdfAT"]]) / (domain_maczt[["ptdfFR"]] - domain_maczt[["ptdfAT"]]),
                   b = (domain_maczt[["ram"]] - f0) / (domain_maczt[["ptdfFR"]] - domain_maczt[["ptdfAT"]])) %>% 
            mutate(a = as.numeric(a), b = as.numeric(b)) %>% 
            mutate(y0 = -b/a)  
        
        # remove CNEC with same ptdf (duplicates)
        #data = distinct(data, a, b, y0, .keep_all = T)
        
        # Find minimal set of CNEC
        selection = cbind(data[rep(seq_len(nrow(data)), each = nrow(edges)),],
                          edges[rep(seq_len(nrow(edges)),nrow(data)),]) %>% 
            mutate(s = BE*a + b - FR) %>% 
            filter(s < 0.5 & s > -0.5) %>% 
            select(BranchId, f0, a, b, y0) %>% 
            distinct()
        
        
        # Add additional CNECs
        if (nb_cnec() > 0) {
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a > 0 & b > 0) %>% slice_min(n = nb_cnec(), order_by=b))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a > 0 & b > 0) %>% slice_max(n = nb_cnec(), order_by=y0))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a > 0 & b < 0) %>% slice_max(n = nb_cnec(), order_by=b))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a > 0 & b < 0) %>% slice_min(n = nb_cnec(), order_by=y0))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a < 0 & b > 0) %>% slice_min(n = nb_cnec(), order_by=b))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a < 0 & b > 0) %>% slice_min(n = nb_cnec(), order_by=y0))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a < 0 & b < 0) %>% slice_max(n = nb_cnec(), order_by=b))
            selection = rbind(selection, data %>% setdiff(selection) %>% filter(a < 0 & b < 0) %>% slice_max(n = nb_cnec(), order_by=y0)) %>% 
                distinct()
        }
        
        selection = setDT(selection)[, list(BranchId = paste(BranchId, collapse = '\r\n')), by = c('a', 'b', 'y0')]
        
        ech0 = antaresFR()+antaresBE() 
        
        max_x = 20000
        
        echange_be_fr = selection %>% 
            mutate(x := (ech0 - `b`)/(`a`+1)) %>% 
            mutate(y := -`x` + ech0) %>% 
            select(x, y)
        
        echange_be_fr = rbind(echange_be_fr,
                              data.table(x = c(-max_x, max_x), y = c(max_x + ech0, -max_x + ech0)))
        
        
        # BE/FR plot (with DE as balance)
        p <- plot_ly() %>% 
            layout(title = "Coupe BE - FR",
                   xaxis = list(title = "BE", range = c(min(c(antaresBE(), min(edges[["BE"]])))-1500, max(c(antaresBE(), max(edges[["BE"]])))+1500), zerolinewidth=2), 
                   yaxis = list(title = "FR", range = c(min(c(antaresFR(), min(edges[["FR"]])))-1500, max(c(antaresFR(), max(edges[["FR"]])))+1500), zerolinewidth=2),
                   showlegend = FALSE)
        p <- p %>% 
            add_polygons(x = edges[["BE"]], y = edges[["FR"]], line=list(color="transparent"), fillcolor='rgba(26,150,65,0.5)', text="Domain FB") %>% 
            add_trace(p, x = edges[["BE"]], y = edges[["FR"]], type="scatter", text = paste("BE:",round(edges[["BE"]]),"\nFR:",round(edges[["FR"]])), mode = "markers", hoverinfo = 'text', marker=list(color='black', size = 5))
        for (cnec in selection[["BranchId"]]) {
            tmpdata  = selection %>% filter(BranchId == cnec)
            points = c(-max_x,0, tmpdata[["y0"]], edges[["BE"]], max_x)
            p <- add_trace(p, 
                           x = points, 
                           y = cnecY(points,tmpdata),
                           type="scatter", mode="lines", text=cnec, hoverinfo = 'text')
        }
        
        p <- p %>% 
            add_trace(p, x = antaresBE(), y = antaresFR(), type="scatter", text = paste("BE:",antaresBE(),"\nFR:",antaresFR()), mode = "markers", hoverinfo = 'text', marker=list(color='red', size = 15)) %>% 
            add_trace(p, x = echange_be_fr[["x"]], y = echange_be_fr[["y"]], 
                      type="scatter", mode="lines", line=list(color = 'rgb(150, 150, 150)', dash = 'dash'),
                      text = paste("BE:",round(echange_be_fr[["x"]]),"\nFR:",round(echange_be_fr[["y"]])), hoverinfo = 'text')
        
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
