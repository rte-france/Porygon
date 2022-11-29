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
library(tibble)

cnecY <- function(x, data) {
  # Y < a * x + b
  data[["a"]] * x + data[["b"]]
}

orderVertex <- function(edges) {
  
  center = c((max(edges[,1])+min(edges[,1]))/2, (max(edges[,2])+min(edges[,2]))/2)
  
  # reorder edges
  edges %>% 
    as.data.table() %>% 
    mutate_all(as.numeric) %>% 
    mutate(angle = atan2(.[[2]]-center[2], .[[1]]-center[1])) %>% 
    arrange(angle) %>% 
    select(-angle)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Flowbased domain viewer"),

    sidebarLayout(
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Choose PTDF File (CSV format)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),    

        # Input: Select a file ----
        fileInput("file2", "Choose VERTEX File (CSV format)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),    
        
        # Horizontal line ----
        tags$hr(),
        
        numericInput(inputId = "nb_cnec",
                     "Number of CNEC to display on each axe",
                     value = 1),
        sliderInput(inputId = "bilanBE",
                    "BE balance",
                    min = -10000,
                    max = 10000,
                    step = 100,
                    value = 0),
        sliderInput(inputId = "bilanNL",
                    "NL balance",
                    min = -10000,
                    max = 10000,
                    step = 100,
                    value = 0),
        
        # Horizontal line ----
        tags$hr(),
        
        sliderInput(inputId = "alegro",
                    "ALEGrO set-point",
                    min = -1000,
                    max = 1000,
                    step = 50,
                    value = 0)
        
      ),
      
      # Show a plot
      mainPanel(
        #add_busy_spinner(spin = "fading-circle"),  
        plotlyOutput(outputId = "fbPlot")
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  domain_maczt_new = reactive({
    
    req(input$file1)
    
    tryCatch(
      {
        show_modal_spinner(spin = "fading-circle", text = "Enumerating domain vertex...")
        domain_maczt <- read.csv2(input$file1$datapath) 
        message(nrow(domain_maczt), " CNEC read")
        
        domain_maczt = setDT(domain_maczt)[, list(BranchId = paste(id, collapse = '\r\n')), 
                                           by = c("AT.hub", "BE.hub", "DE.hub", "FR.hub", "NL.hub", "Margin", "Presolved")]
        
        message(nrow(domain_maczt), " CNEC left after duplicate removal")
        
        remove_modal_spinner() # remove it when done
        
        domain_maczt
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  domain_vertex = reactive({   
    
    req(input$file2)
    
    tryCatch(
      {
        show_modal_spinner(spin = "fading-circle", text = "Enumerating domain vertex...")
        domain_vertex <- read.csv2(input$file2$datapath) 
        message(nrow(domain_vertex), " vertex read")
        remove_modal_spinner() # remove it when done
        
        domain_vertex
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  
  nb_cnec = reactive({ nb_cnec = pmax(input$nb_cnec-1, 0)})
  
  bilanBE = reactive({ bilanBE = input$bilanBE })
  bilanNL = reactive({ bilanNL = input$bilanNL })
  
  alegro = reactive({ alegro = input$alegro })
  
  output$fbPlot <- renderPlotly({
    
    domain_maczt = domain_maczt_new()
    domain_maczt_presolved = domain_maczt %>% filter(Presolved == 'True')
    
    # Find cut edges
    nodes = data.frame(BranchId = domain_maczt_presolved[["BranchId"]],
                       FR = domain_maczt_presolved[["FR.hub"]] - domain_maczt_presolved[["AT.hub"]],
                       DE = domain_maczt_presolved[["DE.hub"]] - domain_maczt_presolved[["AT.hub"]],
                       f0 = domain_maczt_presolved[["Margin"]] - (domain_maczt_presolved[["BE.hub"]] * bilanBE() + domain_maczt_presolved[["NL.hub"]] * bilanNL() - domain_maczt_presolved[["AT.hub"]] * (bilanBE()+bilanNL()))) 
    
    A = nodes %>% select(DE, FR) %>% as.matrix()
    B = nodes %>% select(f0) %>% as.matrix()
    
    edges = vertexenum::enumerate.vertices(A, B)
    
    if (length(edges) == 0) {
      stop(
        paste('FB Domain in empty')
      )
    }
    
    colnames(edges) = c("DE", "FR")
    edges = orderVertex(edges)
    
    
    # Compute CNEC equations
    data = data.frame(BranchId = domain_maczt[["BranchId"]],
                      f0 = domain_maczt[["BE.hub"]] * bilanBE() + domain_maczt[["NL.hub"]] * bilanNL() - domain_maczt[["AT.hub"]] * (bilanBE()+bilanNL())) %>% 
      mutate(a = -(domain_maczt[["DE.hub"]] - domain_maczt[["AT.hub"]]) / (domain_maczt[["FR.hub"]] - domain_maczt[["AT.hub"]]),
             b = (domain_maczt[["Margin"]] - f0) / (domain_maczt[["FR.hub"]] - domain_maczt[["AT.hub"]])) %>% 
      mutate(a = as.numeric(a), b = as.numeric(b)) %>% 
      mutate(y0 = -b/a)  
    
    # remove CNEC with same ptdf (duplicates)
    #data = distinct(data, a, b, y0, .keep_all = T)
    
    # Find minimal set of CNEC
    selection = cbind(data[rep(seq_len(nrow(data)), each = nrow(edges)),],
                      edges[rep(seq_len(nrow(edges)),nrow(data)),]) %>% 
      mutate(s = DE*a + b - FR) %>% 
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
    
    vertex = domain_vertex()
    vertex_projection = vertex %>% 
      # mutate("DE" = DE - AT, 
      #        "FR" = FR - AT) %>% 
      select("DE", "FR")
    enveloppe = chull(vertex_projection)
    vertex_projection = orderVertex(vertex_projection %>% slice(enveloppe))
    vertex_projection = rbind(vertex_projection, vertex_projection %>% head(1))
    
    # DE/FR plot (with BE as balance)
    max_x = 20000
    gap = 1000
    
    p <- plot_ly() %>% 
      layout(title = "Coupe DE - FR",
             xaxis = list(title = "DE", range = c(min(vertex_projection[["DE"]])-gap, max(vertex_projection[["DE"]])+gap), zerolinewidth=2), 
             yaxis = list(title = "FR", range = c(min(vertex_projection[["FR"]])-gap, max(vertex_projection[["FR"]])+gap), zerolinewidth=2),
             showlegend = FALSE)
    
    # enveloppe
    p <- p %>% 
      add_polygons(x = vertex_projection[["DE"]], y = vertex_projection[["FR"]], line=list(color="rgb(40,40,40)", width="3"), fillcolor="transparent", text="Domain enveloppe", hoverinfo = 'none') 
    
    # cut
    p <- p %>% 
      add_polygons(x = edges[["DE"]], y = edges[["FR"]], line=list(color="transparent"), fillcolor='rgba(26,150,65,0.5)', text="Domain cut") %>% 
      add_trace(p, x = edges[["DE"]], y = edges[["FR"]], type="scatter", text = paste("DE:",round(edges[["DE"]]),"\nFR:",round(edges[["FR"]])), mode = "markers", hoverinfo = 'text', marker=list(color='black', size = 5))
    for (cnec in selection[["BranchId"]]) {
      tmpdata  = selection %>% filter(BranchId == cnec)
      points = c(-max_x,0, tmpdata[["y0"]], edges[["DE"]], max_x)
      p <- add_trace(p, 
                     x=points, 
                     y=cnecY(points,tmpdata), 
                     type="scatter", mode="lines", text=cnec, hoverinfo = 'text')
    }
    
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
