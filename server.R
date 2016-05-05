
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# load some libraries:
library(shiny)
library(MASS)
library(proxy)
library(plyr)
library(reshape2)
library(memoise)
library(googleVis)
library(htmlwidgets)
library(networkD3)
library(GeLaToLab)
library(MultiViz)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))



shinyServer(function(input, output, session) {
  
  mystate <- prepare.global.state(session)
  
  # Compute distance matrix between topics
  # We wrap this in its own reactive function so that it isn't recomputed if say the value of lambda changes
  computeDist <- reactive({
    #d <- proxy::dist(t(phi), method = distance(measure = input$itm.distance))
#     d <- proxy::dist(t(mystate$kernels))
    # Multidimensional scaling to project the distance matrix onto two dimensions for the vis:
    # Maybe we should explore including options for different scaling algorithms???
    switch(input$cls.scaling,
           PCA = fit <- stats::cmdscale(mystate$kernels, k = 2),
           kruskal = fit <- MASS::isoMDS(d, k = 2)$points,
           sammon = fit <- MASS::sammon(d, k = 2)$points,
           spectral = fit <- GeLaToLab::fit.spectral.embedding(mystate$kernels)
    )
    x <- fit[, 1]
    y <- fit[, 2]
    # collect the (x, y) locations of the topics and their overall proportions in a data.frame:
#     mds.df <- data.frame(topics=1:mystate$K, x=fit[, 1], y=fit[, 2])
#     list(mds.df = mds.df, x = x, y = y)
    list(x = x, y = y)
  })
  
  
clusters <- reactive({
  kmeans(mystate$kernels, input$cls.kmeans)
})

output$kernelplot <- renderPlot({
  compute_state()
  
  plot(computeDist(),
       col = clusters()$cluster,
       pch = 20, cex = 3)
  })

output$cfgforce <- renderForceNetwork({
  compute_state()
  g <- graph.adjacency(mystate$cfg, mode = "directed", weighted = T, diag = F)
#   wc <- cluster_walktrap(g)
#   members <- membership(wc)
  members <- clusters()$cluster
  d3graph <- igraph_to_networkD3(g, group = members)
  
  forceNetwork(Links = d3graph$links, Nodes = d3graph$nodes, 
               Source = 'source', Target = 'target', 
               NodeID = 'name', Group = 'group', zoom=T,width = 700, height= 500, legend=T)

  
})

compute_state <- function(msg = 'Combining Views'){
  if (is.null(mystate$kernels) | isTRUE(mystate$dirty)) {
    withProgress(message = msg,
                 detail = 'This may take a while...', value = 0, {     
                       combine.views(mystate)               
      })

  }
}

observe({
  if (input$comb.views ==0)
    return()
  mystate$dirty <- TRUE
  
#   mystate$
  
  combine.views(mystate, 
                views = c(input$includeMDG, input$includeSrcCorpus, input$includeEvol),
                choice = input$slc.multi.type)
  
})

})
