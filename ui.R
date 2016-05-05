
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
# library(networkD3)

prname= "jedit-5.1.0"
cfg_kernel_beta=5
freq_kernel_alpha=0.01

  headerTag <- paste("The selected project is:",  prname, sep= " ")

ui = shinyUI( navbarPage("MultiViz", id="multiviz", header=headerTag,
tabPanel("Preparation",
         # Sidebar with a slider and selection inputs
         sidebarPanel(width = 3,
                      checkboxInput("includeMDG", "Include MDG", value = FALSE),
                      checkboxInput("includeSrcCorpus", "Include Src Corpus", value = FALSE),
                      checkboxInput("includeEvol", "Include Evolutionary", value = FALSE),
                      hr(),
                      selectInput("slc.multi.type", "Type of Clustering", 
                                  choices = c("Addition" = "MKL.Add",
                                              "Kruskal's Non-Metric" = "kruskal",
                                              "Sammon's Non-Linear Mapping" = "sammon")),
                      
                      actionButton("comb.views","Combine Views")
                      
         ),      
         
         mainPanel(
         tabsetPanel(
           tabPanel("Module Dependecy Graph", 
                    inputPanel(
                       numericInput('cfg_kernel_beta', 'Diffusion Kernel Beta',  cfg_kernel_beta, min = 1)
                       )
                    ),
           tabPanel("Source Code Corpus"),
           tabPanel("Evolutionary Information", 
                    inputPanel(
                      numericInput('freq_gaussian_alpha', 'Gaussian Kernel Alpha',  freq_kernel_alpha, min = 1)
                    )
                    )
         )
         
         )
),


tabPanel("Cluster Source Code",
                              inputPanel(
                                selectInput("cls.scaling", "Multidimensional Scaling Method", choices = c("Classical (PCA)" = "PCA",
                                                                                                          "Kruskal's Non-Metric" = "kruskal",
                                                                                                          "Sammon's Non-Linear Mapping" = "sammon",
                                                                                                          "Spectral Embedding" = "spectral")),
                                sliderInput("cls.kmeans", "Number of clusters", min = 1, max = 10, value = 1, width = "125px")
                              ),
                              #the el parameter in the js code selects the outputIds
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Embedding", 
                                           #HTML("<div id=\"clsDat\" class=\"shiny-document-output\"><svg /></div>")  
                                           plotOutput('kernelplot')),
                                  tabPanel("Force Network", forceNetworkOutput("cfgforce"))
                                )
                              )
                                   
                                
                     )
  )
  
)
