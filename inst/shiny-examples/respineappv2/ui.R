library('shiny')
library('shinythemes')
library('shinycssloaders')
library('shinydashboard')
library('knitr')
library('markdown')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')



# Set heigth plots
h_plots <- 800

header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable=TRUE)

body <- dashboardBody(
  fluidRow(
    column(width = 5,
      fluidRow(
        box(
          sliderInput(inputId = "size_pp", label = "Tamaño de la repoblación de pinar",
                      min = 200, max = 1500, value = 750),
          selectInput(inputId = "density_pp", label = "Densidad de la plantación",
                      choices = c('baja', 'media', 'alta'), selected = 'media'),
          selectInput(inputId = "pp_pastUse", label = "Uso del pasado",
                      choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'), selected = 'Matorral'),
          sliderInput(inputId = "n_nf",label = "Nº bosques naturales", min = 1, max= 5, value =2),
          sliderInput(inputId = "size_nf", label = "Tamaño", min = 50, max = 500, value = 250),
          actionButton("doPaisaje", "Configura Paisaje"),
          actionButton("doRiquezaInit", "Riqueza Inicial")
          ),
        box(h5('Dispersantes'),
          sliderInput(inputId = "sb",label = "Aves pequeño tamaño",
                      min = 0, max = 100, value = 0, step = 1),
          uiOutput("mb"),
          tableOutput("disptable")),
        box(h5('Simulación'),
          sliderInput("timeRange", "Número de años simulación:", min=10, max=50, value=30),
          actionButton("doPropagulo", "Input Propágulos"),
          actionButton("doRiquezaEnd", "Riqueza Final"))
      ),

      fluidRow(
        box(width=4, "Riqueza Inicial Repoblaciones"),
        box(width=4, "Riqueza Bosques Naturales"),
        box(width=4, "Riqueza Final Repoblaciones"),
        infoBoxOutput("rich_ppInitBox"),
        infoBoxOutput("rich_nfBox"),
        infoBoxOutput("rich_ppEndBox")
        )
      ),
    column(width = 7,
           box(width = NULL,
                 uiOutput('plotMaps'))
           )
    )
  )

dashboardPage(header, sidebar, body, skin = 'green')
