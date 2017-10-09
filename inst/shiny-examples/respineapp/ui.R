library('shiny')
library('shinythemes')
library('shinydashboard')
library('knitr')
library('markdown')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')


navbarPage("Respine App", id = "navbar", theme = shinytheme("simplex"),
  tabPanel(title='', value = 'panel1',
    sidebarLayout(
      sidebarPanel(
        h5("Pinar de Repoblación"),
        sliderInput(inputId = "size_pp", label = "Tamaño de la repoblación de pinar",
                    min = 200, max = 1500, value = 750),
        selectInput(inputId = "density_pp", label = "Densidad de la plantación",
                    choices = c('baja', 'media', 'alta'),
                    selected = 'media'),
        selectInput(inputId = "pp_pastUse", label = "Uso del pasado",
                    choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'),
                    selected = 'Matorral'),
        h5("Bosques naturales"),
        sliderInput(inputId = "n_nf",label = "Nº bosques naturales",
                    min = 1, max= 5, value =2),
        sliderInput(inputId = "size_nf", label = "Tamaño",
                    min = 50, max = 500, value = 250),
        actionButton("doPaisaje", "Configura Paisaje"),
        br(),br(),
        h5('Dispersantes'),
        sliderInput(inputId = "small_bird",label = "Aves pequeño tamaño",
                    min = 0, max = 100, value = 0, step = 1),
        uiOutput("medium_bird"),
        tableOutput("restable"),
        sliderInput("timeRange", "Número de años:", min=10, max=50, value=30),
        actionButton("doRiqueza", "Calcula Riqueza")
        ),

      mainPanel(
        tabsetPanel(
          tabPanel("Paisaje Inicial", value = 'panel2',
                   plotOutput(outputId = 'initial_map')),
          tabPanel("Mapa de Riqueza Inicial", value = 'panel3',
                   plotOutput(outputId = 'richness_map')),
          tabPanel("Input Propágulos", value = 'panel4',
                   plotOutput(outputId = 'richness_disper')),
          tabPanel("Mapa de Riqueza Tiempo", value = 'panel5',
                   plotOutput(outputId = 'richness_disperTime'))
          )))))





