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
        h4("Pinar de Repoblación"),
        sliderInput(inputId = "size_pp", label = "Tamaño de la repoblación de pinar",
                    min = 200, max = 1500, value = 750),
        selectInput(inputId = "density_pp", label = "Densidad de la plantación",
                    choices = c('baja', 'media', 'alta'),
                    selected = 'media'),
        br(),
        h4("Bosques naturales"),
        sliderInput(inputId = "n_nf",label = "Nº bosques naturales",
                    min = 1, max= 5, value =2),
        sliderInput(inputId = "size_nf", label = "Tamaño",
                    min = 50, max = 500, value = 250),
        br(),
        h4('Usos del pasado'),
        selectInput(inputId = "pp_pastUse", label = "Uso del pasado",
                    choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'),
                    selected = 'Matorral'),
        br(),
        h4('Dispersantes'),
        sliderInput(inputId = "small_bird",label = "Aves pequeño tamaño",
                    min = 0, max = 100, value = 0, step = 1),
        uiOutput("medium_bird"),
        tableOutput("restable")),

      mainPanel(
        tabsetPanel(
          tabPanel("Paisaje Inicial", value = 'panel2',
                   plotOutput(outputId = 'initial_map')),
          tabPanel("Mapa de Riqueza Inicial", value = 'panel3',
                   plotOutput(outputId = 'richness_map')),
          tabPanel("Mapa de Riqueza Final", value = 'panel4',
                   plotOutput(outputId = 'richness_disper')),
          tabPanel("Dispersores", value = 'panel5',
                   plotOutput(outputId = 'richness_disperTime'))
          )))))





