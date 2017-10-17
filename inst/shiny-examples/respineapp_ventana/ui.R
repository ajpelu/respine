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

header <- dashboardHeader(disable = TRUE)
# title = "Respine App CC", titleWidth = 300)

sidebar <- dashboardSidebar(disable=TRUE)
  # width = 300,
  # h5("Pinar de Repoblación"),
  #          h5("Bosques naturales")
  #          )


body <- dashboardBody(
  fluidRow(
    box(width = 2,
      sliderInput(inputId = "size_pp", label = "Tamaño de la repoblación de pinar",
                    min = 200, max = 1500, value = 750),
      selectInput(inputId = "density_pp", label = "Densidad de la plantación",
                  choices = c('baja', 'media', 'alta'), selected = 'media'),
      selectInput(inputId = "pp_pastUse", label = "Uso del pasado",
                  choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'),
                  selected = 'Matorral'),
      sliderInput(inputId = "n_nf",label = "Nº bosques naturales",
                  min = 1, max= 5, value =2),
      sliderInput(inputId = "size_nf", label = "Tamaño",
                  min = 50, max = 500, value = 250),
      actionButton("doPaisaje", "Configura Paisaje")
      ),
    box(width = 2,
        br(),br(),
        h5('Dispersantes'),
        sliderInput(inputId = "sb",label = "Aves pequeño tamaño",
                    min = 0, max = 100, value = 0, step = 1),
        uiOutput("mb"),
        tableOutput("disptable"),
        sliderInput("timeRange", "Número de años:", min=10, max=50, value=30),
        actionButton("doRiqueza", "Calcula Riqueza")),
    tabBox(width=8, side = "right",
           tabPanel("Paisaje Inicial", value = 'panel2',
                         withSpinner(
                           plotOutput(outputId = 'initial_map'),
                           type=5, size=.8)),
                tabPanel("Mapa de Riqueza Inicial", value = 'panel3',
                         withSpinner(
                           plotOutput(outputId = 'richness_map'),
                           type=5, size = .8),
                         br(),br(),
                         h4('Riqueza Inicial de Especies'),
                         tableOutput("rich_table_init")),
                tabPanel("Input Propágulos", value = 'panel4',
                         withSpinner(
                           plotOutput(outputId = 'richness_disper'),
                           type=5, size = .8)),
                tabPanel("Mapa de Riqueza Tiempo", value = 'panel5',
                         withSpinner(
                           plotOutput(outputId = 'richness_disperTime'),
                           type=5, size = .8),
                         br(),br(),
                         h4('Riqueza Final de Especies'),
                         tableOutput("rich_table_end"))
              )
              )
    )



dashboardPage(header, sidebar, body, skin = 'green')


