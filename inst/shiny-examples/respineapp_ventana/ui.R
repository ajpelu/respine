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

# Set heigth plots
h_plots <- 1000

body <- dashboardBody(
  fluidRow(
    column(width = 4,
      fluidRow(
        box(
          h4("Pinar de Repoblación"),
          sliderInput(inputId = "size_pp", label = "Tamaño de la repoblación de pinar",
                      min = 200, max = 1500, value = 750),
          selectInput(inputId = "density_pp", label = "Densidad de la plantación",
                      choices = c('baja', 'media', 'alta'), selected = 'media'),
          selectInput(inputId = "pp_pastUse", label = "Uso del pasado",
                      choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'), selected = 'Matorral'),
          br(),
          h5("Bosques naturales"),
          sliderInput(inputId = "n_nf",label = "Nº bosques naturales", min = 1, max= 5, value =2),
          sliderInput(inputId = "size_nf", label = "Tamaño", min = 50, max = 500, value = 250),
          actionButton("doPaisaje", "Configura Paisaje"),
          actionButton("doRiquezaInit", "Riqueza Inicial")
          ),
        box(h4('Dispersantes'),
          sliderInput(inputId = "sb",label = "Aves pequeño tamaño",
                      min = 0, max = 100, value = 0, step = 1),
          uiOutput("mb"),
          tableOutput("disptable"),
          sliderInput("timeRange", "Número de años simulación:", min=10, max=50, value=30),
          actionButton("doPropagulo", "Input Propágulos"),
          actionButton("doRiquezaEnd", "Riqueza Final")
        )
      ),

      fluidRow(
        infoBoxOutput("rich_ppInitBox"),
        infoBoxOutput("rich_nfBox"),
        infoBoxOutput("rich_ppEndBox")

        #valueBox(paste(), "Riqueza Inicial Repoblación", icon = icon('tree-conifer', lib='glyphicon'), color = 'green'),
        #valueBox(10, "Riqueza Inicial Bosques Naturales", icon = icon('tree-deciduous', lib='glyphicon'), color = 'yellow'),
        #valueBox(10, "Riqueza Final Repoblación", icon = icon('tree-conifer', lib='glyphicon'), color = 'olive')
        )
      ),
    column(width = 8,
           box(width = NULL,
                 uiOutput('plotMaps')
               )

          # wellPanel(uiOutput('plotMaps', height = h_plots))

      # tabBox(width = NULL, side = "right",
      #        tabPanel("Test Mapas", value = 'panel6',
      #                 uiOutput('plotMaps')),
      #
      #        tabPanel("Paisaje Inicial", value = 'panel2',
      #                    withSpinner(
      #                      plotOutput(outputId = 'initial_map', height = h_plots),
      #                      type=5, size=.8)),
      #           tabPanel("Mapa de Riqueza Inicial", value = 'panel3',
      #                    withSpinner(
      #                      plotOutput(outputId = 'richness_map', height = h_plots),
      #                      type=5, size = .8),
      #                    br(),br(),
      #                    h4('Riqueza Inicial de Especies'),
      #                    tableOutput("rich_table_init")),
      #           tabPanel("Input Propágulos", value = 'panel4',
      #                    withSpinner(
      #                      plotOutput(outputId = 'richness_disper', height = h_plots),
      #                      type=5, size = .8)),
      #           tabPanel("Mapa de Riqueza Tiempo", value = 'panel5',
      #                    withSpinner(
      #                      plotOutput(outputId = 'richness_disperTime', height = h_plots),
      #                      type=5, size = .8),
      #                    br(),br(),
      #                    h4('Riqueza Final de Especies'),
      #                    tableOutput("rich_table_end")))
      )
  ))







dashboardPage(header, sidebar, body, skin = 'green')


