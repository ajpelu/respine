library('shiny')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')


m <- matrix(nrow=75, ncol=75, byrow = TRUE)
r <- raster(m)
extent(r) <- matrix(c(0, 0, 50, 50), nrow=2)
# Set default value
r[] <- 0

# Total nCells
size_landscape <- ncell(r)

# 1 # PINE PLANTATION (pp)

# 1.1 Size
# size_pp_max <- ceiling(size_landscape*0.70)
# size_pp_min <- ceiling(size_landscape*0.015)


fluidPage(

  # Application title
  titlePanel('Naturalización de Pinares'),

  # hr(), # break horizontal line

  sidebarLayout(
    sidebarPanel(
      h4('Repoblación'),
        sliderInput(inputId = "size_pp",
                    label = "Tamaño de la repoblación de pinar",
                    min = 300, max = 6000, value = 4500),
        selectInput(inputId = "density_pp",
                    label = "Densidad de la plantación",
                    choices = c('baja', 'media', 'alta'),
                    selected = 'media'),
      br(),
      h4('Bosques naturales'),
        sliderInput(inputId = "n_nf",
                    label = "Nº bosques naturales",
                    min = 1, max= 5, value =2),
        sliderInput(inputId = "size_nf",
                    label = "Tamaño",
                    min = 10, max = 500, value = 350),
      br(),
      h4('Usos del pasado'),
        selectInput(inputId = "pp_pastUse",
                    label = "Uso del pasado",
                    choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'),
                    selected = 'Matorral')
      ),

    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa Inicial", plotOutput(outputId = 'initial_map'))
                  )
              )
    )
)

#   # Plot Output
#   plotOutput(outputId = 'initial_map'),
#
#   hr(),
#
#   fluidRow(
#     column(3,
#            h4('Repoblación'),
#            sliderInput(inputId = "size_pp",
#                        label = "Tamaño de la repoblación de pinar",
#                        min = 300, max = 6000, value = 4500),
#
#            selectInput(inputId = "density_pp",
#                        label = "Densidad de la plantación",
#                        choices = c('baja', 'media', 'alta'),
#                        selected = 'media')
#     ),
#     column(4,
#            h4('Bosques naturales'),
#            sliderInput(inputId = "n_nf",
#                        label = "Nº bosques naturales",
#                        min = 1, max= 5, value =2),
#
#            sliderInput(inputId = "size_nf",
#                        label = "Tamaño",
#                        min = 10, max = 500, value = 350)
#
#     ),
#     column(5,
#            h4('Usos del pasado'),
#            selectInput(inputId = "pp_pastUse",
#                        label = "Uso del pasado",
#                        choices = c('Bosque natural', 'Matorral', 'Pastizal', 'Cultivo'),
#                        selected = 'Matorral')
#            )
#   )
# )
