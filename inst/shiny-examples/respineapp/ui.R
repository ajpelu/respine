library('shiny')
library('knitr')
library('markdown')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')



ancho <- 63 * 2
alto <- 53 * 2
m <- matrix(nrow=alto, ncol=ancho, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, ancho, alto), nrow=2)
r[] <- 0


# m <- matrix(nrow=75, ncol=75, byrow = TRUE)
# r <- raster(m)
# extent(r) <- matrix(c(0, 0, 50, 50), nrow=2)
# # Set default value
# r[] <- 0

# Total nCells
size_landscape <- ncell(r)

# 1 # PINE PLANTATION (pp)

# 1.1 Size
# size_pp_max <- ceiling(size_landscape*0.70)
# size_pp_min <- ceiling(size_landscape*0.015)


navbarPage("Respine App", id = "navbar",
  tabPanel(title = "Configurar Paisaje Inicial", value = 'panel1',
    # Sidebar
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
                    plotOutput(outputId = 'richness_map'))
                    )))))


