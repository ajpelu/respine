library('shiny')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')


## --> Este codigo solo se ejecuta una vez

# Crear landscape vacío
set.seed(123)
m <- matrix(nrow=100, ncol=200, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
r[] <- 0

source('createLandscape.R', local=TRUE)


shinyServer(
  function(input, output){
    # --> Run once each time user visits the app


    output$mapa <- renderPlot({
      # --> Run each time a user changes a widget that output$map relies on

      # Landscape parameteres
      size_pp <- input$size_pp  # size pine plantations
      den_pp <- input$density_pp # density pine plantations
      size_nf <- input$size_nf # size natural forests
      n_nf <- input$n_nf  # n patchs natural forests

      # Colour Tree density
      colour_tree_density <- switch(input$density_pp,
                                    'baja' = '#a1d99b',
                                    'media' = '#238b45',
                                    'alta' = '#00441b')

      d <- createLandscape(r,
                           size_pp = size_pp,
                           size_nf = size_nf, n_nf = n_nf)

      dvector <- rasterToPolygons(d, fun=function(x){x>0}, dissolve = TRUE)

      #
      ## set colours
      colores <- c('lightgoldenrod1', # Crops
                   'green', # Natural forests
                   'gray99', # Other
                   colour_tree_density) # Pine plantation

      ## Legend
      myKey <- list(text = list(lab = c("crop", "natural forest","other", "pine")),
                    rectangles=list(col = colores),
                    space='bottom', columns=4)


      ## plot
      levelplot(d, att='landuse', scales=list(draw=FALSE),
                col.regions = colores, colorkey=FALSE, key = myKey)



    })
  }

)


