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
source('initRichness.R', local = TRUE)
source('dist2nf.R', local = TRUE)


shinyServer(
  function(input, output){
    # --> Run once each time user visits the app

    # Tree density
    den_pp <- reactive({
      den_pp <- input$density_pp # density pine plantations
      })

    # Colour Tree density
    colour_tree_density <- reactive({
      colour_tree_density <- switch(den_pp(),
                                    'baja' = '#a1d99b',
                                    'media' = '#238b45',
                                    'alta' = '#00441b')
      })

    rasterIni <- reactive({
      d <- createLandscape(r,
                           size_pp = input$size_pp,
                           size_nf = input$size_nf,
                           n_nf = input$n_nf)
      })


    output$initial_map <- renderPlot({

      # ## $TODO$$ Como hacer esto?? Es necesario
      # ## dvector <- rasterToPolygons(d, fun=function(x){x>0}, dissolve = TRUE)

      ## set colours
      colores <- c('lightgoldenrod1', # Crops
                   'green', # Natural forests
                   'gray99', # Other
                   colour_tree_density()) # Pine plantation

      ## Legend
      myKey <- list(text = list(lab = c("crop", "natural forest","other", "pine")),
                    rectangles=list(col = colores),
                    space='bottom', columns=4)

      ## plot
      levelplot(rasterIni(), att='landuse', scales=list(draw=FALSE),
                col.regions = colores, colorkey=FALSE, key = myKey)

    })

    output$richness_map <- renderPlot({

      pastUse <- switch(input$pp_pastUse,
                        'Bosque natural' = 'Oak',
                        'Matorral' = 'Shrubland',
                        'Pastizal' = 'Pasture',
                        'Cultivo' = 'Crop')

      # Compute distance raster
      dist_raster <- dist2nf(rasterIni(), nf_value = 2)

      myr_range <- as.data.frame(
        cbind(value = c(0,1,2,3),
              lowRich = c(0, 12.82, mean(13.72, 15.62), 5),
              upRich = c(0, 13.34, mean(16.11, 19.66), 7)))

      mapa_riqueza <- initRichness(r = rasterIni(),
                                   draster = dist_raster,
                                   r_range = myr_range,
                                   treedensity = den_pp(), # density pine plantations
                                   pastUse = pastUse,
                                   rescale = TRUE)
    })
  }

)


