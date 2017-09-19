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

    # Tree density (numeric)
    den_pp <- reactive({
      den_pp <- switch(input$density_pp,
                       'baja' = 100,
                       'media' = 1250,
                       'alta' = 3000)
    })

    # Colour Tree density
    colour_tree_density <- reactive({
      colour_tree_density <- switch(input$density_pp,
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


    output$medium_bird <- renderUI({
      sliderInput(inputId = "medium_bird",
                  label = "Aves mediano tamaño",
                  min = 0, max = 100 - input$small_bird, value = 0)
      })

    output$restable <- renderTable({
      myvals<- c(input$small_bird, input$medium_bird, 100-input$small_bird-input$medium_bird)
      data.frame(Names=c("Aves pequeño tamaño", "Aves mediano tamaño", "Mamíferos"),
                 Values=myvals)
    })

    output$initial_map <- renderPlot({

      # ## $TODO$$ Como hacer esto?? Es necesario
      # ## dvector <- rasterToPolygons(d, fun=function(x){x>0}, dissolve = TRUE)

      ## set colours
      colores <- c('lightgoldenrod1', # Crops
                   'green', # Natural forests
                   'white', # Other
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
              lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
              upRich = c(0, 13.34, mean(16.11, 19.66), 3)))

      mapa_riqueza <- initRichness(r = rasterIni(),
                                   draster = dist_raster,
                                   r_range = myr_range,
                                   treedensity = den_pp(), # density pine plantations
                                   pastUse = pastUse,
                                   rescale = FALSE)

      ## Legend
      # myKey <- list(space='bottom')
      #plot(mapa_riqueza)

      mapa_riqueza[mapa_riqueza == 0] <- NA

      mytheme <- rasterTheme(region = brewer.pal(6, "YlGn"))

      levelplot(mapa_riqueza,
                par.settings = mytheme, margin = FALSE,
                scales=list(draw=FALSE),
                colorkey = list(space = "bottom"),
                pretty=TRUE)




      # myTheme <- BTCTheme()
      # myTheme$panel.background$col = 'gray'

    })
  }

)


