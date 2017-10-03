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

ancho <- 63 * 2
alto <- 53 * 2

m <- matrix(nrow=alto, ncol=ancho, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, ancho, alto), nrow=2)
r[] <- 0

source('createLandscape.R', local=TRUE)
source('initRichness.R', local = TRUE)
source('dist2nf.R', local = TRUE)


shinyServer(
  function(input, output, session){

    den_pp <- reactive({
      den_pp <- switch(input$density_pp,
                       'baja' = 100,
                       'media' = 1250,
                       'alta' = 3000)
      })

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

    tableDisp <- reactive({
      myvals <- c(input$small_bird, input$medium_bird, 100-input$small_bird-input$medium_bird)
      tabladispersantes <- data.frame(Names=c("Aves pequeño tamaño", "Aves mediano tamaño", "Mamíferos"),
                 Values=myvals)
      })

    animales <- reactive({
      myvals <- c(input$small_bird, input$medium_bird, 100-input$small_bird-input$medium_bird)
    })


    output$restable <- renderTable({
      tableDisp()
    })


    rasterRich <- reactive({

      pastUse <- switch(input$pp_pastUse,
                        'Bosque natural' = 'Oak',
                        'Matorral' = 'Shrubland',
                        'Pastizal' = 'Pasture',
                        'Cultivo' = 'Crop')

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

    })

    rasterDisp <- reactive({
      v <- disper(x = rasterIni(),
                  xr = rasterRich(),
                  nf_value = 2,
                  pp_value = 1)
      })


    output$initial_map <- renderPlot({

      colores <- c('lightgoldenrod1', # Crops
                   'green', # Natural forests
                   'white', # Other
                   colour_tree_density()) # Pine plantation

      myKey <- list(text = list(lab = c("crop", "natural forest","other", "pine")),
                    rectangles=list(col = colores),
                    space='bottom', columns=4)

      levelplot(rasterIni(), att='landuse', scales=list(draw=FALSE),
                col.regions = colores, colorkey=FALSE, key = myKey)

    })

    output$richness_map <- renderPlot({

      mapa_riqueza <- rasterRich()

      mapa_riqueza[mapa_riqueza == 0] <- NA

      mytheme <- rasterTheme(region = brewer.pal(6, "YlGn"))

      levelplot(mapa_riqueza,
                par.settings = mytheme, margin = FALSE,
                scales=list(draw=FALSE),
                colorkey = list(space = "bottom"),
                pretty=TRUE)
    })

    output$richness_disper <- renderPlot({
      v <- rasterDisp()

      levelplot(v[['msb']],
                margin=FALSE,  par.settings = RdBuTheme)
      })

    output$richness_disperTime <- renderPlot({
      v <- rasterDisp()

      vectores <- animales()
      # per_sb <- 0.5 # tableDisp()$myvals[1]
      per_sb <- vectores[1]
      # per_mb <- 0.4 # tableDisp()$myvals[2]
      per_mb <- vectores[2]
      # per_ma <- 0.1 # tableDisp()$myvals[3]
      per_ma <- vectores[3]

      # propaguleInputs
      piB <- (3.7)/10
      piM <- (0.2)/10

      vv <- disper_time(msb = v[['msb']],
                        mmb = v[['mmb']],
                        mma = v[['mma']],
                        x = rasterIni(),
                        xr = rasterRich(),
                        pp_value = 1,
                        per_sb = per_sb, per_mb = per_mb, per_ma = per_ma,
                        propaguleInputBird = piB,
                        propaguleInputMammal = piM,
                        time_span = 1)

      levelplot(stack(vv),
                margin=FALSE,  par.settings = RdBuTheme)
    })


  }

)


