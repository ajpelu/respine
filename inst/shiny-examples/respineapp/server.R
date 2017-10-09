library('shiny')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')



### -------------------------------
# Load functions
source('createLandscape.R', local=TRUE)
source('initRichness.R', local = TRUE)
source('dist2nf.R', local = TRUE)
source('disper.R', local=TRUE)
source('disper_time.R', local=TRUE)


### -------------------------------
# Initial configuration

## Create empty landscape
set.seed(123)
ancho <- 53 * 2
alto <- 63 * 2

m <- matrix(nrow=alto, ncol=ancho, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, ancho, alto), nrow=2)
r[] <- 0

## Some parameters
line_pol <- 2 ### Line width polygon
pp_value <- 1 ### Value for Pine plantation


### -------------------------------
# SERVER
shinyServer(
  function(input, output, session){

    valores <- reactiveValues(
      doPlot = 0,
      doRiqueza = 0,
      doTime = 1)

    observeEvent(input$doPaisaje, {
      # 0 will be coerced to FALSE
      # 1+ will be coerced to TRUE
      valores$doPlot <- input$doPaisaje
      })



    ### --------------------------
    den_pp <- reactive({
      den_pp <- switch(input$density_pp, 'baja' = 100, 'media' = 1250, 'alta' = 3000)
      })

    colour_tree_density <- reactive({
      colour_tree_density <- switch(input$density_pp,
                                    'baja' = '#a1d99b', 'media' = '#238b45','alta' = '#00441b')
      })

    landscapeInit <- reactive({
      d <- createLandscape(r, size_pp = input$size_pp, size_nf = input$size_nf, n_nf = input$n_nf)
      })

    output$medium_bird <- renderUI({
      sliderInput(inputId = "medium_bird",
                  label = "Aves mediano tamaño",
                  min = 0, max = 100 - input$small_bird, value = 0)
      })

    tableDisp <- reactive({
      myvals <- c(input$small_bird, input$medium_bird, 100-input$small_bird-input$medium_bird)
      tabladispersantes <- data.frame(Dispersor=c("Small birds", "Meidum Birds", "Mammals"),
                 Porcentaje=myvals)
      })

    animales <- reactive({
      myvals <- c(input$small_bird, input$medium_bird, 100-input$small_bird-input$medium_bird)
    })

    output$restable <- renderTable({
      tableDisp()
    })

    rasterRich <- reactive({
      pastUse <- switch(input$pp_pastUse, 'Bosque natural' = 'Oak', 'Matorral' = 'Shrubland',
                        'Pastizal' = 'Pasture','Cultivo' = 'Crop')
      dist_raster <- dist2nf(landscapeInit(), nf_value = 2)

      myr_range <- as.data.frame( cbind(value = c(0,1,2,3),
              lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
              upRich = c(0, 13.34, mean(16.11, 19.66), 2)))

      mapa_riqueza <- initRichness(r = landscapeInit(), draster = dist_raster,
                                   r_range = myr_range, treedensity = den_pp(),
                                   pastUse = pastUse, rescale = FALSE)
    })

    rasterDisp <- reactive({
      v <- disper(x = landscapeInit(), xr = rasterRich(), nf_value = 2, pp_value = pp_value)
      })


    propagule <- reactive({
      v <- rasterDisp()
      perdisp <- animales()

      x = landscapeInit()
      xr = rasterRich()

      msb = v[['msb']]
      mmb = v[['mmb']]
      mma = v[['mma']]

      per_sb = perdisp[1]
      per_mb = perdisp[2]
      per_ma = perdisp[3]

      propaguleInputBird = (3.7)/10
      propaguleInputMammal = (0.2)/10

      # Get richnes of pine plantations
      rich_pp <- calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
      names(rich_pp) <- 'rich_pp'

      # Compute propagule input by cell
      seed_input <- ((msb * per_sb) + (mmb * per_mb)) * propaguleInputBird  + (mma * per_ma) * propaguleInputMammal
      names(seed_input) <- 'seed_input'

      propagule_stack <- stack(rich_pp, seed_input)
    })




    output$initial_map <- renderPlot({

      if (valores$doPlot == FALSE) return()

      isolate({
        colores <- c('lightgoldenrod1', # Crops
                     'green', # Natural forests
                     'white', # Other
                     colour_tree_density()) # Pine plantation
        myKey <- list(text = list(lab = c("Cultivos", "Bosques Naturales","Matorrales", "Pinares")),
                      rectangles=list(col = colores), space='bottom', columns=4)


        limite <- rasterToPolygons(landscapeInit(), fun=function(x){x==1}, dissolve = TRUE)

        levelplot(landscapeInit(), att='landuse', scales=list(draw=FALSE),
                  col.regions = colores, colorkey=FALSE, key = myKey) +
          spplot(limite, fill = "transparent", col = "black",
                 xlim = c(extent(landscapeInit())@xmin, extent(landscapeInit())@xmax),
                 ylim = c(extent(landscapeInit())@ymin, extent(landscapeInit())@ymax),
                 colorkey = FALSE, lwd=line_pol)

        })
      })

    output$richness_map <- renderPlot({
      if (valores$doPlot == FALSE) return()
      isolate({
      mapa_riqueza <- rasterRich()
      mapa_riqueza[mapa_riqueza == 0] <- NA
      mytheme <- rasterTheme(region = brewer.pal(9, "YlGn"))
      limite <- rasterToPolygons(landscapeInit(), fun=function(x){x==1}, dissolve = TRUE)
      levelplot(mapa_riqueza,
                par.settings = mytheme, margin = FALSE,
                scales=list(draw=FALSE),
                colorkey = list(space = "bottom"),
                pretty=TRUE) +
        spplot(limite, fill = "transparent", col = "black",
               xlim = c(extent(landscapeInit())@xmin, extent(landscapeInit())@xmax),
               ylim = c(extent(landscapeInit())@ymin, extent(landscapeInit())@ymax),
               colorkey = FALSE, lwd = line_pol)
      })
    })

    output$richness_disper <- renderPlot({

      v <- rasterDisp()
      levelplot(v[['msb']],
                margin=FALSE,  par.settings = RdBuTheme)
      })


    output$richness_disperTime <- renderPlot({

      i <- 1

      propagulo_time <- propagule()[['rich_pp']] + (propagule()[['seed_input']])*i

      rich_time <- calc(stack(landscapeInit(), rasterRich(), propagulo_time),
                        fun = function(x) ifelse(
                          x[1] == pp_value, x[1]*x[3], x[2]))

      names(rich_time) <- paste0('rich_y',i)
      rich_time[rich_time == 0] <- NA

      limite <- rasterToPolygons(landscapeInit(), fun=function(x){x==1}, dissolve = TRUE)
      mytheme <- rasterTheme(region = brewer.pal(9, "YlGn"))

      levelplot(stack(rich_time),
                par.settings = mytheme, margin = FALSE,
                scales=list(draw=FALSE),
                colorkey = list(space = "bottom"),
                pretty=TRUE) +
        spplot(limite, fill = "transparent", col = "black",
               xlim = c(extent(landscapeInit())@xmin,
                        extent(landscapeInit())@xmax),
               ylim = c(extent(landscapeInit())@ymin,
                        extent(landscapeInit())@ymax),
               colorkey = FALSE, lwd=line_pol)
    })


  }

)


