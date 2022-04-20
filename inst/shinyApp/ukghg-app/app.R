## UK-GHG Interface app
## R script to make a shiny app which provides acces to the ukghg package
##
## Peter Levy, CEH Edinburgh
## Bush Estate, Penicuik, EH26 0QB, U.K.
## plevy@ceh.ac.uk
## Tel: 0131 445 8556
## September 2019

library(piggyback)
library(shiny)
library(lubridate)
library(ukghg)
library(ggplot2)
library(bslib)
library(tibble)
library(leaflet)
library(DT)

devtools::source_url("https://github.com/NERC-CEH/UKCEH_shiny_theming/blob/main/theme_elements.R?raw=TRUE")

source('functions.R')

ls_sectors <- 1:12
sectorNames <- c('Total',
                 "Energy production",
                 "Domestic combustion",
                 "Industrial combustion",
                 "Industrial processes",
                 "Offshore",
                 "Solvents",
                 "Road transport",
                 "Other transport",
                 "Waste",
                 "Agriculture",
                 "Natural")

# ls_sectors <- 1:11
# sectorNames <- c("Energy production",
#                  "Domestic combustion",
#                  "Industrial combustion",
#                  "Industrial processes",
#                  "Offshore",
#                  "Solvents",
#                  "Road transport",
#                  "Other transport",
#                  "Waste",
#                  "Agriculture",
#                  "Natural")
# names(ls_sectors) <- sectorNames

# some inital values for selections
df_init <- data.frame(
  startDate = "01/01/2014 00:00",
  endDate   = "01/12/2014 00:00",
  ghgName = "CO2"
)

# Format the dates as POSIXct
df_init$startDate <- as.POSIXct(df_init$startDate, format = "%d/%m/%Y %H:%M", tz = "UTC")
df_init$endDate   <- as.POSIXct(df_init$endDate, format = "%d/%m/%Y %H:%M", tz = "UTC")

# Define UI for the app
ui <- fluidPage(
  tags$style("
        #map_controls {
  /* Appearance */
  background-color: white;
  padding: 10px 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.65;
  zoom: 0.9;
  transition: opacity 0ms 0ms;
}
#map_controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
}
#waiting_text {
text-align: center
}"), # this above css code is from :https://shiny.rstudio.com/gallery/superzip-example.html
        
  theme = UKCEH_theme,
  UKCEH_titlePanel("UK-GHG"),
    tabsetPanel(id = 'TID',
      tabPanel("Run Model",
               sidebarLayout( 
                 sidebarPanel(
                   fluidRow(column(6, h4('Select:')), column(6, actionButton("sendjobbutton", "Run Model", class="btn btn-success btn-lg"), align="right")),
                   fluidRow(
                     column(4, selectInput('select_gas', h5("Gas"), choices = as.list(c("CH4", "CO2", "N2O", "C2H6", "VOC")))),
                     column(4, selectInput('select_proj', h5("Projection"), choices = as.list(c("OSGB", "LonLat")))),
                     column(4, uiOutput('res_selector')),
                     #column(4, selectInput('select_res', h5("Resolution"), choices = list('OSGB (km)' = as.list(c("1", "20", "100")),'LonLat (deg)' = as.list(c("0.1"))), selected = "100" ))
                     ), 
                   fluidRow(
                     column(4, selectInput("select_unitType", label = h5("Units"), choices = as.list(c("mol", "g")))),
                     column(4, selectInput("select_unitSIprefix", label = h5("Unit Prefix"), choices = as.list(c("kilo", "none", "milli", "micro", "nano", "pico")), selected = "micro" )),
                     column(4, selectInput("select_writeNetCDF", label = h5("output NetCDF"), choices = as.list(c(TRUE, FALSE)), selected = FALSE ))# I want this as a button # 
                     ),
                   fluidRow(
                     column(2, h5('Start:')),
                     column(width = 4, dateInput("sdate", value = min(as.Date(df_init$startDate)), min = min(as.Date(df_init$startDate)), max = max(as.Date(df_init$endDate)), label = h5("Date"))),
                     column(width = 3, numericInput("shour", value = 00, label = h5("Hour"), min = 0, max = 23, step = 1)),
                     column(width = 3, numericInput("smin", value = 00, label = h5("Minute"), min = 0, max = 59, step = 1))
                   ),
                   fluidRow(
                     column(2, h5('End:')),
                     column(width = 4, dateInput("edate", value = max(as.Date(df_init$endDate)), min = min(as.Date(df_init$startDate)), max = max(as.Date(df_init$endDate)), label = h5("Date"))),
                     column(width = 3, numericInput("ehour", value = 00, label = h5("Hour"), min = 0, max = 23, step = 1)),
                     column(width = 3, numericInput("emin", value = 00, label = h5("Minute"), min = 0, max = 59, step = 1))
                   ),
                   
                   fluidRow(
                     sliderInput("intslider", label = "Select the number of time steps between the start and end times:", min = 1, max = 366, value = 1, step = 1)
                     ),
                   fluidRow(
                     column(width = 6, h6("Time scales of variation:"))
                       ),
                   fluidRow(
                     column(width = 3, checkboxInput("log_year", "Inter-annual", TRUE)),
                     column(width = 3, checkboxInput("log_yday", "Seasonal", TRUE)),
                     column(width = 3.5, checkboxInput("log_wday", "Day-of-week", FALSE)),
                     column(width = 3, checkboxInput("log_hour", "Diurnal", FALSE)),
                     column(width = 12, checkboxInput("log_includeBio", "Include Natural Biogenic Fluxes", TRUE)), align="center"
                     #column(3, actionButton("sendjobbutton", "Run Model", class="btn btn-outline-success btn-lg"), align="center")
                     )),
                   # fluidRow(
                   #   column(12, actionButton("sendjobbutton", "Run Model", class="btn btn-outline-success btn-lg"), align="center")
                   # )),
                 mainPanel(
                   column(12, h3("Model parameters:"), align="center"),
                   #leafletOutput('map', width = "100%", height = "100%"),  
                   #plotOutput('timePlot'),
                   #verbatimTextOutput('tmp'),
                   dataTableOutput('job_table')
                   )
                 #mainPanel(leafletOutput('map')) # I'll add my leaflet map here
               )),
      tabPanel("View Output", 
               #htmlOutput('waiting_text', class = 'waiting_text'),
               leafletOutput('map', height = '795px'),
               absolutePanel(
                 id = "map_controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, top = 200, left = 100, right = 'auto', bottom = "auto", width = 330, height = "auto",
                 #h5('Select Sector'),
                 selectInput('sel_sector', label = 'Select sector:', choices = sectorNames),
                 #selectInput('sel_sector', label = 'Select sector:', choices = c('Total', sectorNames)),
                 #selectInput('sel_sector', label = 'Select sector:', choices = sectorNames),
                 uiOutput('time_points'),
                 column(12, actionButton("plotmapbutton", "Plot Data", class = "btn btn-success btn-lg"), align = 'center')
                 ),
               #selectInput('sel_sector', label = 'Select sector:', choices = sectorNames)
               #leafletOutput('map', height = '795px'),
      ),
      tabPanel("Help",
               verbatimTextOutput('title_text'),
               #verbatimTextOutput('checks'),
               plotOutput('checks'),
               verbatimTextOutput('title_text2'),
               ), 
      
      tabPanel("Download",
               verbatimTextOutput('raster_info'),
               plotOutput('raster_check'),
               htmlOutput('waiting_text', class = 'waiting_text')),
               #verbatimTextOutput('raster_check')),
      tabPanel("Documentation",
               p("UK-GHG is a spatio-temporal model of greenhouse gas fluxes from the UK."),
               uiOutput("git_link"),
               p("Other documenation available:"),
               a("R package manual",target="_blank",href="ukghg.pdf"),
               p("and slides from a presentation from Sept 2017:"),
               a("Presentation",target="_blank",href="CMO_ukghg.pdf")
               ),
      tabPanel("Theory",
               includeHTML("./www/ukghg_timeDisaggn.html")
               )
      )
  )



# Define server logic required for the app
server <- function(input, output) {
 # Create a data frame with all the information about the job

  output$res_selector <- renderUI(selectInput('select_res', h5("Resolution"), choices = list('OSGB' = c("1 km", "20 km", "100 km"), 'LonLat' = "0.1 deg")[[input$select_proj]], selected = '100 km'))
  
  startDate <- reactive(as.POSIXct(strptime(paste(sprintf("%02d", day(input$sdate)), "/", sprintf("%02d", month(input$sdate)), "/", year(input$sdate), " ", sprintf("%02d", input$shour), ":", sprintf("%02d", input$smin), sep = ""), "%d/%m/%Y %H:%M"), tz = "UTC"))
  endDate   <- reactive(as.POSIXct(strptime(paste(sprintf("%02d", day(input$edate)), "/", sprintf("%02d", month(input$edate)), "/", year(input$edate), " ", sprintf("%02d", input$ehour), ":", sprintf("%02d", input$emin), sep = ""), "%d/%m/%Y %H:%M"), tz = "UTC"))
  
  
  datect <- reactive({
    # create a sequence of timestamps
    seq(startDate(), endDate(), length = input$intslider)
  })
  
  #observeEvent(input$sendjobbutton, {
    output$time_points <- renderUI(selectInput('timp_sector', label = 'Select timepoint:', choices = strftime(datect(), format = "%d-%m-%Y %H:%M")))
  #})
    
  # time_point_i <- observeEvent(input$sendjobbutton, {  
  #   which(input$timp_sector == strftime(datect(), format = "%d-%m-%Y %H:%M"))
  #   })
    
  time_point_i <- reactive({which(input$timp_sector == strftime(datect(), format = "%d-%m-%Y %H:%M"))})
  
  job_df <- reactive({
    # create a job dataframe
    tibble(datech = as.character(datect()),
               ghgName = input$select_gas,
               #ghgName = tolower(input$select_gas),
               #nice_ghgName = input$select_gas,
               proj = input$select_proj,
               res = input$select_res,
               unitType = input$select_unitType,
               unitSIprefix = input$select_unitSIprefix,
               writeNetCDF = input$select_writeNetCDF,
               log_year = input$log_year,
               log_yday = input$log_yday,
               log_wday = input$log_wday,
               log_hour = input$log_hour,
               log_includeBio = input$log_includeBio,
               datect = datect())
  })
  
  output$job_table <- renderDataTable({
    datatable(job_df()[,c('datech', 'ghgName', 'proj', 'res', 'unitType', 'unitSIprefix', 'log_year', 'log_yday', 'log_wday', 'log_hour', 'log_includeBio')], colnames = c('Timestamp', 'GHG', 'Proj', 'Res', 'Unit', 'Unit Prefix', 'Year', 'yDay', 'wDay', 'Hour', 'Bio'), rownames= FALSE, extensions = 'Buttons', options = list(
      #autoWidth = TRUE, 
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection', 
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download')
      )
    ))
  }, serve = F)
  

  myFlux <- eventReactive(input$sendjobbutton, {
    #shinybusy::show_modal_spinner()
    #showModal(modalDialog('Model running'))
    calcFlux(tolower(input$select_gas), job_df()$datect, proj = input$select_proj,
                       res = gsub(' km| deg', '', input$select_res), input$select_unitType, input$select_unitSIprefix,
                       includeBio = input$log_includeBio,
                       timeScales = c(input$log_year, input$log_yday, input$log_wday, input$log_hour),
                       writeNetCDF = input$select_writeNetCDF)
    #removeModal()
    #shinybusy::remove_modal_spinner()
  })
  
  # output$timePlot <- renderPlot({
  #         ylab_text <- paste("Total Net Flux, Tg", input$select_gas, "/y")
  #         plot(job_df()$datect, myFlux()$total, type = "b", xlab = "Date", ylab = ylab_text)
  #       })
  
  #reactive()
  
  target_stack <- reactive({
    if(input$sel_sector == 'Total'){
      myFlux()$s_ghgTotal[[time_point_i()]]
    } else{
      myFlux()$ls_ghgBySectorByTime[[which(input$sel_sector == sectorNames)-1]][[time_point_i()]]
      }
  })
  # 
  # #target_stack_n <- reactive({dim(target_stack())[3]}) # extract the depth of the raster stack - i.e. how many time points? 
  # 
  vals2 <- reactive({values(target_stack())})
  vals <- reactive({values(myFlux()$s_ghgTotal[[1]])})
  output$title_text <- renderPrint(vals())
  #output$title_text <- reactive(paste(input$sel_sector, input$timp_sector, time_point_i(), sep = ' '))
  output$checks <- renderPlot(plot(target_stack()[[time_point_i()]]))
  output$title_text2 <- renderPrint(vals2())
  #output$checks <- renderText(dim(target_stack()[[time_point_i()]]))
  #output$checks <- renderText(names(target_stack()))
  # 
  # #####
  # observeEvent(input$sendjobbutton, {
  #       
  #      output$map <- renderLeaflet({
  #         leaflet() %>%
  #           addTiles() %>%
  #           fitBounds(-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085)
  #       })
  # })
  # 
  # observe({
  #   pal <- colorNumeric(palette = "viridis", domain = rev(vals()), na.color = "transparent", reverse = F)
  #   
  #   leafletProxy("map") %>%
  #     clearShapes() %>%
  #     addRasterImage(target_stack()[[time_point_i()]], opacity = 0.5, colors = pal) %>% 
  #     addLegend_decreasing(pal = pal, values = vals(), decreasing = T, title = toupper(input$select_gas))
  # })
  
  
  #####
  
  # THIS WORKED
  observeEvent(input$sendjobbutton, {
      pal <- colorNumeric(palette = "viridis", domain = rev(vals()), na.color = "transparent", reverse = F)

      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          fitBounds(-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085) %>%
          addRasterImage(myFlux()$s_ghgTotal[[1]], opacity = 0.5, colors = pal) %>%
          addLegend_decreasing(pal = pal, values = vals(), decreasing = T, title = toupper(input$select_gas))
        })
      })

  
  #target_raster <- observeEvent(input$sendjobbutton, myFlux()$ls_ghgBySectorByTime[[ls_sectors[input$sel_sector]]], ignoreInit = T)
  
  #output$raster_check <- renderPlot(plot(target_raster()))

  observeEvent(input$sendjobbutton == 0, {
    output$waiting_text <- renderText('<b>Outputs will appear here after the model has been run.<b>')
    }, once = T)
  
  observeEvent(input$sendjobbutton == 1, {
    output$waiting_text <- NULL
  }, ignoreInit = T)
  
  # observeEvent(input$sendjobbutton, {
  #   target_raster <- myFlux()$ls_ghgBySectorByTime[[ls_sectors[input$sel_sector]]]# will [1] beocme the time point...? issue of how to link time point number with date/time
  #   output$raster_check <- renderPlot(plot(target_raster))
  #   output$raster_info <- renderText(cat(dim(target_raster)))
  # }, ignoreInit = T)
  # 
  
  observe({
    if (input$TID == "View Output" | input$TID == "Download" )  {
      if (input$sendjobbutton == 0) {
      showModal(modalDialog(
        title = "Model outputs unavailable",
        div(id = "aa", style = "width: 1100px; height: 100px;", HTML("Run the model to access model outputs")),
        easyClose = TRUE
        ))
      }
    }
  })
  
  
  #output$raster_check <- renderPlot(plot(target_raster))

  # eventReactive(input$sel_sector, {
  #   #names(myFlux()$ls_ghgBySectorByTime) <- sectorNames
  #   leafletProxy('map') %>%
  #     addRasterImage(target_raster)
  #   })

  
  
  
  
  
  
  
  # observeEvent(input$gender == "Female", {
  #   leafletProxy("map", data=lnd) %>%
  #     clearShapes() %>%
  #     addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup2)
  # })

  # observe({
  #   names(myFlux()$ls_ghgBySectorByTime) <- sectorNames
  # 
  #   proxy <- leafletProxy('map')
  # 
  #   proxy %>%
  #     addRasterImage(myFlux()$ls_ghgBySectorByTime[1])
  # 
  # })

  
  
  
  
  
}


# Run the application using local file links
# runApp()

# Run the application 
shinyApp(ui = ui, server = server)