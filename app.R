# load libraries
    library(shiny)    
    library(leaflet)
    library(magrittr)
    
# Read data into R
    # monitoring data
        # mon.temp <- tempfile()
        # download.file(url = 'https://data.ca.gov/node/2176/download', destfile = mon.temp, method = 'libcurl')
        # monitoring.data <- readr::read_csv(file = mon.temp)
        # unlink(mon.temp)
        # rm(mon.temp)
        # names(monitoring.data) <- make.names(names(monitoring.data))
        # read from saved file
            monitoring.data <- readr::read_csv(file = 'C:\\David\\Stormwater\\_SMARTS_Data_Download_Automation\\Industrial_Ad_Hoc_Reports_-_Parameter_Data_2018-07-16.csv')
    # facilities
        # fac.temp <- tempfile()
        # download.file(url = 'https://data.ca.gov/node/2171/download', destfile = fac.temp, method = 'libcurl')
        # facilities <- readr::read_csv(file = fac.temp)
        # unlink(fac.temp)
        # rm(fac.temp)
        # names(facilities) <- make.names(names(facilities))
        # read from saved file
            facilities <- readr::read_csv(file = 'C:\\David\\Stormwater\\_SMARTS_Data_Download_Automation\\Industrial_Application_Specific_Data_2018-07-16.csv')
        
    # filter the monitoring data so that it only includes effluent data
        monitoring.data <- monitoring.data %>% dplyr::filter(MONITORING_LOCATION_TYPE == 'Effluent Monitoring')
        
    # convert date field to a date class
        monitoring.data <- monitoring.data %>% dplyr::mutate(SAMPLE_DATE = lubridate::mdy(SAMPLE_DATE))
    
    # filter for reasonable dates (since the year 2000 or later, but nothing past today's date)
        monitoring.data <- monitoring.data %>% dplyr::filter(SAMPLE_DATE >= 2000/01/01 & SAMPLE_DATE <= Sys.time())
        
    # get the WB region from the WDID
        monitoring.data <- monitoring.data %>% dplyr::mutate(Region_calc = substr(x = WDID, start = 1, stop = 2))
        monitoring.data <- monitoring.data %>% dplyr::mutate(Region_calc = gsub(x = Region_calc, pattern = ' ', replacement = ''))
        
    # Create a column with ug/L Results converted to mg/L
        # monitoring.data <- monitoring.data %>% dplyr::mutate(Result.Conv = dplyr::if_else(UNITS=='ug/L', RESULT / 1000, RESULT))
        # monitoring.data <-  monitoring.data %>% dplyr::mutate(Unit.Conv = dplyr::if_else(UNITS=='ug/L', 'mg/L', UNITS))
        
    # Create a list of WDIDs
        WDID.list <- monitoring.data %>% dplyr::distinct(WDID)
        WDID.list <- WDID.list$WDID
        WDID.list <- c('All WDIDs', WDID.list)
        
    # GET GEOSPATIAL LAYERS (Regional Board Boundaries, 2012 303d Impaired Waters, ...)
        RB_Boundaries <- readr::read_rds('data/unsimplified_RB_Bounds.RDS')
        # get the bounds of CA
            CA.bounds <- attributes(sf::st_geometry(RB_Boundaries))$bbox
        
    # Get a list of parameters
        parameters <- monitoring.data %>% dplyr::distinct(PARAMETER)
        # parameters <- readr::read_csv(file = 'data/parameters.csv')
        
    # initialize map data dataframe
        # map.data <- monitoring.data[0, ]
        
    # get the min and max dates
        min.date <- min(monitoring.data$SAMPLE_DATE)
        max.date <- max(monitoring.data$SAMPLE_DATE)

            
# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
    ui <- navbarPage(title = "Industrial Stormwater Effluent Water Quality Assessment Tool", # theme = shinythemes::shinytheme('flatly'),
                     tabPanel('Home',
                              h3('Background:'),
                              p('This draft version of the Industrial Stormwater Assessment Tool is intended to summarize statewide industrial stormwater quality 
                                monitoring data reported to the ',
                                tags$a(href = 'https://www.waterboards.ca.gov/','California State Water Resources Control Board'), '. It computes the median of 
                                the results at each facility for a selected parameter, and displays the computed scores on a map and in tabular format.'),
                              # ('on the ', 
                              # tags$em('WQI Scores', class = 'linkstyle', onclick = "fakeClick('WQI Scores')"), 
                              # ' tab).'),
                              #br(),
                              hr(), #style="border: 1px solid darkgrey"),
                              #br(),
                              h3('Instructions:'),
                              p('In general, you will view and interact with the data through the WQI Scores tab.'), 
                              tags$ul(
                                  tags$li(tags$u(tags$b('WQI Scores:')),'The panel on the left side of this tab contains a menu with inputs that can be used to customize the WQI scores calcualted and 
                              displayed in the map, and in the corresponding data table below the map. Use the inputs in the menu to select a Water Board Region, 
                              Year, and Standard to apply in computing the WQI scores. You can also filter for sites whose computed WQI scores fall 
                              within a given range, sites which fall within CES tracts with a given range of scores for a selected CES parameter, 
                              and/or sites that are within a given proximity to 303(d) waterbodies. Within the map, you can select the background 
                              map and toggle layers on or off, through the menu in the upper right corner of the map. You can also view and download 
                              the a tabular summary of the WQI scores in the table below the map.'),
                                  tags$li(tags$u(tags$b('Additional Data:')), 
                                          'Contains links to download the complete set of sampling data and facility information considered in computing the median values.'),
                                  tags$li(tags$u(tags$b('More Information:', class = 'linkstyle')), 
                                          tags$ul(
                                              tags$li(tags$u(tags$b('Data Sources:')),
                                                      'Links to data sources used in this tool.'), 
                                              tags$li(tags$u(tags$b('Application Information:')),
                                                      'Access to the source code for this tool, and information about how to provide feedback or ask questions.')
                                          )
                                  )
                              ),
                              hr(), #style="border: 1px solid darkgrey"),
                              tags$a(href = 'https://github.com/CAWaterBoardDataCenter', 
                                     tags$img(src = 'data_center_logo_withText_crop_resize.png', width = '400px', height = '97px')) # , style = "border:1px solid ghostwhite"))
                     ),
                 tabPanel('Sampling Summary',
                          sidebarLayout(
                              sidebarPanel( # Sidebar with inputs 
                                  h4('Filters:'), 
                                  selectInput(inputId = 'parameter.selected', label = 'Parameter:', choices = c('', parameters$PARAMETER)),
                                  selectInput(inputId = 'region.selected', label = 'Water Board Region:', choices = c('All Regions', 1:4, '5R', '5S', '5F', '6A', '6B', 7:9), selected = 'All Regions'),
                                  dateRangeInput(inputId = 'dates.selected', label = 'Sample Date Range:', start = min.date, end = max.date, min = min.date, max = max.date),
                                  sliderInput(inputId = 'count.selected', label = 'Minimum Number of Samples:', min = 0, max = 100, value = 0),
                                  numericInput(inputId = 'min.selected', label = 'Minimum Value:', value = NULL),
                                  numericInput(inputId = 'max.selected', label = 'Maximum Value:', value = NULL),
                                  textOutput('min.max')
                              ),
                              mainPanel( # Show map and data table
                                  # h4('Water Quality Monitoring Results:'),
                                  leaflet::leafletOutput('monitoring.map',height = 500),
                                  hr(style="border: 3px solid darkgrey"),
                                  h5('Water Quality Monitoring Summary - Tabular Data:'),
                                  DT::dataTableOutput('results.table')# ,
                                  # hr(style="border: 3px solid darkgrey"),
                              )
                          )
                 ),
                 tabPanel('Additional Data',
                          h3('Download Additional Data:'),
                          br(),
                          h4('Sampling Data:'),
                          p('The button below downloads the disaggregated water quality sampling data for the selected paramater, Waterboard region, and date range (selected in the ', tags$b(em('Sampling Summary')), ' tab), as a csv file: '),
                          downloadButton(outputId = 'downloadRawData', label = 'Filtered Sampling Data', class = "buttonstyle"), HTML('&emsp;'),
                          br(), br(), br(),
                          h4('Facility Information:'),
                          p('The button below downloads detailed information for the facilities that reported sampling data for the selected paramater, Waterboard region, and date range (selected in the ', tags$b(em('Sampling Summary')), ' tab), as a csv file: '),
                          downloadButton(outputId = 'downloadFacilities', label = 'Filtered Facility Information', class = "buttonstyle")
                          ),
                 navbarMenu('More Information',
                            tabPanel('Data Sources',
                                     h3('Data Sources:'),
                                     tags$ul(
                                         tags$li(tags$b('Industrial Stormwater Data: '), 'This tool assesses industrial stormwater discharge monitoring data reported to the 
                                                 California State Water Resources Control Boards\'', 
                                                 a(href = 'https://smarts.waterboards.ca.gov', 'Storm Water Multiple Application and Report Tracking System (SMARTS)'), 
                                                 'database. Selected data from the SMARTS database is available via the ',
                                                 a(href = 'https://data.ca.gov/', 'California Open Data Portal'), 
                                                 ', where the data is refreshed daily to multiple different datasets. Each time the tool is opened, it accesses the most recent 
                                                 version of the relevant SMARTS datasets on the open data portal, including:'),
                                         tags$ul(
                                             tags$li(tags$a(href = 'https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/fe4712db-015a-4e92-a13f', 'Monitoring Data')),
                                             tags$li(tags$a(href = 'https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/a5f001af-abbb-4bc7-9196', 'Facility information'))
                                         )
                                    )
                            ),
                            tabPanel('Application Information', # Link to the code, etc...
                                     h3('Application Information:'),
                                     h4('Source Code:'),
                                     p('This tool was buit by staff at the ',
                                       a(href = 'https://www.waterboards.ca.gov/', 'California State Water Resources Control Board\'s'), 
                                       'Office of Information Management and Analysis (OIMA), using the ', 
                                       a(href = 'https://shiny.rstudio.com/', 'Shiny'), 
                                       ' package for the R programming language. The source code for this tool is available here:',  '  '),
                                     actionButton(inputId = 'github', label = 'Code on GitHub', icon = icon('github', class = 'fa-1x'),
                                                  class = 'buttonstyle',
                                                  onclick ="window.open('https://github.com/daltare/Stormwater_Enforcement_Tool')"),
                                     br(), br(),
                                     h4('Feedback and Questions:'),
                                     p('For quesitons or comments about this tool, you can send an email to: ', 
                                       a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov'),
                                       p('Alternatively, you can ',
                                         a(href = 'https://github.com/daltare/Stormwater_Enforcement_Tool/issues', 'open an issue on this project\'s GitHub page'),
                                         'to request a new feature, note a bug, or leave any other comments about the tool. Feedback is appreciated!'))
                            )
                            )
                 )
            
# Define server logic required to draw map and create the associated data table -------------------------------------
    server <- function(input, output, session) {
        # Filter for the selected parameter, region, and date range
            filtered.data <- reactive({
                monitoring.data %>%
                    dplyr::filter(if(input$parameter.selected == '') {PARAMETER == 'Nothing'} else {PARAMETER == input$parameter.selected}) %>% # if no parameter is selected, no data is returned (all is filtered out)
                    dplyr::filter(if(input$region.selected == 'All Regions' | input$region.selected == '') {TRUE} else {Region_calc == input$region.selected} ) %>% # if all regions are selected or a selection is not made, don't filter out any data (i.e. the TRUE statement); otherwise, filter for data in the selected region
                    dplyr::filter(SAMPLE_DATE >= input$dates.selected[1]) %>% 
                    dplyr::filter(SAMPLE_DATE <= input$dates.selected[2]) # %>% 
                    # dplyr::group_by(WDID) %>% # group the data by facility
                    # dplyr::summarize(max.value = max(RESULT, na.rm = TRUE), number.samples = n()) %>% # calculate summary statistics for each site
                    # dplyr::filter(number.samples >= input$count.selected) %>% # filter for the minimum number of samples
                    # dplyr::right_join(facilities, by = 'WDID') %>% # join the calculated statistics to the more detailed facility information
                    # dplyr::filter(!is.na(max.value)) %>%  # filter out all sites where there is no summary statistic
                    # dplyr::filter(max.value != -Inf) %>%  # -Inf is returned when all taking the max of a set of results that are all NAs
                    # dplyr::filter(if(is.na(input$min.selected)) {TRUE} else {max.value >= input$min.selected}) %>%
                    # dplyr::filter(if(is.na(input$max.selected)) {TRUE} else {max.value <= input$max.selected})
            })
        
        # Calculate statistics for each site/parameter combination, and filter for the selected number of samples per site and range of results
            map.data <- reactive({
                filtered.data() %>%
                    # dplyr::filter(if(input$parameter.selected == '') {PARAMETER == 'Nothing'} else {PARAMETER == input$parameter.selected}) %>% # if no parameter is selected, no data is returned (all is filtered out)
                    # dplyr::filter(if(input$region.selected == 'All Regions' | input$region.selected == '') {TRUE} else {Region_calc == input$region.selected} ) %>% # if all regions are selected or a selection is not made, don't filter out any data (i.e. the TRUE statement); otherwise, filter for data in the selected region
                    # dplyr::filter(SAMPLE_DATE >= input$dates.selected[1]) %>% 
                    # dplyr::filter(SAMPLE_DATE <= input$dates.selected[2]) %>% 
                    dplyr::group_by(WDID) %>% # group the data by facility
                    dplyr::summarize(max.value = max(RESULT, na.rm = TRUE), number.samples = n()) %>% # calculate summary statistics for each site
                    dplyr::filter(number.samples >= input$count.selected) %>% # filter for the minimum number of samples
                    dplyr::right_join(facilities, by = 'WDID') %>% # join the calculated statistics to the more detailed facility information
                    dplyr::filter(!is.na(max.value)) %>%  # filter out all sites where there is no summary statistic
                    dplyr::filter(max.value != -Inf) %>%  # -Inf is returned when all taking the max of a set of results that are all NAs
                    dplyr::filter(if(is.na(input$min.selected)) {TRUE} else {max.value >= input$min.selected}) %>%
                    dplyr::filter(if(is.na(input$max.selected)) {TRUE} else {max.value <= input$max.selected})
            })
            
        # Get the facilities with samples in the filtered.data dataset
            facilities.filtered <- reactive({
                facilities %>% 
                    dplyr::filter(WDID %in% filtered.data()$WDID)
            })
            
            # map.data.min <- reactive({min(map.data()$max.value)})
            # map.data.max <- reactive({max(map.data()$max.value)})
            # 
            # output$min.max <- renderText({
            #     paste0('Min: ', map.data.min(), ' | Max: ', map.data.max())
            # })
            
            observe({
                map.data.min <- min(map.data()$max.value)
                map.data.max <- max(map.data()$max.value)
                output$min.max <- renderText({
                    paste0('Min: ', map.data.min, ' | Max: ', map.data.max, ' | ', input$min.selected, '----', input$max.selected)
                })
            })
        
        # Create the shared map data (for the crosstalk package - links the map with the data table) ----
            shared.map.data <- crosstalk::SharedData$new(map.data)
            
        # Create the map ----
            output$monitoring.map <- leaflet::renderLeaflet({
                # create the empty map
                    l <- leaflet() #shared.map.data)
                # enter the basemap options to allow the user to select
                    basemap.options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
                # add the basemaps listed above to the map (for options, see: http://leaflet-extras.github.io/leaflet-providers/preview/)
                    for (provider in basemap.options) {
                        l <- l %>% leaflet::addProviderTiles(provider, group = provider)
                    }
                # add the min-map window
                    l <- l %>% leaflet::addMiniMap(tiles = basemap.options[[1]], toggleDisplay = TRUE, position = "bottomleft")
                # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
                    l <- l %>% htmlwidgets::onRender("
                                                     function(el, x) {
                                                     var myMap = this;
                                                     myMap.on('baselayerchange',
                                                     function (e) {
                                                     myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                                                     })
                                                     }")
                # Set the bounds of the map dynamically - initial view is based on the full extent of the WQI score points for the selected region, after that the map is based on the most recent bounds when a new option (standard, period, etc) is selected
                    # isolate(if (is.null(input$monitoring.map_bounds)) {
                    #     l <- l %>% leaflet::fitBounds(lng1 = bounds[[1]], lat1 = bounds[[2]], lng2 = bounds[[3]], lat2 = bounds[[4]])
                    #     # l <- l %>% leaflet::fitBounds(lng1 = min(monitoring.data.WQI$Longitude, na.rm = TRUE), lat1 = min(monitoring.data.WQI$Latitude, na.rm = TRUE), lng2 = max(monitoring.data.WQI$Longitude, na.rm = TRUE), lat2 = max(monitoring.data.WQI$Latitude, na.rm = TRUE))
                    # } else { # maintain the current view
                    #     l <- l %>% leaflet::setView(lng = mean(c(input$monitoring.map_bounds$west, input$monitoring.map_bounds$east)), lat = mean(c(input$monitoring.map_bounds$north, input$monitoring.map_bounds$south)), zoom = input$monitoring.map_zoom)                                
                    # })
                # center map on selected California
                    l <- l %>% leaflet::fitBounds(round(CA.bounds[[1]], 4),
                                                  round(CA.bounds[[2]], 4),
                                                  round(CA.bounds[[3]], 4),
                                                  round(CA.bounds[[4]], 4))
                # add regional board boundaries
                    l <- l %>% leaflet::addPolygons(data = RB_Boundaries,  
                                color = 'black', # "#444444",
                                weight = 2.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                fill = FALSE,
                                # fillOpacity = 0.5,
                                # fillColor = 'lightblue',
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                '<b>', 'Region Number: ', '</b>', RB_OFF, '<br/>',
                                                '<b>', 'Region Name: ', '</b>', RB_NAME),
                                group = 'Regional Board Boundaries')
                # create a button to re-center the map
                    l <- l %>% leaflet::addEasyButton(leaflet::easyButton(
                        icon="fa-globe", title="Center Map on California",
                        # fit to data points
                        # onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                        #                   min(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   min(map.data$Longitude, na.rm = TRUE), '],[',
                        #                   max(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   max(map.data$Longitude, na.rm = TRUE), ']]); }'))))
                        # fit to RB boundary
                        onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                                                   round(CA.bounds[[2]],4), ', ',
                                                   round(CA.bounds[[1]],4), '],[',
                                                   round(CA.bounds[[4]],4), ', ',
                                                   round(CA.bounds[[3]],4), ']]); }'))))
                # Add controls to select the basemap
                    l <- l %>% leaflet::addLayersControl(baseGroups = basemap.options,
                                                         overlayGroups = c('Effluent Discharge Monitorting Sites', 'Regional Board Boundaries'),
                                                         options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE))
                # Add the measuring tool
                    l <- l %>% leaflet::addMeasure(position = 'topleft')
                # output the map object
                    l
            })
            
        # Create the color palette for the map of the statistical results (gradient from low to high values)
            results.leaflet.pal <- reactive({
                leaflet::colorNumeric(
                # palette = colorRamp(c('olivedrab2', 'red3'), interpolate='spline'),
                palette = colorRamp(c('red3', 'olivedrab2'), interpolate='spline'),
                domain = map.data()$max.value,
                reverse = TRUE)
            })
            
        # Add the selected monitoring data ----
            observe({
                # add the points
                if (input$parameter.selected != '' & nrow(map.data()) > 0) {
                        leafletProxy('monitoring.map') %>% 
                            clearMarkers() %>%
                            addCircleMarkers(radius = 4,
                                             data = map.data(), # shared.map.data,
                                             lat = ~FACILITY_LATITUDE,
                                             lng = ~FACILITY_LONGITUDE,
                                             stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                             fill = TRUE, fillOpacity = 1, 
                                             fillColor = ~ results.leaflet.pal()(max.value),   # wqi.leaflet.pal(WQI),
                                             popup = ~paste0('<b>', '<u>', 'Facility Information:', '</u>', '</b>','<br/>',
                                                             '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                                             '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                                             '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                                             '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                                             '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                                             '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                                             '<br/>',
                                                             '<b>', '<u>', 'Results:', '</u>', '</b>','<br/>',
                                                             # '<b>', 'Total Samples: ', '</b>', Total.Samples,'<br/>',
                                                             # '<b>', 'Median: ', '</b>', median, '<br/>'),
                                                             '<b>', 'Maximum: ', '</b>', max.value, '<br/>',
                                                             '<b>', 'Number of Samples: ', '</b>', number.samples),
                                             group = 'Effluent Discharge Monitorting Sites') 
                    } else {
                        leafletProxy('monitoring.map') %>%
                            clearMarkers() %>% 
                            clearControls()
                    }
            })
            
        # Add the selected Regional Board boundaries ----
            observe({
                RB_selected <- if (input$region.selected == 'All Regions') {RB_Boundaries} else {RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected)}
                bounds <- attributes(sf::st_geometry(RB_selected))$bbox
                leafletProxy('monitoring.map') %>% 
                    clearShapes() %>%
                    addPolygons(data = RB_selected,  
                                color = 'black', # "#444444",
                                weight = 2.0,
                                smoothFactor = 1.0,
                                opacity = 1.0,
                                fill = FALSE,
                                # fillOpacity = 0.5,
                                # fillColor = 'lightblue',
                                highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                '<b>', 'Region Number: ', '</b>', RB_OFF, '<br/>',
                                                '<b>', 'Region Name: ', '</b>', RB_NAME),
                                group = 'Regional Board Boundaries') %>% 
                    leaflet::fitBounds(round(bounds[[1]], 4),
                                       round(bounds[[2]], 4),
                                       round(bounds[[3]], 4),
                                       round(bounds[[4]], 4))
            })
            
        # Add the legend ----
            observe({
                leafletProxy('monitoring.map') %>% 
                    clearControls() %>% 
                    leaflet::addLegend(position = "bottomright", pal = results.leaflet.pal(), values = map.data()$max.value, title = 'Results', opacity = 1, layerId = 'results.legend')#, bins = 2)
            })
        
        # Create the Data Table -----------------------------------------------------------
            output$results.table <- DT::renderDataTable(
                #map.data(), #shared.map.data, 
                map.data() %>% dplyr::select(-c("PERMIT_TYPE",
                                                "FACILITY_CONTACT_FIRST_NAME", 
                                                "FACILITY_CONTACT_LAST_NAME", 
                                                "FACILITY_TITLE", 
                                                "FACILITY_PHONE", 
                                                "FACILITY_EMAIL", 
                                                "CERTIFIER_BY",
                                                "CERTIFIER_TITLE",
                                                "CERTIFICATION_DATE")), 
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip', 
                               buttons = list('colvis', list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv', filename = 'WQI_Scores'),
                                                  list(extend='excel', filename= 'WQI_Scores')),
                                   text = 'Download Data' )),
                               scrollX = TRUE,
                               scrollY = 250, 
                               scroller = TRUE, 
                               deferRender = TRUE),
                class = 'cell-border stripe',
                server = FALSE, # crosstalk only works with server = FALSE
                rownames = FALSE
            )
            
        # Monitoring Data Download Button -------------------------------------------------
            output$downloadRawData <- downloadHandler(
                filename = 'Industrial_Stormwater_Monitoring_Data_Filtered.csv', 
                content = function(con) {
                    write.csv(filtered.data(), con, row.names = FALSE)
                },
                contentType = 'text/csv'
            )
            
        # Facilities Data Download -------------------------------------------------
            output$downloadFacilities <- downloadHandler(
                filename = 'Industrial_Stormwater_Facilities_Filtered.csv', 
                content = function(con) {
                    write.csv(facilities.filtered(), con, row.names = FALSE)
                },
                contentType = 'text/csv'
            )    
    }
            
# Run the application ----
    shinyApp(ui = ui, server = server)
