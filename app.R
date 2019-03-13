# load libraries
    library(shiny)    
    library(leaflet)
    library(magrittr)
    
# Read data into R
    # monitoring data
        mon.temp <- tempfile()
        download.file(url = 'https://data.ca.gov/node/2176/download', destfile = mon.temp, method = 'libcurl')
        monitoring.data <- readr::read_csv(file = mon.temp)
        unlink(mon.temp)
        rm(mon.temp)
        names(monitoring.data) <- make.names(names(monitoring.data))
        # read from saved file
            # monitoring.data <- readr::read_csv(file = unz('data/Industrial_Ad_Hoc_Reports_-_Parameter_Data_2019-03-12.zip',
            #                                               'Industrial_Ad_Hoc_Reports_-_Parameter_Data_2019-03-12.csv'))
    # facilities
        fac.temp <- tempfile()
        download.file(url = 'https://data.ca.gov/node/2171/download', destfile = fac.temp, method = 'libcurl')
        facilities <- readr::read_csv(file = fac.temp)
        unlink(fac.temp)
        rm(fac.temp)
        names(facilities) <- make.names(names(facilities))
        # read from saved file
            # facilities <- readr::read_csv(file = unz('data/Industrial_Application_Specific_Data_2019-03-12.zip',
            #                                          'Industrial_Application_Specific_Data_2019-03-12.csv'))
        
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
        
    # Look at cases where the result is NA - may indicate a non-detect
        # View(monitoring.data2 %>% dplyr::filter(is.na(RESULT)) %>% dplyr::group_by(RESULT_QUALIFIER, RESULT) %>% dplyr::summarize(count = n()))
        # set non-detects to zero
            monitoring.data <- monitoring.data %>% dplyr::mutate(RESULT = dplyr::case_when(RESULT_QUALIFIER == 'ND' & is.na(RESULT) ~ 0,
                                                                                           TRUE ~ RESULT))
    
    # Standardize units
        # First, look at some individual parameters which have units that are difficult to convert
            # 'Asbestos' - 4 units (Counts/L, Fibers/L, mg/L, million fibers/L
                # View(monitoring.data %>% dplyr::filter(PARAMETER == 'Asbestos') %>% dplyr::group_by(UNITS) %>% dplyr::summarise(count = n()))
                # DROP THE 'Asbestos' DATA
                    monitoring.data <- monitoring.data %>% dplyr::filter(PARAMETER != 'Asbestos')
            # 'BOD5 @ 20 Deg. C, Percent Removal' - 2 units (%, mg/L)
                # View(monitoring.data %>% dplyr::filter(PARAMETER == 'BOD5 @ 20 Deg. C, Percent Removal') %>% dplyr::group_by(UNITS) %>% dplyr::summarise(count = n()))
                # DROP THE 'BOD5 @ 20 Deg. C, Percent Removal' data
                    monitoring.data <- monitoring.data %>% dplyr::filter(PARAMETER != 'BOD5 @ 20 Deg. C, Percent Removal') 
            # 'pH' - 3 units, but it appears they should all be the same (most are SU, also some  mg/L and ppth)
                # View(monitoring.data %>% dplyr::filter(PARAMETER == 'pH') %>% dplyr::group_by(UNITS) %>% dplyr::summarise(count = n()))
                # CONVERT ALL UNITS FOR 'pH' to 'SU'
                    monitoring.data <- monitoring.data %>% dplyr::mutate(UNITS = dplyr::case_when(PARAMETER == 'pH' ~ 'SU',
                                                                                                  TRUE ~ UNITS))
            # 'E.coli' - 2 units, most are MPN/100 mL, a few are ppth
                # View(monitoring.data %>% dplyr::filter(PARAMETER == 'E.coli') %>% dplyr::group_by(UNITS) %>% dplyr::summarise(count = n()))
                # for E coli, drop cases where units are 'ppth'
                    monitoring.data <- monitoring.data %>% dplyr::filter(!(PARAMETER == 'E.coli' & UNITS == 'ppth'))    
            # 'Enterococci MPN' - 3 units, most are MPN/100 mL, a few are CFU/100 mL or ppth
                # View(monitoring.data %>% dplyr::filter(PARAMETER == 'Enterococci MPN') %>% dplyr::group_by(UNITS) %>% dplyr::summarise(count = n()))
                # for Enterococci MPN, drop cases where units are 'ppth' or 'CFU/100 mL'
                    monitoring.data <- monitoring.data %>% dplyr::filter(!(PARAMETER == 'Enterococci MPN' & UNITS == 'ppth'))
                    monitoring.data <- monitoring.data %>% dplyr::filter(!(PARAMETER == 'Enterococci MPN' & UNITS == 'CFU/100 mL'))
        # # Second, get a summary of all of the analyte / unit combinations
            # NOTE: the following steps only need to be done once, so they are commented out, and the results are written to a csv file
        #     # Count analyte / unit combinations
        #         conv.param.unit <- monitoring.data %>% dplyr::group_by(PARAMETER, UNITS) %>% dplyr::summarize(count = n())
        #         conv.units.count <- conv.param.unit %>% dplyr::group_by(PARAMETER) %>% dplyr::summarise(unit.types = n())
        #     # get a list of all parameters reported with 2 or more units
        #         conv.units.list <- conv.units.count # %>% dplyr::filter(unit.types > 1)
        #         conv.units.list$max.unit <- NA
        #         conv.units.list$second.unit <- NA
        #         conv.units.list$third.unit <- NA
        #         conv.units.list$fourth.unit <- NA
        #         for (i in seq(nrow(conv.units.count))) { # conv.units.list))) {
        #             conv.param <- conv.param.unit %>% dplyr::filter(PARAMETER == conv.units.list$PARAMETER[i]) %>% dplyr::arrange(dplyr::desc(count))
        #             conv.units.list$max.unit[i] <- conv.param$UNITS[1]
        #             conv.units.list$second.unit[i] <- conv.param$UNITS[2]
        #             conv.units.list$third.unit[i] <- conv.param$UNITS[3]
        #             conv.units.list$fourth.unit[i] <- conv.param$UNITS[4]
        #         }
        #     # View a table showing all of the unit conversions required
        #         View(conv.units.list %>% dplyr::select(max.unit, second.unit, third.unit, fourth.unit) %>% dplyr::distinct() %>% dplyr::arrange(max.unit))
        #     # Write out a list of the parameter:unit combinations
        #         readr::write_csv(x = conv.units.list, path = 'data/Parameters_Units_List.csv')
            # Read in the list mapping each parameter to its most commonly reported unit, created from the steps above
                unit.conversions <- readr::read_csv(file = 'data/Parameters_Units_List.csv')
            # Define Unit Conversions
                conv_ugL_to_mgL <- 0.001 # ug/L to mg/L
                conv_ugL_to_pgL <- 1000000 # ug/L to pg/L
                conv_pgL_to_ugL <- 10^-6 # pg/L to ug/L 
                conv_mgL_to_ugL <- 1000 # mg/L to ug/L
                conv_ngL_to_ugL <- 0.001 # ng/L to ug/L
            
            
# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
    ui <- navbarPage(title = "Industrial Stormwater Effluent Water Quality Assessment Tool          ", # theme = shinythemes::shinytheme('flatly'),
                     tabPanel('Home',
                              h3('Background:'),
                              p('This draft version of the Industrial Stormwater Assessment Tool is intended to summarize statewide industrial stormwater quality 
                                monitoring data reported to the ',
                                tags$a(href = 'https://www.waterboards.ca.gov/','California State Water Resources Control Board.'), ' It computes basic summary statistics 
                                for each regulated facility for a selected parameter, and displays the summary statistics on a map and in tabular format.'),
                              # tags$em('Sampling Summary', class = 'linkstyle', onclick = "fakeClick('Sampling Summary')"), 
                              # ' tab).'),
                              #br(),
                              hr(), #style="border: 1px solid darkgrey"),
                              h3('Instructions:'),
                              p('This tool displays a summary of the water quality monitoring data on an interacitve map and associated data table in the ', 
                                tags$b(em('Sampling Summary')),
                                ' tab, with additional supporting information contained in the remaining tabs, as described below:'),
                              tags$ul(tags$li(tags$u(tags$b('Sampling Summary:')),
                                              'The panel on the left side of this tab contains a menu with filters to customize the information displayed in the 
                                              map and in the corresponding data table below the map. Use the filters in the upper portion of the menu to select a 
                                              parameter and Water Board Region to investigate. The filters in the middle portion of the menu can optionally be used 
                                              to filter the sample data considered in computing the summary statistics (including the range of sample dates and/or 
                                              sample results to consider). The filters in the lower portion of the menu can be used to filter the range of 
                                              statistical results displayed on the map and in the table (including the statistical result displayed on the map, 
                                              the range of results for that statistic, and the mimumum number of samples), and to scale the points plotted on the 
                                              map to the magnitude of the selected statistic. Within the map, you can select the background layer and toggle layers 
                                              on or off (through the menu in the upper right corner of the map). You can also view and download a tabular version 
                                              of the summary statistics in the table below the map.'),
                                      tags$li(tags$u(tags$b('Additional Data:')), 
                                              'Contains links to download the complete set of sampling data and facility information considered in computing the 
                                              summary statistics.'),
                                      tags$li(tags$u(tags$b('More Information:', class = 'linkstyle')), 
                                              tags$ul(
                                                  tags$li(tags$u(tags$b('Data Sources:')), 'Links to data sources used in this tool.'), 
                                                  tags$li(tags$u(tags$b('Application Information:')), 'Access to the source code for this tool, and information 
                                                          about how to provide feedback or ask questions.')
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
                                  # h3('Filters:'), 
                                  selectInput(inputId = 'parameter.selected', label = 'Parameter:', choices = c('', parameters$PARAMETER)),
                                  selectInput(inputId = 'region.selected', label = 'Water Board Region:', choices = c('All Regions', 1:4, '5R', '5S', '5F', '6A', '6B', 7:9), selected = 'All Regions'),
                                  hr(style="border: 1px solid darkgrey"),
                                  h4('Filter Sample Data Considered:'),
                                  dateRangeInput(inputId = 'dates.selected', label = 'Sample Date Range:', start = min.date, end = max.date, min = min.date, max = max.date),
                                  # h5('Range of Sampling Data Considered: '),
                                  tags$b(textOutput('unit.reported.sampling')),
                                  numericInput(inputId = 'min.selected.samples', label = 'Minimum Value:', value = NULL),
                                  numericInput(inputId = 'max.selected.samples', label = 'Maximum Value:', value = NULL),
                                  hr(style="border: 1px solid darkgrey"),
                                  h4('Filter Statistical Results Reported:'),
                                  selectInput(inputId = 'statistic.selected', label = 'Statistic to Plot:', choices = c('Median', 'Maximum')),
                                  checkboxInput(inputId = 'marker.size', label = 'Scale marker size by selected statistic', value = FALSE),
                                  tags$b(textOutput('unit.reported.statistic')),
                                  # h5('Range of Results for the Selected Statistic:'),
                                  numericInput(inputId = 'min.selected.result', label = 'Minimum Value:', value = NULL),
                                  numericInput(inputId = 'max.selected.result', label = 'Maximum Value:', value = NULL),
                                  sliderInput(inputId = 'count.selected', label = 'Minimum Number of Samples:', min = 0, max = 100, value = 0)#,
                                  # hr(style="border: 1px solid darkgrey"),
                              ),
                              mainPanel( # Show map and data table
                                  # h4('Water Quality Monitoring Results:'),
                                  leaflet::leafletOutput('monitoring.map',height = 500),
                                  hr(style="border: 3px solid darkgrey"),
                                  h5('Water Quality Monitoring Summary - Tabular Data:'),
                                  DT::dataTableOutput('results.table')
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
                                                  onclick ="window.open('https://github.com/daltare/NAS-Stormwater-Tool')"),
                                     br(), br(),
                                     h4('Feedback and Questions:'),
                                     p('For quesitons or comments about this tool, you can send an email to: ', 
                                       a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov'),
                                       p('Alternatively, you can ',
                                         a(href = 'https://github.com/daltare/NAS-Stormwater-Tool/issues', 'open an issue on this project\'s GitHub page'),
                                         'to request a new feature, note a bug, or leave any other comments about the tool. Feedback is appreciated!'))
                            )
                            )
                 )
            
# Define server logic required to draw map and create the associated data table -------------------------------------
    server <- function(input, output, session) {
        # convert the results for the selected parameter to the standardized unit
            unit.out <- reactive({
                if (input$parameter.selected == '') {
                    as.character(NA)
                } else {
                    as.character((unit.conversions %>% dplyr::filter(PARAMETER == input$parameter.selected) %>% dplyr::select(max.unit))[1,1])
                }
            })
            
            # output the reporting unit
                output$unit.reported.statistic <- renderText({
                    if (input$parameter.selected == '') {
                        paste0('Range of Results for the Selected Statistic:')
                    } else {
                        paste0('Range of Results for the Selected Statistic (', unit.out(), '):')
                    }
                })
                
                output$unit.reported.sampling <- renderText({
                    if (input$parameter.selected == '') {
                        paste0('Range of Sampling Data Considered:')
                    } else {
                        paste0('Range of Sampling Data Considered (', unit.out(), '):')
                    }
                })
                
            
            unit.types.count <- reactive({
                if (input$parameter.selected == '') {
                    as.numeric(NA)
                } else {
                    as.numeric((unit.conversions %>% dplyr::filter(PARAMETER == input$parameter.selected) %>% dplyr::select(unit.types))[1,1])
                }
            })
            
            converted.data <- reactive({
                monitoring.data %>% 
                    dplyr::filter(PARAMETER == input$parameter.selected) %>% 
                    dplyr::mutate(converted.units = unit.out()) %>% 
                    dplyr::mutate(converted.result = dplyr::case_when(UNITS == converted.units ~ RESULT,
                                                                      UNITS == 'mg/L' & converted.units == 'ug/L' ~ RESULT * conv_mgL_to_ugL,
                                                                      UNITS == 'ng/L' & converted.units == 'ug/L' ~ RESULT * conv_ngL_to_ugL,
                                                                      UNITS == 'pg/L' & converted.units == 'ug/L' ~ RESULT * conv_pgL_to_ugL,
                                                                      UNITS == 'ug/L' & converted.units == 'mg/L' ~ RESULT * conv_ugL_to_mgL,
                                                                      UNITS == 'ug/L' & converted.units == 'pg/L' ~ RESULT * conv_ugL_to_pgL))
                })

        # Filter for the selected region, date range, and value range
            filtered.data <- reactive({
                converted.data() %>%
                    dplyr::filter(if(input$region.selected == 'All Regions' | input$region.selected == '') {TRUE} else {Region_calc == input$region.selected} ) %>% # if all regions are selected or a selection is not made, don't filter out any data (i.e. the TRUE statement); otherwise, filter for data in the selected region
                    dplyr::filter(SAMPLE_DATE >= input$dates.selected[1]) %>% # filter for the min sample date
                    dplyr::filter(SAMPLE_DATE <= input$dates.selected[2]) %>%  # filter for the max sample date
                    dplyr::filter(if(is.na(input$min.selected.samples)) {TRUE} else {converted.result >= input$min.selected.samples}) %>%
                    dplyr::filter(if(is.na(input$max.selected.samples)) {TRUE} else {converted.result <= input$max.selected.samples})
            })
        
        # Calculate statistics for each site/parameter combination, and filter for the selected number of samples per site and range of results
            map.data <- reactive({
                filtered.data() %>%
                    dplyr::group_by(WDID, converted.units) %>% 
                    dplyr::summarize(maximum.value = max(converted.result, na.rm = TRUE), 
                                     median.value = median(converted.result, na.rm = TRUE), 
                                     samples_count = n()) %>% # calculate summary statistics for each site
                    dplyr::mutate(maximum.value.reported = dplyr::case_when(maximum.value == 0 ~ 'Not Detected',
                                                                        TRUE ~ as.character(maximum.value))) %>% 
                    dplyr::mutate(median.value.reported = dplyr::case_when(median.value == 0 ~ 'Not Detected',
                                                                           TRUE ~ as.character(median.value))) %>% 
                    dplyr::select(WDID, median.value, maximum.value, maximum.value.reported, median.value.reported, units = converted.units, samples_count) %>% 
                    dplyr::filter(samples_count >= input$count.selected) %>% # filter for the minimum number of samples
                    dplyr::right_join(facilities, by = 'WDID') %>% # join the calculated statistics to the more detailed facility information
                    dplyr::filter(!is.na(maximum.value)) %>%  # filter out all sites where there is no summary statistic
                    dplyr::filter(maximum.value != -Inf) %>% # -Inf is returned when all taking the max of a set of results that are all NAs
                    # filter for sites where the statistic is within the selected range
                    dplyr::filter(if (input$statistic.selected == 'Median' & !is.na(input$min.selected.result)) {
                        median.value >= input$min.selected.result} else {TRUE}) %>% 
                    dplyr::filter(if (input$statistic.selected == 'Median' & !is.na(input$max.selected.result)) {
                        median.value <= input$max.selected.result} else {TRUE}) %>% 
                    dplyr::filter(if (input$statistic.selected == 'Maximum' & !is.na(input$min.selected.result)) {
                        maximum.value >= input$min.selected.result} else {TRUE}) %>% 
                    dplyr::filter(if (input$statistic.selected == 'Maximum' & !is.na(input$max.selected.result)) {
                        maximum.value <= input$max.selected.result} else {TRUE}) 
            })
            
        
            
        # Get the facilities with samples in the filtered.data dataset (for data download button)
            facilities.filtered <- reactive({
                facilities %>% 
                    dplyr::filter(WDID %in% filtered.data()$WDID)
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
                domain = if (input$statistic.selected == 'Median') {
                    map.data()$median.value} else if (input$statistic.selected == 'Maximum') {
                        map.data()$maximum.value},
                reverse = TRUE)
            })
            
        # Add the selected monitoring data ----
            observe({
                # add the points
                map.data.temp <- map.data()
                range.median <- range(map.data.temp$median.value[map.data.temp$median.value > 0])
                range.maximum <- range(map.data.temp$maximum.value[map.data.temp$maximum.value > 0])
                min.median <- min(map.data.temp$median.value[map.data.temp$median.value > 0])
                min.maximum <- min(map.data.temp$maximum.value[map.data.temp$maximum.value > 0])
                input$statistic.selected
                if (input$parameter.selected != '' & nrow(map.data()) > 0) {
                        leafletProxy('monitoring.map') %>% 
                            clearMarkers() %>%
                            addCircleMarkers(data = map.data(), # shared.map.data,
                                             # radius = 4,
                                             # radius = ~ if (input$statistic.selected == 'Median') {max(4,log10((10 / min.median) * median.value))} else if (input$statistic.selected == 'Maximum') {maximum.value},
                                             radius = ~ if (input$marker.size) {
                                                 if (input$statistic.selected == 'Median') {
                                                     ((median.value - range.median[1]) / ((range.median[2] - range.median[1]))/0.5 + 1) * 4} else if (input$statistic.selected == 'Maximum') {
                                                         ((maximum.value - range.maximum[1]) / ((range.maximum[2] - range.maximum[1]))/0.5 + 1) * 4}
                                             } else {4},
                                             lat = ~FACILITY_LATITUDE,
                                             lng = ~FACILITY_LONGITUDE,
                                             stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                             fill = TRUE, fillOpacity = 1, 
                                             # fillColor = ~ results.leaflet.pal()(maximum.value),   # leaflet.pal(wq),
                                             fillColor = ~ if (input$statistic.selected == 'Median') {results.leaflet.pal()(median.value)} else if (input$statistic.selected == 'Maximum') {results.leaflet.pal()(maximum.value)},
                                             popup = ~paste0('<b>', '<u>', 'Facility Information:', '</u>', '</b>','<br/>',
                                                             '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                                             '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                                             '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                                             '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                                             '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                                             '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                                             '<br/>',
                                                             '<b>', '<u>', 'Results:', '</u>', '</b>','<br/>',
                                                             '<b>', 'Median: ', '</b>', median.value.reported, ' ', ifelse(median.value.reported == 'Not Detected', '', units), '<br/>', # if(median.value.reported == 'Not Detected') {''} else {units}, '<br/>',
                                                             '<b>', 'Maximum: ', '</b>', maximum.value.reported, ' ', ifelse(maximum.value.reported == 'Not Detected', '', units), '<br/>', # if(maximum.value.reported == 'Not Detected') {''} else {units}
                                                             '<b>', 'Number of Samples: ', '</b>', samples_count),
                                             group = 'Effluent Discharge Monitorting Sites'#,
                                             #clusterOptions = markerClusterOptions()
                                             )
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
                    leaflet::addLegend(position = "bottomright", 
                                       pal = results.leaflet.pal(), 
                                       values = if (input$statistic.selected == 'Median') {map.data()$median.value} else if (input$statistic.selected == 'Maximum') {map.data()$maximum.value}, 
                                       title = 'Results', 
                                       opacity = 1, 
                                       layerId = 'results.legend')#, bins = 2)
            })
        
        # Create the Data Table -----------------------------------------------------------
            output$results.table <- DT::renderDataTable(
                #map.data(), #shared.map.data, 
                map.data() %>% 
                    dplyr::select(-c("PERMIT_TYPE",
                                                "FACILITY_CONTACT_FIRST_NAME", 
                                                "FACILITY_CONTACT_LAST_NAME", 
                                                "FACILITY_TITLE", 
                                                "FACILITY_PHONE", 
                                                "FACILITY_EMAIL", 
                                                "CERTIFIER_BY",
                                                "CERTIFIER_TITLE",
                                                "CERTIFICATION_DATE",
                                                'median.value', 
                                                'maximum.value')) %>% 
                    dplyr::rename(sampling_median = median.value.reported, sampling_maximum = maximum.value.reported) %>% 
                    dplyr::rename_all(toupper), 
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip', 
                               buttons = list('colvis', list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv', filename = 'Industrial_Stormwater_Monitoring_Summary_Statistics'),
                                                  list(extend='excel', filename= 'Industrial_Stormwater_Monitoring_Summary_Statistics')),
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
