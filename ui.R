#-------------------------------------------------------------
# This is the user-interface definition of Cargo IS Miner. You can
# run the application by clicking 'Run App' above.
#
# Xuzhou QIN, xuzhou.qin@airbus.com
#-------------------------------------------------------------


library(shinydashboard)


#--------------------------------------
# _____________Welcome_________ -------
#--------------------------------------
## db_connection_check ######
# db_connection_check <-   box(title = 'Database connection check',width = 8, solidHeader = TRUE,collapsible = TRUE,
#     valueBoxOutput('check_cargois',width = 3),
#     valueBoxOutput('check_sea',width = 3),
#     valueBoxOutput('check_BIO',width = 3),
#     valueBoxOutput('check_fr24',width = 3)
#   )

#===============================================
# _____________Cargo IS___________ ======
#===============================================

## cargo.is.input: Cargo IS query input ##########
cargo.is.input <- box(width = 4,
                      title = 'Cargo IS query', status = 'warning', solidHeader = TRUE,
                      selectizeInput(
                        'level', 'Query level', choices = choice.level
                      ),
                      # selectizeInput(
                      #   'year', 'Year', choices = choice.year
                      # ),
                      uiOutput(
                        'year_cargois_ui'     ),
                      
                      # uiOutput(
                      #   'cargois_ui_org'
                      # ),
                      # uiOutput(
                      #   'cargois_ui_dst'
                      # ),
                      
                      selectizeInput('org', 'Origin', choices = NULL),

                      selectizeInput('dst', 'Destination', choices = NULL),
                      
                      # textInput('org','Origin'),
                      # textInput('dst','Destination'),
                      
                      checkboxInput('yld_evo', 'Plot evolution since 2010 (this could take several minutes)',
                                    FALSE),
                      checkboxInput('min_max_avg', 'Show min/max/average',
                                    FALSE),
                      actionButton('go2','Load data',
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
                      downloadButton('dl_cargo_is', 'Download raw data')
                      
                      #htmlOutput("text2")
)

## valueboxes: value Boxes #######

valueboxes = fluidRow(
                valueBoxOutput('box.count_obs',width = 2),
                valueBoxOutput('box.count_apt_pair',width = 2),
                valueBoxOutput('box.count_ORG',width =2),
                valueBoxOutput('box.count_DST',width = 2),
                valueBoxOutput('box.avg_weight',width = 2),
                valueBoxOutput('box.ave_yield',width = 2)
              )

## cargo.is.preview: preview the table DATA #########
cargo.is.preview <-  box(width = 8, title = 'Preview of the first 50 rows',
                         div(dataTableOutput('table_preview'), style = 'overflow-x: scroll')
                     )

## top.airport: top airport, air routes ########


top.airport = tabBox(width = 8,
                     tabPanel(title = 'Top Airport',
                              # div(style="display:inline-block",
                              #     sliderInput('pt.size.top.apt','Select the point size', 
                              #                 min = 1, max = 100, value = 50)),
                              # div(style="display:inline-block",
                              #     sliderInput('nb.top','Enter the number of airport/air routes to display',
                              #                 min = 1, max = 50, value = 20)),
                              
                              # sliderInput('pt.size.top.apt','Select the point size', 
                              #             min = 1, max = 100, value = 50),
                              # sliderInput('nb.top','Enter the number of airport/air routes to display',
                              #             min = 1, max = 50, value = 20),
                              
                              plotlyOutput('top.airport')),
                     
                     tabPanel(title = 'Top air route (O&D)', plotlyOutput('top.air.route')),
                     tabPanel(title = 'Airports data table', DT::dataTableOutput('topairport.table')),
                     tabPanel(title = 'Air routes data table', DT::dataTableOutput('topairroute.table'))
)####

fluidrow.top_apt_inputs <- fluidRow(box(width = 4,                            
                                      sliderInput('pt.size.top.apt','Select the point size',
                                                min = 1, max = 100, value = 50),
                                      sliderInput('nb.top','Enter the number of airport/air routes to display',
                                                min = 1, max = 50, value = 20),
                                      p('thickness = weight, color = yield')),
                                    top.airport
)

## fluidrow.Treemap: Cargo IS origin/destination treemaps ########
fluidrow.Treemap = fluidRow(
                    tabBox(
                      title = 'Origin Countries Treemap', 
                      # tabPanel('Plot',plotOutput('tree.org_cty')),
                      
                      tabPanel('Plot',
                               div(htmlOutput('tree.org_cty_gvis'), 
                                   style = 'overflow-x: scroll')),
                      
                      tabPanel('Table', dataTableOutput('tree.org_cty_dtable'))
                    ),
                    
                    tabBox(
                      title = 'Destination Countries Treemap', 
                      
                      tabPanel('Plot', 
                               div(htmlOutput('tree.dst_cty_gvis'), 
                                   style = 'overflow-x: scroll')),
                      
                      tabPanel('Table', dataTableOutput('tree.dst_cty_dtable'))
                    )
                  )

## fluidrow.Yield_evo: Yield evolution ########
fluidrow.Yield_evo =fluidRow(
                      tabBox(width = 12,
                      #    
                      #     # actionButton('go.allvalue','Yield evolution plot'),
                      # tabPanel( title = 'Yield Evolution',  plotlyOutput('evo_yield')),
                      # tabPanel('Weight Evolution',plotlyOutput('evo_weight') ),
                      # tabPanel('Table', dataTableOutput('evo_plot_dtable')),
                      tabPanel('Yield Evolution', 
                               div(htmlOutput('yld_evo_cargois_gvis'),
                                  style = 'overflow-x: scroll')),
                      
                      tabPanel('Table', dataTableOutput('evo_plot_dtable'))
                          
                      )
                    )

## box.yield.calc.inp: Yield calculator input #########

box.yield.calc.inp <- box( width = 3,
                           title = 'Inputs', status = 'warning', solidHeader = TRUE,
                           fileInput('file1', 'Upload the airport list',
                                     accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       'text/tab-separated-values',
                                       'text/plain',
                                       '.csv',
                                       '.tsv'
                                     )
                           ), 
                           selectizeInput(
                             'year2', 'Select the year', choices = choice.year
                           ),
                           tags$hr(),
                           checkboxInput('header', 'Header', FALSE),
                           radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';', Tab='\t'),','),
                           radioButtons('quote', 'Quote',c(None='','Double Quote'='"', 
                                                           'Single Quote'="'"),'"'),
                           actionButton('go','Start'),downloadButton('downloadData', 'Download')
)

## box.yield.calc.out: Yield calculator output #########
box.yield.calc.out <- box(width = 9, 
                          title = 'Yields', status = 'primary', solidHeader = TRUE,collapsible = TRUE,
                          DT::dataTableOutput('datatable'))

## box.yield.calc.map: Mapping the air routes in the results  #########
box.yield.calc.map <- box(width = 3,
                          selectizeInput(
                            'projection', 'Select map projection', choices = choice.projection
                          ),
                          selectizeInput(
                            'compare', 'Select the criteria', choices = choice.compare
                          ),
                          # selectizeInput(
                          #   'color','Select line color', choices = choice.color
                          # ),
                          # selectizeInput(
                          #   'pt.color','Select point color', choices = choice.color
                          # ),
                          # selectizeInput(
                          #   'pt.size','Select point size', choices = choice.size
                          # ),
                          textInput('color','Line color','#3498DB'),
                          textInput('pt.color','Point color'),
                          textInput('pt.size','Point size', 8)
)

## box.yield.calc.map.out: output the map #########
box.yield.calc.map.out <- box(width = 9, height = 700,
                              title = 'Mapping the air routes, color = yield, thickness = weight', status = 'primary', solidHeader = TRUE,collapsible = TRUE, 
                              plotlyOutput('map')
)

## fluidRow.other_analysis ####
fluidRow.other_analysis <- fluidPage(
  
  # _input parameters ####
  
  # column(width = 3,
  #        div(box(title = 'Parameters', solidHeader = TRUE,
  #                 status = 'warning',uiOutput('select_wb'),width = '100%',
  #                selectInput('cargois_versus_plot_gyny','Choose the yield',
  #                            choices = list('Gross yield' = 'gy',
  #                                           'Net yield' = 'ny')),
  #            textInput('cargois_width','Set the width of download file',30),
  #            textInput('cargois_height','Set the height of download file',15)),
  #            class = 'absolute_box')), 
  # column(width = 9, 
  #     ## __ plos ####
  #     fluidRow(
  #     box(width = '100%', plotOutput('freq_shipment_evo'),
  #         downloadButton('cargois_dl_freq_shipment_evo', 'download PNG'))
  #   ),
  #   fluidRow(
  #     # plotOutput('dist_weight_awb'),
  #     box(width = '100%', plotOutput('dist_weight_awb'),
  #         downloadButton('cargois_dl_dist_weight_awb', 'download PNG')
  #            )
  # 
  #   ),
  
    fluidRow(
      column(width = 3, 
          box(title = 'Parameters', 
              solidHeader = TRUE,
              status = 'warning',uiOutput('select_wb'),width = '100%',
              
              radioButtons('cargois_versus_plot_gyny','Choose the yield',
                          choices = list('Gross yield' = 'gy',
                                         'Net yield' = 'ny')),
              
              # radioButtons('cargois_versus_plot_avg_para', 'Choose the arithmetic',
              #              choices = list('Average yield' = 'normal',
              #                             'Weighted average yield' = 'weight')),
              
              textInput('cargois_width','Set the width of download file',30),
              
              textInput('cargois_height','Set the height of download file',15)
          )
      ),
      
      
      ## __versus plot ####
      column(width = 9,
          tabBox(width = '100%', 
              tabPanel('Shipment freq evoluion', plotOutput('freq_shipment_evo'),
                      downloadButton('cargois_dl_freq_shipment_evo', 'download PNG')
                      ),

              tabPanel('Shipment weight distribution', plotOutput('dist_weight_awb'),
                      downloadButton('cargois_dl_dist_weight_awb', 'download PNG')
                    ),

              tabPanel('Weight vs Distance',plotOutput('cargois_corr_plot'),
                      downloadButton('cargois_dl_weight_distance', 'download PNG')
                      ),
              tabPanel('Frequency vs Distance', plotOutput('cargois_freq_distance'),
                      downloadButton('cargois_dl_freq_distance', 'download PNG')
                      ),
              tabPanel('Revenue vs Distance', plotOutput('cargois_revenu_distance'),
                      downloadButton('cargois_dl_revenu_distance', 'download PNG')
                      ),
              tabPanel('Yield vs Distance', plotOutput('cargois_gy_distance'),
                      downloadButton('cargois_dl_gy_distance', 'download PNG')
              ),
              tabPanel('Avg_weight vs Distance', plotOutput('cargois_avgweight_distance'),
                      downloadButton('cargois_dl_avgweight_distance', 'download PNG')
              )
          )
      )
    ),
  
  fluidRow(
    box('Data preview', width = 12,
        div(dataTableOutput('table_versus_plot'), 
            style = 'overflow-x: scroll'),
        downloadButton('dl_table_versus_plot', 'Download data')
        )
  )
)


## fluidpage_stat_analysis ####
fluidpage_stat_analysis <- fluidPage(
  fluidRow(
    box(width = 3,
        uiOutput('cargois_ui_x_axis'),
        uiOutput('cargois_ui_y_axis')
        ),
    box(width = 9), 
    tableOutput('test3'),
    textOutput('text3')
    
  )
)


#===============================================
# ____________Seabury________ ==================
#===============================================

## fluidrow_Seabury: Seabury treemaps ########
fluidrow_Seabury = fluidRow(
  box(width = 3,
      title = 'Seabury query', status = 'warning', solidHeader = TRUE,
      h4(strong('---Treemap---')),
      selectizeInput(
        'level.sea', 'Query level', choices = choice.level.sea
      ),
      uiOutput('year.sea.ui'),
      # uiOutput('org.sea.ui'),
      # uiOutput('dst.sea.ui'),
      
      selectizeInput('org.sea', 'Origin', choices = NULL),
      selectizeInput('dst.sea', 'Destination', choices = NULL),
      
      # textInput('org.sea','Origin'),
      # textInput('dst.sea','Destination'),
      selectizeInput(
        'sfc.air', 'Select freight type', choices = choice.sfc.air
      ),
      actionButton('go.sea','Draw treemap'),
      
      downloadButton('dl_sea_treemap', 'Download data table'),
      
      # htmlOutput("text.sea"),
      ####
      
      # br(),
      h4(strong('---Evolution---')),
      # textInput('gxname','Enter GXNAME'),
      # tableOutput('gxcode.finder'),
      # uiOutput('gxcode.finder_ui'),
      
      selectizeInput('gxcode.finder', 'Select the commodity that you want to plot',
                     choices=NULL),
      # selectizeInput(
      #   'gx.query.level', 'Select GX level to query', choices = choice.level.sea.GX.level
      # ),
      checkboxInput('surface.air', 'Plot the surface freight (reclick "Plot selected")',
                    FALSE),
      checkboxInput('weight.value', 'Plot for the cargo weight (unchecked for the value)',
                    TRUE),
      checkboxInput('gx.evo.plot.all', 'Plot all the GX level (this could take several minutes)',
                    FALSE),
      actionButton('gx.evo.plot','Plot selected')
      # actionButton('gx.evo.plot.all','   Plot all   ')
      
  ),
  
  box(width = 9, collapsible = TRUE, collapsed = TRUE,
      title = 'How to use?', status = 'success', solidHeader = TRUE
  ),
  
  tabBox(width = 9, height = 700, 
         # tabPanel(title = 'G1 treemap', plotOutput('tree.g1', click="click1"),
         #          tableOutput('summary')
         # ),div(dataTableOutput('table_preview'), style = 'overflow-x: scroll')
         tabPanel(title = 'Commodity treemap', 
                  div(htmlOutput('tree.g1'), style = 'overflow-x: scroll')),
         # tabPanel(title = 'G2 treemap', plotOutput('tree.g2', click="click2"),
         #          tableOutput('summary2')
         # ),
         # tabPanel(title = 'G3 treemap', plotOutput('tree.g3', click="click3"),
         #          tableOutput('summary3')
         # ),
         # tabPanel(title = 'G4 treemap', plotOutput('tree.g4', click="click4"),
         #          tableOutput('summary4')
         # )
         tabPanel(title = 'Data table', 
                  dataTableOutput('sea_treemap_table')
         )
  ),
  
  
  #########  Seabury evolution 
  tabBox(width = 12, 
         tabPanel(title = 'G1 level evolution', plotlyOutput('sea.evo1')),
         tabPanel(title = 'G2 level evolution', plotlyOutput('sea.evo2')),
         tabPanel(title = 'G3 level evolution', plotlyOutput('sea.evo3')),
         tabPanel(title = 'G4 level evolution', plotlyOutput('sea.evo4')),
         tabPanel(title = 'Table', tableOutput('sea.evo.tab') )
  )
  
)

#===============================================
# ___________FlightRadar 24____________=========
#===============================================

## fluidrow_FR24_load: FlightRadar24 load #########
# querry FR24 DB:
# object: analyse for an airline, its fleets AC type, network, frequency of air routes,
#         
fluidrow_FR24_load <- fluidPage(
  fluidRow(
    
    # _query ####
    box( width = 4,
      title = 'FlightRadar24', status = 'warning', solidHeader = TRUE,
      dateRangeInput('fr24_daterange', 'Select the range of date',
                     start = '2015-9-14', end = '2015-9-20', startview = 'year'),
      textInput('fr24_airline.code', 'Enter the Airline IATA code'),
      textInput('fr24_select_org', 'Origin airport code',NA),
      textInput('fr24_select_dst', 'Destination airport code',NA),
      textInput('fr24_select_reg_number', 
                'Registration number (if multiple, separate with ";")',NA),
      textInput('fr24_select_ac_type', 
                'Aircraft type (if multiple, separate with ";")',NA),
      uiOutput('textaera_fr24'),
      
      # actionButton('go_fr24','Generate query') ,,

      actionButton('go2_fr24','Query FR24') ,      
      actionButton('fr24_refresh',' Enrich with BIO',icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      downloadButton('dl_fr24', 'Download data')
    ),
    
    box( width = 8,
         title = 'Raw data ummary',
         infoBoxOutput('fr24_info_nb_flight'),
         infoBoxOutput('fr24_info_nb_tail'),
         infoBoxOutput('fr24_info_nb_org'),
         infoBoxOutput('fr24_info_nb_dst')
    ),
    
    box( width = 8, 
         title = 'Frequency',
         uiOutput('fr24ui_freq_sel_col'),
         
         plotlyOutput('fr24_freq_check_dt_quality'),
         style = 'overflow-y: scroll; max-height: 700px'
    ),
    
    # _data Cleaner ####
    box( width = 12, collapsible = TRUE, 
      title = 'Data Cleaner',
      uiOutput('fr24ui_select_reg_number2'),
      uiOutput('fr24ui_select_flight_number2'),
      uiOutput('fr24ui_select_org2'),
      uiOutput('fr24ui_select_dst2')#,
      # actionButton('fr24_refresh',' Enrich with BIO',icon("paper-plane"), 
      #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  # *****TEST OUTPUT*****
  fluidRow(
    
    box(title = 'FlightRadar24 raw data preview', width = 12,
        dataTableOutput('test_fr24')),
    box(title = 'FR24 with BIO AC_TYPE',width = 12,
        p('if there is no information after [LANDING_TIME], this means that this aircraft is nolonger in service today. We do not have the current status in BIO database.'),
        div(dataTableOutput('text_fr24'),
            style = 'overflow-x: scroll')),
    
    tableOutput('test_fr24_tab')
    # textOutput('fr24_bio_ac_colnames'),
    # textOutput('test2_fr24')
  )
)

# fluidrow_FR24_airline ####
fluidrow_FR24_airline <- fluidPage(
  fluidRow(
    tabBox(width = '100%',
           tabPanel(title = 'Airport map', width = '100%',
                    # htmlOutput('fr24_airline_network_map')
        leafletOutput('fr24_airline_network_map', height = 500)
        
        ))#,
    # HTML(' 
    #      
    #      <div id="mapid"></div>
    #      ')
  )
  
)
fluidrow_FR24_airport <- fluidPage()
fluidrow_FR24_AC <- fluidPage()

#__________ITEM END_______________ -----------------------------------------------

# +++++++++++++++ UI STRUCTURE +++++++++++++++++ =====

# ______DASHBOARD HEAD SIDEBAR__________ ####
dashboardPage(
  # Dashboard Head ####
  
  #TODO: move the connection check to the notification
  dashboardHeader(title = "Cargo Data Miner", 
                  # dropdownMenu(type = "messages",
                  #   notificationItem(
                  #     text = "Cargo IS Miner alpha v0.2 06/06/2016",
                  #     icon("heart")
                  #   )
                  # )
                  dropdownMenuOutput('db_connection_check2'),
                            # dropdown menu of connection check
                  dropdownMenu(type = 'messages', 
                               notificationItem(text ='CDM_update_info', icon = shiny::icon('heart'))
                               ),
                  dropdownMenu(type = 'tasks', 
                               taskItem(text ='Cargo IS development', value = 70, color = 'aqua'),
                               taskItem(text ='Seabury development', value = 45, color = 'red'),
                               taskItem(text ='FlightRadar24 development', value = 20, color = 'yellow'),
                               taskItem(text ='Other tools', value = 0, color = 'green')
                               
                               
                  )
  ),
  # DASHBOARD SIDEBAR####
  dashboardSidebar(
    sidebarMenu(
      #-------------- New structure --------------
      menuItem('Home', tabName = 'welcome', icon = icon('home')),
      
      menuItem('Cargo IS', icon = icon('database'),
               menuSubItem('Data loader', tabName = 'cargois', icon = icon('cloud-download')),
               menuSubItem('Yield analysis', tabName = 'cargois_plot', icon = icon('line-chart')),
               menuSubItem('AWB analysis', tabName = 'cargois_other', icon = icon('bar-chart')),
               menuSubItem("Statistical analysis", tabName = "cargois_stat", icon = icon("superscript")),
               
               menuSubItem("Yield Calculator", tabName = "yield", icon = icon("calculator"))
               
      ),
      
      menuItem('Seabury', icon = icon('database'),
               menuSubItem('Seabury Trade', tabName = 'sea_trade', icon = icon('plane')),
               menuSubItem("Seabury Express", tabName = "sea_exp", icon = icon("gear"))
      ),
      
      menuItem('Flightradar24', icon = icon('database'),
               menuSubItem('Data loader FR24', tabName = 'loader_fr24', icon = icon('cloud-download')),
               menuSubItem('Airline network', tabName = 'fr24_airline', icon = icon('signal')),
               menuSubItem("Airport", tabName = "fr24_airport", icon = icon("gear")),
               menuSubItem("Aircraft", tabName = "fr24_AC", icon = icon("gear"))
               
      ),
      
      menuItem('Tools', icon = icon('gear'),
               menuSubItem('Code finder', tabName = 'tool1', icon = icon('search')),
               menuSubItem("developing", tabName = "tool2", icon = icon("gear"))
      )
      #-------------------------------------------
      

      #-------------- Old structure --------------
      # menuItem('Dashboard',tabName = 'dashboard', icon = icon('dashboard'), 
      #          # menuSubItem('Welcome', tabName = 'welcome', icon = icon('home')),
      #          menuSubItem('Cargo IS', tabName = 'cargois', icon = icon('dollar')),
      #          menuSubItem('Seabury Trade', tabName = 'sea_trade', icon = icon('briefcase')),
      #          menuSubItem('Flightradar24', tabName = 'fr24', icon = icon('map-marker')),
      #          menuSubItem('Test',tabName = 'test', icon = icon('ban'))),
      # 
      # menuItem("Yield Calculator", tabName = "yield", icon = icon("calculator"),badgeLabel = 'New', badgeColor = 'green'),
      # menuItem("Analysis", tabName = "analysis", icon = icon("map")),
      # menuItem("Map", tabName = "map", icon = icon("map"))
      #-------------------------------------------
      
    ),
    # _________add CSS________ ####
    # background image

    tags$head(tags$style(type = "text/css", 
                 "section.content {
                 background-image: url('07.jpg');
                 background-repeat: no-repeat;
                 background-attachment: fixed;
                 background-position: center;
                 min-height: 100vh;
                 -webkit-background-size: cover; /* pour anciens Chrome et Safari */
                  background-size: cover; /* version standardisée */

                 }"
                    )),
    
    # absolute box position ####
    tags$head(tags$style(type = "text/css", 
                         "div.absolute_box {
                      position: fixed;
                      top: 65px;
                      width: 18%;
                 }"
    )),
    
    tags$head(tags$style(type = 'text/css',
                         "
                         #mapid { height: 600px; }
                         
                         ")),
    
    # ----------- progres indicator ------------
    tags$head(tags$style(type="text/css", "
                           #loadmessage {
                           position: fixed;
                           bottom: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #FFFFFF;
                           background-color: #2E86C1;
                           z-index: 105;
                           }
                           ")),#2E86C1
    
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(img(src = 'airbus.png',height = 52, width = 216, 
                                  img(src= 'hourglass.gif')),
                              id="loadmessage")),
    
    tags$head(HTML('
                   
     <link rel="stylesheet" href="W3S.css">
     <link rel="stylesheet" href="https://npmcdn.com/leaflet@1.0.0-rc.3/dist/leaflet.css" />
     <script src="https://npmcdn.com/leaflet@1.0.0-rc.3/dist/leaflet.js"></script>
     <script src="arc.js"></script>
     <script src="Leaflet.Arc.min.js"></script>')),
    
    # # test keydown
    # conditionalPanel(condition="$('html').keypress(function(event) {
    #                  event.which == 78
    # })",
    #                  tags$div(img(src = 'airbus.png',height = 52, width = 216, 
    #                               img(src= 'hourglass.gif')),
    #                           id="loadmessage")),
    

    br(),
    
    br(),
    
    # ------------- R logo ####
    img(src="MROs.png",height = 53, width = 63, p((' Microsoft R Open \nVersion 3.2.5'))),
    
    h4("Cargo Data Miner alpha"),
    p('v0.2 06/06/2016')
    # p('v0.1 04/05/2016')
  ),
  
  # ________Dashboard tab content_________###### DISPLAY NEW created sections ----------------
  dashboardBody(


    tabItems(

      ## tab 0: Welcome ----
      tabItem(
        
        tabName = 'welcome', 
        bootstrapPage(
          fluidRow(
        
        # HTML('<center><img src="airbus_big.png"></center>'),
        # h1('Welcome to Cargo Data Miner alpha!', align = 'center'),
        # p('This is an web application written on R Shiny developed by CSMX. The
        #   objectif of this application is to facilitate the data mining process of our daily work.', align = 'center')


            HTML('


      
           <div class="w3-container w3-animate-opacity">
           <img style="display: block; margin-left: auto; margin-right: auto;" src="airbus_big.png">
           <div style="margin-left: auto; margin-right: auto; width: 700px">
           
           <div class="w3-card-2 w3-margin">
           <div class="w3-container w3-padding ">
           <h1 style="font-family: Lucida Console, Monaco, monospace" >
           Cargo Data Miner &alpha; </h1>
           </div>
           
           <div class="w3-container w3-white ">
           <p><i>Cargo Data Miner</i> is a R Shiny Dashboard application. It is developed by CSMX (Freighter Marketing). The initiative is to build a platform where everybody could query and analyze cargo data.
      <i>Cargo Data Miner</i> provides automatic data analysis and visualization for the following three databases:</p>
           
           <div class="w3-dropdown-hover">
           <span class="w3-tag w3-light-grey w3-small w3-margin-bottom">Cargo IS</span>
           <div style = "width: 400px;" class="w3-dropdown-content w3-border">
           <p style = "padding: 15px">Cargo IS provides weight, charges and revenu information for 64 countries.</p>
           <a href="http://www.iata.org/services/statistics/intelligence/cargois/Pages/index.aspx">Read more.</a>
           </div>
           </div>
           
           <div class="w3-dropdown-hover">
           <span class="w3-tag w3-light-grey w3-small w3-margin-bottom">Seabury</span>
           <div style = "width: 400px;" class="w3-dropdown-content w3-border">
           <p style = "padding: 15px">World air and ocean trade volumes are captured for over 200 countries from 1994 onwards.
           Data is available on a country to country level, with further breakdowns into customs districts
           for the largest freight consumption and production markets. Types of goods are provided at
           an unmatched level of detail for 2000 commodities.</p>
           <a href="http://www.seaburygroup.com/industry-expertise/software-data/cargo-and-global-trade-database/">Read more.</a>
           </div>
           </div>
           
           <div class="w3-dropdown-hover">
           <span class="w3-tag w3-light-grey w3-small w3-margin-bottom">FlightRadar 24</span>
           <div style = "width: 400px;" class="w3-dropdown-content w3-border">
           <p style = "padding: 15px">Flightradar24 is a live flight tracker that shows air traffic in real time. Data from 2014 to 2015 are available in Airbus server.</p>
           <a href="https://www.flightradar24.com/">Read more.</a>
           </div>
           </div>
           
      
           <P>The code is written using <a href="https://mran.revolutionanalytics.com">Microsoft R Open 3.2.5</a>, which is a distribution of R 3.2.5.
      The CSS framework comes from <a href="http://www.w3schools.com/w3css">W3.CSS</a>.
           </p>
      
           <p>All feedbacks are welcomed! You can reach me by
           <a href="mailto:xuzhou.qin@gmail.com?Subject=CDM%20feedback" target="_top">sending me an email</a>.
           </p>
           </div>
           </div>
           </div>
           </div>
      
           ')
          ),
          fluidRow(
            htmlOutput("consol"),
            tags$script('
                        $(document).on("keypress", function (e) {
                        Shiny.onInputChange("key_track", e.which);
                        });
                        ')
          )

        )
        # fluidPage(
        #   fluidRow(
        #     
        #     box( width = 5,
        #          title = 'Introduction', status = 'success', solidHeader = TRUE,collapsible = TRUE,
        #          p('This is an web application written on R Shiny developed by CSMX. 
        #            The objectif of this application is to facilitate the data mining process of our daily work.')
        #          )
        #   )
        #   # fluidRow(
        #   #          db_connection_check
        #   # )
        # )
        
        
      ),
      ##-------------------------
      ## TAB 1: Cargo IS ----
      
      ### tab1.1 data loader ----
      tabItem(
        tabName = 'cargois',
        valueboxes,
        #### row 1.1.1: input, top airports, top air routes
        fluidRow(             
          cargo.is.input,
          cargo.is.preview
        )
      ),
      ###-----------------------

      ### tab 1.2 Yield analysis ----
      tabItem(
        tabName = 'cargois_plot',
        
        fluidrow.top_apt_inputs ,
        # fluidRow(top.airport),
        #### row 1.3.1: cargo IS treemap ----
        fluidrow.Treemap,  
        
        #### row 1.3.2: cargo IS yield evolution chart/table ----
        fluidrow.Yield_evo
      ),
      
      ### tab 1.3 AWB analysis ----
      tabItem(
        tabName =  'cargois_other',
        fluidRow.other_analysis
      ),

      ### tab 1.4 statistical analysis ----
      tabItem(
        tabName =  'cargois_stat',
        p('#TODO: correlation plot of distance and yield;
          distribution of AWB weight;
          '),
        fluidpage_stat_analysis
        ),

      ### tab 1.5 yield calculator ----
      tabItem(
        tabName = "yield",  # h2("Air routes yield calculator"),
        
        ### row 1.2.1: Cargo IS yield calculator input output ----
        fluidRow(
          box.yield.calc.inp,
          box.yield.calc.out
        ),
        
        ### row 1.2.2: Cargo IS yield calculator Air Routes MAPPING ----
        fluidRow(
          box.yield.calc.map,
          box.yield.calc.map.out
        )  
      ),
      ###------------------------

      ## TAB 2: Seabury ----
      tabItem(
        tabName = 'sea_trade', 
        ### row 2.1: Seabury treemap/evolution chart/table ----
        fluidrow_Seabury 
      ),
      
      ## TAB 3: FR24 ----
      tabItem(
        tabName = 'loader_fr24',
        # p('Developing...'),
        fluidrow_FR24_load
      ),
      tabItem(
        tabName = 'fr24_airline',
        # p('Developing...'),
        fluidrow_FR24_airline
      ),
      tabItem(
        tabName = 'fr24_airport',
        # p('Developing...'),
        fluidrow_FR24_airport
      ),
      tabItem(
        tabName = 'fr24_AC',
        # p('Developing...'),
        fluidrow_FR24_AC
      ),

      
      ## TAB 4: TOOLS ----
      tabItem(
        tabName = 'tool1',

          # p('Develop: user consol')
        bootstrapPage(    # ------------- Code Finder####
                          selectizeInput('level_codefinder','Code finder', choices = choice.codefinder),
                          textInput('name_codefinder','Name'),
                          tableOutput('out_codefinder')
                          # actionButton('Find_code', 'Find code'),
                          
 
          )

      ),
      
      ## Tab 3: Statistical tools
      tabItem(
        tabName = 'analysis'
      ),
      
      ## Tab 4: Map, air routes selection tool
      tabItem(
        tabName = 'Map'
      )
    )
  )
)
#=================================================================
  
  
  
 