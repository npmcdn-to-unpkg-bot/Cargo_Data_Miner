#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# define environment
mainDir = 'C:/Users/QIN_XUZ/Documents/CARGOIS'
dir.create(mainDir, showWarnings = FALSE)
## create folders if not exist
TEMPS = paste(mainDir,'Temporaire',sep = '/');dir.create(TEMPS, showWarnings = FALSE)
OUTPUT = paste(mainDir,'output',sep = '/');dir.create(OUTPUT, showWarnings = FALSE)
FLATFILE = paste(mainDir,'CARGOIS_raw',sep = '/');dir.create(FLATFILE, showWarnings = FALSE)
SCRIPTS = paste(mainDir,'Scripts',sep = '/');dir.create(SCRIPTS, showWarnings = FALSE)
ANALYSIS = paste(mainDir,'Analysis',sep = '/');dir.create(ANALYSIS, showWarnings = FALSE)
DATA = paste(mainDir,'DATA',sep = '/');dir.create(DATA, showWarnings = FALSE)

# load functions
source(file.path(SCRIPTS,'Data_input.R'))
choice.level = list("Region-Region"= 'R2R',
     'Country-Region' ='C2R',
     'Region-Country'='R2C',
     'Country-Country'= 'C2C',
     'Country-World'='C2W',
     'World-Country'='W2C',
     'Airport-Airport' = 'A2A',
     'ALL' = 'ALL')
choice.year = list("2013" = 2013, '2014' = 2014, '2015'=2015, '2016'=2016, 'ALL'=NA)

library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Cargo IS Miner", dropdownMenu(type = "messages",
                                                         notificationItem(
                                                           text = "Cargo IS Miner alpha v0.1",
                                                           icon("heart")
                                                         )
                                            )
  ),
  
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      menuItem("Yield 2015", tabName = "yield", icon = icon("calculator")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      ## first tab content
      tabItem(
        tabName = 'dashboard',
        fluidRow(
          box(
            title = 'Inputs', status = 'warning', solidHeader = TRUE,
            selectizeInput(
              'level', 'Query level', choices = choice.level
            ),
            selectizeInput(
              'year', 'Year', choices = choice.year
            ),
            textInput('org','Origin'),
            textInput('dst','Destination'),
            actionButton('go2','Start'),
            p("Airport: use IATA code, 

              Country: use country code, e.g., China = CN, 

              Region: Europe = EUR, 
                      Africa = AFI, 
                      North Asia = NASIA, 
                      Asia Pacific = ASPAC, 
                      Middle East and North Africa = MENA, 
                      North America = NAM, 
                      Latin America = LATAM")
          ),
          valueBoxOutput('box.count_obs'),
          valueBoxOutput('box.count_apt_pair'),
          valueBoxOutput('box.count_ORG'),
          valueBoxOutput('box.count_DST')
        )
      ),
      # Second tab content
      tabItem(
        tabName = "yield",
        h2("Air routes yield calculator"),
        fluidRow(
          box( 
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
            actionButton('go','Start')
          ),
          
          box(
            title = 'Yields', status = 'primary', solidHeader = TRUE,collapsible = TRUE,
            tableOutput('datatable')),
          box(
            title = 'Map', status = 'primary', solidHeader = TRUE,collapsible = TRUE, width = 12,
            plotOutput('map')
          )
        )
      ),
      tabItem(
        tabName = 'Map')
    )
  )
)
  
  
  
  
 