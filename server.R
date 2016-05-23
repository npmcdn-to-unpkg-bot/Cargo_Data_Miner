#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output) {
  observeEvent(input$go2, {
    output$box.count_obs = renderValueBox({
      data = CargoIS.SQL.query(CargoDB, level = input$level,ORG = input$org,
                               DST = input$dst,year = input$year)
      valueBox(
        length(table(data$DESTINATION_AIRPORT_CODE)), "Airports",
        icon = icon("plane"), color = "purple"
      )
    })
    
    output$box.count_apt_pair = renderValueBox({
      data = CargoIS.SQL.query(CargoDB, level = input$level,ORG = input$org,
                               DST = input$dst,year = input$year)
      valueBox(
        length(table(paste(data$ORIGIN_AIRPORT_CODE, data$DESTINATION_AIRPORT_CODE))), "Air routes", icon = icon("share-alt"),
        color = "blue"
      )
    })
    
    output$box.count_ORG = renderValueBox({
      data = CargoIS.SQL.query(CargoDB, level = input$level,ORG = input$org,
                               DST = input$dst,year = input$year)
      valueBox(
        length(table(data$ORI_COUNTRY)), "Origin countries", icon = icon("sign-out"),
        color = "green"
      )
    })
    
    output$box.count_DST = renderValueBox({
      data = CargoIS.SQL.query(CargoDB, level = input$level,ORG = input$org,
                               DST = input$dst,year = input$year)
      valueBox(
        length(table(data$DST_COUNTRY)), "Destination countries", icon = icon("sign-in"),
        color = "yellow"
      )
    })
  })
  
  
  
  observeEvent(input$go, {
    
    output$datatable = 
      renderTable({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        cargo =CargoIS.SQL.query(CargoDB, 'ALL',  year = input$year2)
        route1 = read.csv(inFile$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
        yld = output_yield(cargo, route1)
        yld[is.na(yld)]=0
        yld
        
      })

    output$map <- renderPlot({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      cargo =CargoIS.SQL.query(CargoDB, 'ALL',  year = input$year2)
      route1 = read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
      yld = output_yield(cargo, route1)
      yld[is.na(yld)]=0

      groutes = data.frame(apt = row.names(yld), cnt = yld$total_weight)
      Plot.air.route(groutes, line_size = 1, point_size = 1)
    }, width = 1050)
    
  })

}
