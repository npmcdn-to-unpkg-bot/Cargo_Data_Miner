#-------------------------------------------------------------
# This is the server logic of Cargo IS Miner. You can run the 
# application by clicking 'Run App' above.
#
# Xuzhou QIN, xuzhou.qin@airbus.com
#-------------------------------------------------------------

library(shinydashboard)

function(input, output) {
  
  # ______CONSOLE________ ####
  output$consol = renderUI({
    show = FALSE
    
    if(is.null(input$key_track)){
      
    } else if (input$key_track == 126) {
      show <- TRUE
    }
    # else if (input$key_track == 64){
    #   show <- FALSE
    # }
    
    if(show){
      HTML(paste('
                 
                 <div class="w3-card-2 w3-margin">
                 <div class="w3-container w3-padding w3-red">
                 <h4 style="font-family: Lucida Console, Monaco, monospace" >
                 Console </h1>
                 </div>
                 
                 <div class="w3-container w3-white ">
                 <p>', paste(capture.output(Sys.info()), collapse = ' '),'</p>
                 </div>
                 </div>      '))
      # box(width = 12, p(capture.output(warnings())))
    } else {
      p('')
    }
    
    
    
  })
######### connection check #########
  
  output$db_connection_check2 <- renderMenu({
    # if(class(CargoDB) == 'try-error'){
    #   statu_cargois <- 'Failed'
    # }else{
    #   statu_cargois <- 'OK'
    # }
    
    # if(class(BIO) == 'RODBC'){
    #   statu_bio <- 'OK'
    # }else{
    #   statu_bio <- 'Failed'
    # }
    
    # if(class(FR24) == 'try-error'){
    #   statu_fr24 <- 'Failed'
    # }else{
    #   statu_fr24 <- 'OK'
    # }
    
    status <- function(db){
      if(db == 'OK'){
        return('success')
      } else {
        return('danger')
      }
    }
    
    if(statu_bio == 'OK' & statu_fr24 == 'OK' & statu_cargois == 'OK'){
      badge_status <- "info"
    } else {
      badge_status <- "danger"
    }
    
    list_msg <- c(statu_cargois,statu_cargois,statu_bio,statu_fr24)
    
    messageData <- data.frame('from' = c('CargoIS','Seabury','BIO','FlightRadar24'),
                              'message' = list_msg,
                              'status' = sapply(list_msg, status))
    
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    
    msgs <- apply(messageData, 1, function(row) {
      notificationItem(icon = shiny::icon('database'), 
                       text =paste(row[['from']],  row[["message"]], sep = ': '),
                       status = row[['status']])
    })
    
    dropdownMenu(type = "notification", .list = msgs, icon = shiny::icon('link'),
                 badgeStatus = badge_status)
  })


  
######### Code Finder ##########  
  output$out_codefinder <- eventReactive(input$Find_code, {

    result <- Code.finder(input$name_codefinder, level = 'airport')
      
    renderTable({result})
  })

  output$out_codefinder <- renderTable({
    if(nchar(input$name_codefinder) == 0){
      result <- NULL
    } else {
      result <- Code.finder(input$name_codefinder, level = input$level_codefinder)
    }
  })

  
######### load data ##########
  
  # Cargo IS query

  DATA2 <- eventReactive(input$go2, {
    if(input$yld_evo){
      dat <- CargoIS.SQL.query(CargoDB, 
                               level = input$level,
                               ORG = input$org, 
                               DST = input$dst)
    } else {
      dat <- CargoIS.SQL.query(CargoDB, 
                               level = input$level,
                               ORG = input$org,
                               DST = input$dst,
                               year = input$year)
    }
    dat
  })
  
  DATA <- eventReactive(input$go2, {
    # if(!input$yld_evo){
    # 
    #   dat <- CargoIS.SQL.query(CargoDB,
    #                            level = input$level,
    #                            ORG = input$org,
    #                            DST = input$dst,
    #                            year = input$year)
    # } else{
    #   if(is.na(input$year)){
    #     dat <- DATA2()
    #   } else {
    #     dat.sauv <- DATA2()
    #     dat <- dat.sauv[dat.sauv$SALES_YEAR == input$year,]
    #   }
    # }
    dat.sauv <- DATA2()
    dat <- dat.sauv[dat.sauv$SALES_YEAR == input$year,]
    dat
  })
  
  
  # AIRPORT LOCATION
  AIRPORT_LOCATION <- reactive({
    get.airport.loc()
  })
  
#######################################################################
################ _________Cargo IS___________  ########################
####################################################################### 
  
######### HTML output #########
  # row 1.1-Cargo IS query input comments
  output$text2 <- renderUI({
    x = paste("Airport: use IATA code,",
      "Country: use country code, e.g., China = CN,",
      "Region: Europe = EUR,",
      "        Africa = AFI,", 
      "        North Asia = NASIA," ,
      "        Asia Pacific = ASPAC," ,
      "        Middle East and North Africa = MENA," ,
      "        North America = NAM,",
      "        Latin America = LATAM", sep = '<br/>')
    HTML(x)
  })
  
  # row 1.4-Seabury query input comments
  output$text.sea <- renderUI({
    x = paste("Country: use country code, e.g., China = CN,",
              " Regions: Europe, Africa,",
              "        Antarctica, Asia Pacific," ,
              "        M. East & South Asia," ,
              "        North America, Latin America,",
              "        Categories & Errors", sep = '<br/>')
    HTML(x)
  })
  
  # Render UI: cargo IS years: ####
  output$year_cargois_ui <- renderUI({
    # choice_cargois_year <- list(as.character(c(2010:max(choice_cargois_year$YEAR_NUM))) = c(2010:max(choice_cargois_year$YEAR_NUM)), 'ALL'=NA)
    selectizeInput('year', 'Select year', choices = choice.year)
  })
  
######### Data Preview cargo IS #########
  output$table_preview <- renderDataTable({
    head(DATA(),50)
  })
  
  ##  downloadable datatable
  output$dl_cargo_is <- downloadHandler(
    #- This function returns a string which tells the client
    #- browser what name to use when saving the file.
    filename <- function() {
      paste('CargoIS_', paste(input$org, input$dst, input$year, sep = '_'),'.csv', sep = "")
    },
    #- This function should write data to a file given to it by
    #- the argument 'file'.
    content <- function(file) {
      ### Write to a file specified by the 'file' argument
      write.table(data.frame(DATA()), file, sep = ',',
                  row.names = FALSE, col.names = TRUE)
    }
  )
  
######### Top Airport Air routes #########
  ## top airport
  output$top.airport <- renderPlotly({
    
    p <- Plot.airport.html(DATA(),  projection = 'Mercator', 
                      top = input$nb.top, 
                      plot.airport = TRUE, 
                      pt.size = 1000000/(as.numeric(input$pt.size.top.apt)))
    p
  })
  ## output the datatable for the top airports
  output$topairport.table <- DT::renderDataTable(
    
    Plot.airport.html(DATA(),  projection = 'Mercator', 
                      top = input$nb.top, 
                      plot.airport = TRUE, 
                      pt.size = 1000000/(as.numeric(input$pt.size.top.apt)), # point size = input slider
                      table = TRUE),
    options = list(pageLength = 10)
  )
  ## top air routes
  output$top.air.route <- renderPlotly({

    p2 <- Plot.airport.html(DATA(),  projection = 'Mercator', 
                            top = input$nb.top, 
                            plot.airport = FALSE
    )
    p2
  })
  
  ## output the datatable for the top air routes
  output$topairroute.table <- DT::renderDataTable(
    Plot.airport.html(DATA(),  projection = 'Mercator', 
                      top = input$nb.top, 
                      plot.airport = FALSE,
                      table = TRUE),
    options = list(pageLength = 10)
  )


######### value boxes ##########
  ## value box 
  output$box.count_obs = renderValueBox({
    valueBox(
      length(table(DATA()$DESTINATION_AIRPORT_CODE)), "Airports",
      icon = icon("plane"), color = "purple"
    )
  })
  ## value box
  output$box.count_apt_pair = renderValueBox({
      valueBox(
        length(table(paste(DATA()$ORIGIN_AIRPORT_CODE, DATA()$DESTINATION_AIRPORT_CODE))), "Air routes", icon = icon("share-alt"),
        color = "blue"
      )
    })
  ## value box
  output$box.count_ORG = renderValueBox({
      valueBox(
        length(table(DATA()$ORI_COUNTRY)), "Origin countries", icon = icon("sign-out"),
        color = "green"
      )
    })
  ## value box
  output$box.count_DST = renderValueBox({
      valueBox(
        length(table(DATA()$DST_COUNTRY)), "Destination countries", icon = icon("sign-in"),
        color = "yellow"
      )
    })
  ## value box
  output$box.avg_weight = renderValueBox({
    dt = DATA()
    dt[is.na(dt)] = 0
    avg = sum(dt[,'WEIGHT_CURRENT_YEAR'])/length(table(paste(dt[,'SALES_YEAR'], dt[,'SALES_MONTH'], sep= '')))
    
    valueBox(
      paste(round(avg/1000, digits = 0),'t'), "AVG monthly Weight", icon = icon("cube"),
      color = "fuchsia"
    )
  })
  ## value box
  output$box.ave_yield = renderValueBox({
    dt = DATA()
    dt[is.na(dt)] = 0
    avg_grs = (sum(dt[,'WEIGHT_CHARGES_CURR_YEAR_USD'])+sum(dt[,'OTHER_CHARGES_CURR_YEAR_USD']))/sum(dt[,'WEIGHT_CURRENT_YEAR'])
    avg_net = (sum(dt[,'WEIGHT_CHARGES_CURR_YEAR_USD']))/sum(dt[,'WEIGHT_CURRENT_YEAR'])
    
    valueBox(
      paste(round(avg_grs, digits = 2),round(avg_net, digits = 2), sep = ' / '), "AVG grs/net yield", icon = icon("dollar"),
      color = "teal"
    )
  })
  
######### Cargo IS origin/destination treemaps   #########  
  

  output$tree.org_cty_gvis <- renderGvis({
    df <- DATA()
    df <- df[complete.cases(df),]
    output$tree.dst_cty_gvis <- renderGvis({
      treemap_cargois_gvis(df, ORG = FALSE)
    })
    
    output$tree.org_cty_dtable <- renderDataTable({
      treemap_cargois_gvis(df, ORG = TRUE, PLOT = FALSE)
    })
    output$tree.dst_cty_dtable <- renderDataTable({
      treemap_cargois_gvis(df, ORG = FALSE, PLOT = FALSE)
    })
    treemap_cargois_gvis(df, ORG = TRUE)
  })
  

######### Evo Yield(with gvis) ######### 
  # gvis output evo plot
  output$yld_evo_cargois_gvis <- renderGvis({
    if(input$yld_evo){
      data2 = DATA2()
      year.range <- sort(choice.year, decreasing = FALSE)
    } else {
      data2 <- DATA()
      year.range <- input$year
    }
    data2[is.na(data2)]=0
    
    total_evolution = data.frame(Month = NULL, x= NULL)
    
    for (i in year.range){
      evo = gross_yield_evolution(data2,i)
      evo2 <- net_yield_evolution(data2,i)
      weight <- aggregate(x =list(weight = data2[data2$SALES_YEAR==i,]$WEIGHT_CURRENT_YEAR), 
                          by = list(Months = data2[data2$SALES_YEAR==i,]$SALES_MONTH), 
                          FUN = sum)
      
      Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
      
      df <- data.frame('Months' = Months, 'Gross yield' = evo$evo_gross_yield, 
                       'Net yield' = evo2$evo_net_yield, 'Weight'=weight$weight)
      total_evolution = rbind(total_evolution, df)
    }
    avg <- sum(data2$WEIGHT_CHARGES_CURR_YEAR_USD+data2$OTHER_CHARGES_CURR_YEAR_USD)/sum(data2$WEIGHT_CURRENT_YEAR)
    avg_ny <- sum(data2$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(data2$WEIGHT_CURRENT_YEAR)
    
    max <- max(total_evolution$Gross.yield)
    min <- min(total_evolution$Gross.yield)
    
    max_ny <- max(total_evolution$Net.yield)
    min_ny <- min(total_evolution$Net.yield)
    
    plot.title <- paste('Yield from',input$org, 'to', input$dst)
    plot.subtitle <- paste( 'GROSS YIELD Min: ', round(min,2), '; Avg: ', round(avg,2), '; Max: ', round(max,2),
                            '\nNET YIELD Min: ',round(min_ny,2), '; Avg: ', round(avg_ny,2), '; Max: ', round(max_ny,2),sep = '')
    
          # output the table
          output$evo_plot_dtable <-
            renderDataTable({
              total_evolution
            })
    
    gvisLineChart(total_evolution, "Months", c("Gross.yield",
                                        "Net.yield",                                           
                                        "Weight"),
                  options=list(height=600,legend = "bottom",
                               # chart="[
                               # title:'title'
                               # ]",
                               title=paste(plot.title,'\n', plot.subtitle,sep = ''),
                               # subtitle=plot.subtitle,
                               series="[
                               {color:'#db3236',targetAxisIndex: 0,lineWidth: 3},
                               {color: '#f4c20d',targetAxisIndex: 0,lineWidth: 3},
                               {color: '#4885ed',targetAxisIndex:1,lineDashStyle: [2,2,20,2,20,2],lineWidth: 3}
                               
                               ]",
                               vAxes="[{title:'Yield (USD/kg)'}, {title:'Weight (kg)'}]"
                  ))
  })
  
  
  ######### yield calculator #########
  # define reactive envir
  INFILE <- eventReactive(input$go, {input$file1})
  CARGO <- eventReactive(input$go, {CargoIS.SQL.query(CargoDB, 'ALL',  year = input$year2)})
  ROUTE1 <- eventReactive(input$go, {read.csv(INFILE()$datapath, 
                                              header = input$header,
                                              sep = input$sep, quote = input$quote)})
  
  # output table
  output$datatable = DT::renderDataTable({
    if (is.null(INFILE()))
      return(NULL)
    data <- output_yield(CARGO(), ROUTE1())
    data[is.na(data)] <- 0
    out.data <- data.frame(data) #routes = row.names(data), 
    
    ##  downloadable datatable
    output$downloadData <- downloadHandler(
      #- This function returns a string which tells the client
      #- browser what name to use when saving the file.
      filename <- function() {
        paste('Yield_', substr(Sys.time(),1,10),'.csv', sep = "")
      },
      #- This function should write data to a file given to it by
      #- the argument 'file'.
      content <- function(file) {
        ### Write to a file specified by the 'file' argument
        write.table(out.data, file, sep = ',',
                    row.names = F, col.names = TRUE)
      }
    )
    
    # output plot
    output$map <- renderPlotly({
      if (is.null(INFILE()))
        return(NULL)
      data[is.na(data)] <- 0
      groutes <- data.frame(apt = row.names(data), cnt = data[,'total_weight'],
                            # lsize = 5*(data[,input$compare]/max(data[,input$compare]))^1.5
                            lsize = (data[,input$compare]/max(data[,input$compare])) ## should be line colour
      )
      Plot.air.route.html(groutes, pt.color = input$pt.color, ln.color = input$color , projection = input$projection,  point_size = input$pt.size)
    })
    DT::datatable(out.data,options = list(pageLength = 10))
  })
  
  ######### freq_shipment_evo ########
  output$select_wb <- renderUI({
    WeightBreak <- c('[0-45[',
                     '[45-100[',
                     '[100-300[',
                     '[300-500[',
                     '[500-1000[',
                     '[1000+') #define weight breaks
    
    WeightBreak <- subset(WeightBreak, WeightBreak %in% unique(DATA()$WEIGHT_BREAK))
    
    checkboxGroupInput('check_display_wb','Select displayed AWB weight break',WeightBreak,
                       selected = WeightBreak)
  })
  
  # trigger 
  observe({
    if(!is.null(input$check_display_wb)){
      output$freq_shipment_evo <- renderPlot({
        
        data <- data.frame(DATA(), stringsAsFactors = FALSE)
        evo_wb <- ShipmentEvo(data) # call the ShipmentEvo function
        
        # WeightBreak <- c('[0-45[',
        #                  '[45-100[',
        #                  '[100-300[',
        #                  '[300-500[',
        #                  '[500-1000[',
        #                  '[1000+') #define weight breaks
        # 
        # WeightBreak <- subset(WeightBreak, WeightBreak %in% input$check_display_wb)
        WeightBreak <- input$check_display_wb
        
        #title of plot
        plot.title <- paste('Shipment frequency of different size of cargo')
        plot.subtitle <- paste( 'From: ', input$org, ' To: ', input$dst ,sep = '')
        
        ######FINAL DATA TABLE: input_table######
        input_table <- subset(evo_wb, evo_wb$weight_break %in% WeightBreak)
        
        #### make plot
        p <- ggplot(data= input_table,
                    aes(x=Months, y=x)) +
          geom_line(aes(colour=weight_break, group=weight_break), cex = 0.8)+
          ylab('frequency') + 
          xlab('Date') +
          ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
          
          scale_colour_discrete(name="Air waybill\nWeight", breaks=WeightBreak)+
          # labels=c("0-45kg", "45-100kg", "100-300kg",'300-500kg',
          #          '500-1000kg','1000+kg')
          theme_bw()
        
        # DOWNLOAD PLOT
        output$cargois_dl_freq_shipment_evo <- downloadHandler(
          filename = function() { paste('cargo weigth break freq ',plot.subtitle, '.png', sep='') },
          content = function(file) {
            ggsave(file, p, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        #__________________________________
        ######### distribution of AWB weight ########
        output$dist_weight_awb <- renderPlot({
          # data2 <- subset(data, data$WEIGHT_BREAK %in% input$check_display_wb)
          
          dist <- aggregate(x=list('frequency' = data$AWB_COUNT_CURRENT_YEAR), by = list(weight_break = data$WEIGHT_BREAK),
                            FUN = sum)
          
          dist <- subset(dist, dist$weight_break %in% input$check_display_wb)
          
          # p2 <- ggplot(data=dist, aes(x = weight_break, y = x)) +
          #   geom_bar(stat = 'identity', position = "dodge", fill="steelblue")+
          #   scale_x_discrete(limits = c('[0-45[','[45-100[','[100-300[', '[300-500[', '[500-1000[', '[1000+'))+
          #   ggtitle(bquote(atop(.('Distribution of the Air waybill weight'), atop(italic(.(plot.subtitle)), "")))) +
          #   ylab('frequency') + 
          #   xlab('Air waybill weight') +
          #   theme_bw()
          # g1 <- gvisBarChart(dist, xvar="weight_break", yvar="frequency",
          #              options = list(legend = 'none',
          #                             title='Distribution of the Air waybill weight',
          #                             hAxes = "[{title: 'Frequency', titleTextStyle: {color: '#000000'}}]",
          #                             vAxes = "[{title: 'Weight breaks', titleTextStyle: {color: '#000000'}}]"))
          # 
          # g1
          
          
          #### plot
          p_bar <- ggplot(data=dist, aes(x=weight_break, y=frequency))+
              geom_bar(stat = 'identity', position = "dodge",fill="steelblue")+
              geom_text(aes(label=frequency), vjust=1.6, color="white", size=3.5)+
              scale_x_discrete(limits = WeightBreak)+
              theme(legend.position="right", legend.box = "horizontal" ) +
              labs(title= 'Distribution of AWB', x = 'Weight Break', y = 'Frequency')+
              theme_bw()
          
          # DOWNLOAD PLOT P_BAR: distribution of AWB
          output$cargois_dl_dist_weight_awb <- downloadHandler(
            filename = function() { paste('cargo AWB distribution ',plot.subtitle, '.png', sep='') },
            content = function(file) {
              ggsave(file, p_bar, width = as.numeric(input$cargois_width), 
                     height=as.numeric(input$cargois_height), unit = 'cm')
            }
          )
          p_bar
        })
        p
      })
      
      ######## VERSUS DATA TABLE ########
      versus_datatable_cargois <- reactive({
        df <- DATA()
        df <- df[df$WEIGHT_CURRENT_YEAR != 0,]
        df <- df[df$GREATCIRCLEDISTANCE_NM != 0,]
        cutt <- cut(df$GREATCIRCLEDISTANCE_NM, c(seq(1,10000,by = 500)-1, Inf), 
                    dig.lab=10) # prevent scientific notation
        df$lebal <- cutt
        
        # test <- aggregate(x=list(weight=df$WEIGHT_CURRENT_YEAR), by = list(dist = df$lebal, wb = df$WEIGHT_BREAK), FUN = sum)
        # p_weight_distance <- qplot(dist, data = test, weight = weight, geom = 'bar',
        #                            xlab = 'Distance (nm)',ylab = 'Weight (kg)', fill = wb,
        #                            main = 'Weight vs Distance')
        test <- aggregate(x=list(weight=df$WEIGHT_CURRENT_YEAR, freq = df$AWB_COUNT_CURRENT_YEAR, 
                                 wchg = df$WEIGHT_CHARGES_CURR_YEAR_USD, surchg = df$OTHER_CHARGES_CURR_YEAR_USD),
                          by = list(dist = df$lebal, wb = df$WEIGHT_BREAK), FUN = sum)
        
        
        test$gy <- (test$wchg+test$surchg)/test$weight
        test$ny <- test$wchg/test$weight
        
        test
      })
      
      versus_data_cargois <- reactive({
        subset(versus_datatable_cargois(),
                       versus_datatable_cargois()$wb %in% input$check_display_wb)
        
      })
      ######## Weight vs distance ########
      output$cargois_corr_plot <- renderPlot({
        p_weight_distance <- ggplot(data = versus_data_cargois(), aes(x = dist, y = weight, fill = wb))+
          geom_bar(stat = 'identity') +
          ylab('Weight (kg)') + 
          xlab('Distance (nm)')+
          ggtitle('Weight vs Distance')+
          scale_fill_discrete(breaks = input$check_display_wb, name="Weight\nbreaks") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
        # DOWNLOAD PLOT
        output$cargois_dl_weight_distance <- downloadHandler(
          filename = function() { paste('cargo weight vs distance ',paste( 'From: ', input$org, ' To: ', input$dst ,sep = ''), '.png', sep='') },
          content = function(file) {
            ggsave(file, p_weight_distance, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        p_weight_distance
      })
      
      # freq vs distance  ####
      output$cargois_freq_distance <- renderPlot({
        p_freq_distance <- ggplot(data = versus_data_cargois(), aes(x = dist, y = freq, fill = wb))+
          geom_bar(stat = 'identity') +
          ylab('Numbers of AWB') + 
          xlab('Distance (nm)')+
          ggtitle('Airway bill frequency vs Distance')+
          scale_fill_discrete(breaks = input$check_display_wb, name="Weight\nbreaks") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
        
        
        
        # DOWNLOAD PLOT
        output$cargois_dl_freq_distance <- downloadHandler(
          filename = function() { paste('cargo frequency vs distance ',paste( 'From: ', input$org, ' To: ', input$dst ,sep = ''), '.png', sep='') },
          content = function(file) {
            ggsave(file, p_freq_distance, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        p_freq_distance
      })
      
      
      # gross yield vs distance  ####
      output$cargois_gy_distance <- renderPlot({
        p_gy_distance <- ggplot(data = versus_data_cargois(), aes(x = dist, 
                                                                  y = eval(parse(text=input$cargois_versus_plot_gyny))
                                                                ))+
          geom_line(stat = 'identity', aes(colour = wb, group = wb), cex = 0.8) +
          ylab('Yield (USD/kg)') + 
          xlab('Distance (nm)')+
          ggtitle('Yield vs Distance')+
          scale_colour_discrete(name="Air waybill\nWeight", breaks=input$check_display_wb)+
          # scale_fill_discrete(breaks = input$check_display_wb, name="Weight\nbreaks") #+
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
        
        
        # DOWNLOAD PLOT
        output$cargois_dl_gy_distance <- downloadHandler(
          filename = function() { paste('cargo yield vs distance ',paste( 'From: ', input$org, ' To: ', input$dst ,sep = ''), '.png', sep='') },
          content = function(file) {
            ggsave(file, p_gy_distance, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        p_gy_distance
      })
      
      # Revenue vs distance  ####
      output$cargois_revenu_distance <- renderPlot({
        p_revenu_distance <- ggplot(data = versus_data_cargois(), aes(x = dist, 
                                                                      y = wchg+surchg, 
                                                                      fill = wb))+
                              geom_bar(stat = 'identity') +
                              ylab('Revenue (USD)') + 
                              xlab('Distance (nm)')+
                              ggtitle('Revenue vs Distance')+
                              scale_fill_discrete(name="Air waybill\nWeight", 
                                                    breaks=input$check_display_wb)+
                              theme(axis.text.x = element_text(angle = 30, hjust = 1))
        
        # DOWNLOAD PLOT
        output$cargois_dl_revenu_distance <- downloadHandler(
          filename = function() { paste('cargo revenue vs distance ',paste( 'From: ', input$org, ' To: ', input$dst ,sep = ''), '.png', sep='') },
          content = function(file) {
            ggsave(file, p_revenu_distance, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        p_revenu_distance
      })
      
      # AVG weight vs distance  ####
      output$cargois_avgweight_distance <- renderPlot({
        
        data <- aggregate(x = list(weight = versus_data_cargois()$weight, 
                                   freq = versus_data_cargois()$freq), 
                          by = list(dist = versus_data_cargois()$dist), FUN = sum)
        data$avg <- data$weight/data$freq
        p_avgweight_distance <- ggplot(data = data, aes(x = dist, y = avg, group = 1))+
          geom_line() +
          ylab('Average weight per airway bill (kg)') + 
          xlab('Distance (nm)')+
          ggtitle('Average weight vs Distance')+
          # scale_colour_discrete(name="Air waybill\nWeight", breaks=input$check_display_wb)+
          # scale_fill_discrete(breaks = input$check_display_wb, name="Weight\nbreaks") #+
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
        
        
        # DOWNLOAD PLOT
        output$cargois_dl_avgweight_distance <- downloadHandler(
          filename = function() { paste('cargo average weight vs distance ',paste( 'From: ', input$org, ' To: ', input$dst ,sep = ''), '.png', sep='') },
          content = function(file) {
            ggsave(file, p_avgweight_distance, width = as.numeric(input$cargois_width), 
                   height=as.numeric(input$cargois_height), unit = 'cm')
          }
        )
        p_avgweight_distance
      })
    } # end if
    
  })
  

  
  # Statistical analysis ####
  # 1. get ESAD ####
  # can not use the folloing methode, tooooo slow

# ______________________________________________
#   ESAD_cargois <- eventReactive(DATA(), {
#     df <- data.frame(DATA(),stringsAsFactors = FALSE)
#     df <- df[complete.cases(df),]
#     # get the unique ORG DST pairs
#     orgdst <- unique(paste(df$ORIGIN_AIRPORT_CODE, df$DESTINATION_AIRPORT_CODE,
#                            sep = ""))
#     
#     # make the query 
#     body = "SELECT from_.IATA AS ORG, 
# to_.IATA AS DST, 
# apt.DISTANCE, 
# apt.ESAD 
# FROM BIO_NEO_PROD.bio.AIRPORT_PAIR apt 
# INNER JOIN BIO_NEO_PROD.bio.AIRPORT from_ ON apt.ORG_AIRPORT_ID = from_.AIRPORT_ID 
# INNER JOIN BIO_NEO_PROD.bio.AIRPORT to_ ON apt.DST_AIRPORT_ID = to_.AIRPORT_ID "
#     
#     ## generate the part 'WHERE BLABLABLA'
#     query <- paste(orgdst, collapse = "' OR from_.IATA+ to_.IATA = '")
#     query2 <- paste("WHERE (from_.IATA+ to_.IATA = '", query, "')", sep = "")
#     
#     ## Final query
#     query_final <- paste(body, query2, sep = "")
#     query_final
#     # send query to BIO
#     # sqlQuery(BIO, query_final)
#   })
#   
#   # output$test3 <- renderTable({
#   #   ESAD_cargois()
#   # })
#   
#   
#   output$text3 <- renderText({
#     ESAD_cargois()
#   })
# ______________________________________________
  

  output$cargois_ui_x_axis <- renderUI({
    sel <- names(DATA())
    selectInput('cargois_x_axis', 'Select the x axis', choices = sel)
  })
  
  output$cargois_ui_y_axis <- renderUI({
    sel <- names(DATA())
    selectInput('cargois_y_axis', 'Select the y axis', choices = sel)
  })
  

  
  #===============================================
  # ____________Seabury________ ==================
  #===============================================  
  # RENDER UI/ SELECT YEARS ####

  output$year.sea.ui <- renderUI({
    # query <- "SELECT CARGO_YEAR
    #           FROM A269_CARGO.A269_CARGO_AVAILABLE_YEAR
    #               WHERE DATA_ORIGIN = 'Seabury' "
    # data  <-  dbSendQuery(CargoDB, query)
    # choice_sea_year <- fetch(data)
    
    selectizeInput('year.sea', 'Select year', choices = choice_year_sea)
  })
  
  ######### Seabury treemaps ########

  # Seabury query
  DATA.SEA = eventReactive(input$go.sea, {
    # withProgress(message = 'Loading data...', value = 0, {
    #   # Number of times we'll go through the loop
    #   n <- 4
    #   for (i in 1:n) {
    #     # Each time through the loop, add another row of data. This is
    #     # a stand-in for a long-running computation.
    #     # Increment the progress bar, and update the detail text.
    #     incProgress(1/n, detail = paste( i*25, '%',sep = ''))
    #   }
    # })
    dat <- Seabury.SQL.OD.query(CargoDB, 
                                level = input$level.sea, 
                                ORG = input$org.sea,
                                DST = input$dst.sea, 
                                year = input$year.sea)      #TODO: create a proper YEAR value for SB
    
    dat
  })
  
  output$tree.g1 <- renderGvis({
    data <- data.frame(DATA.SEA())
    if(input$sfc.air == 'AIRWEIGHT_AIRVALUE'){
      dt <- treemap_dt_sea_gvis(data, AIR=TRUE)
      
    } else if (input$sfc.air == 'SFCWEIGHT_SFCVALUE'){
      dt <- treemap_dt_sea_gvis(data, AIR=FALSE)
      
    }
    
    # output the datatable
    output$sea_treemap_table <- renderDataTable({
      dt[,-6] # do not display {logVALUEPERKILO}
    })
    
    # download data table
    output$dl_sea_treemap <- downloadHandler(
      #- This function returns a string which tells the client
      #- browser what name to use when saving the file.
      filename <- function() {
        paste('Seabury_commodities', paste(input$org.sea, input$dst.sea, input$year.sea, sep = '_'),'.csv', sep = "")
      },
      #- This function should write data to a file given to it by
      #- the argument 'file'.
      content <- function(file) {
        ### Write to a file specified by the 'file' argument
        write.table(dt[,-6], file, sep = ',',
                    row.names = FALSE, col.names = TRUE)
      }
    )
    
    treemap_sea_gvis(dt, ORG=input$org.sea, DST= input$dst.sea, YEAR=input$year.sea)
  })

  
  output$gxcode.finder <-  renderTable({
    gx.code <- Seabury.SQL.get.code(CargoDB, input$gxname)
  })
  # data.sea.gx
  
######### load data for plotting evolution ##########
  
  ## get data for selected commodities
  DATA.SEA.GX <- eventReactive(input$gx.evo.plot, {
    n <- 1
    if(input$gx.evo.plot.all){
      # withProgress(message = 'Loading data...', value = 0, {
      #   # Number of times we'll go through the loop
      #  
      #   for (i in 1:n) {
      #     # Each time through the loop, add another row of data. This is
      #     # a stand-in for a long-running computation.
      # 
      #     
      #     # Increment the progress bar, and update the detail text.
      #     incProgress(1/n, detail = paste( i*10, '%',sep = ''))
      #   }
      # })
      qry <-  Seabury.SQL.evo.query(CargoDB,  
                                    level = input$level.sea, 
                                    ORG = input$org.sea, 
                                    DST = input$dst.sea,
                                    GX.code = NA,
                                    year = NA, 
                                    country.name = FALSE, 
                                    SFC = input$surface.air)
      return(qry)
    }else{
      gx.code <- Seabury.SQL.get.code(CargoDB, input$gxname)
      # withProgress(message = 'Loading data...', value = 0, {
      #   # Number of times we'll go through the loop
      #   for (i in 1:n) {
      #     # Each time through the loop, add another row of data. This is
      #     # a stand-in for a long-running computation.
      #     qry <- Seabury.SQL.evo.query(CargoDB,  
      #                                  level = input$level.sea, 
      #                                  ORG = input$org.sea, 
      #                                  DST = input$dst.sea,
      #                                  GX.code = gx.code$GXCODE,
      #                                  year = NA, 
      #                                  country.name = FALSE, 
      #                                  SFC = input$surface.air)
      #     
      #     # Increment the progress bar, and update the detail text.
      #     incProgress(1/n, detail = paste( i*10, '%',sep = ''))
      #   }
      # })
      qry <- Seabury.SQL.evo.query(CargoDB,  
                                   level = input$level.sea, 
                                   ORG = input$org.sea, 
                                   DST = input$dst.sea,
                                   GX.code = gx.code$GXCODE,
                                   year = NA, 
                                   country.name = FALSE, 
                                   SFC = input$surface.air)
      
      return(qry)
    }
  })
  
  # data.sea.gx = eventReactive(input$gx.evo.plot.all, {
  #   gx.code = Seabury.SQL.get.code(CargoDB, input$gxname)
  #   Seabury.SQL.evo.query(CargoDB,  
  #                         level = input$level.sea, ORG = input$org.sea, DST = input$dst.sea,
  #                         GX.code = NA)
  # })
  
######### Seabury evolution plot #########
  output$sea.evo1 <- renderPlotly({
    
    data <- sea.evo(DATA.SEA.GX(), GX.level = 'G1', 
                    weight = input$weight.value, 
                    SFC = input$surface.air )
    df.gxname <-  Seabury.SQL.get.name(CargoDB, names(data)[-1])
    nam = names(data)[2]
    p <- plot_ly(data, x = Year, y = eval(parse(text = nam)), name = df.gxname$GXNAME[1])
    for(namcol in names(data)[c(-1,-2)]){
      p <- add_trace(data, x = Year, y = eval(parse(text = namcol)), 
                     name = df.gxname$GXNAME[df.gxname$GXCODE == namcol], evaluate = TRUE)

    }
    p %>% layout(showlegend = TRUE, yaxis = list(title = ifelse(input$weight.value,yes = 'Weight',no = 'Value')))
  })
  
  output$sea.evo2 <- renderPlotly({
    
    data <- sea.evo(DATA.SEA.GX(), GX.level = 'G2', 
                    weight = input$weight.value, 
                    SFC = input$surface.air)
    df.gxname <-  Seabury.SQL.get.name(CargoDB, names(data)[-1])
    nam = names(data)[2]
    p <- plot_ly(data, x = Year, y = eval(parse(text = nam)), name = df.gxname$GXNAME[1])
    for(namcol in names(data)[c(-1,-2)]){
      p <- add_trace(data, x = Year, y = eval(parse(text = namcol)), 
                     name = df.gxname$GXNAME[df.gxname$GXCODE == namcol], evaluate = TRUE)
      
    }
    p %>% layout(showlegend = TRUE, yaxis = list(title = ifelse(input$weight.value,yes = 'Weight',no = 'Value')))
  })
  
  output$sea.evo3 <- renderPlotly({
    
    data <- sea.evo(DATA.SEA.GX(), GX.level = 'G3', 
                    weight = input$weight.value, 
                    SFC = input$surface.air)
    df.gxname <-  Seabury.SQL.get.name(CargoDB, names(data)[-1])
    nam = names(data)[2]
    p <- plot_ly(data, x = Year, y = eval(parse(text = nam)), name = df.gxname$GXNAME[1])
    for(namcol in names(data)[c(-1,-2)]){
      p <- add_trace(data, x = Year, y = eval(parse(text = namcol)), 
                     name = df.gxname$GXNAME[df.gxname$GXCODE == namcol], evaluate = TRUE)
      
    }
    p %>% layout(showlegend = TRUE, yaxis = list(title = ifelse(input$weight.value,yes = 'Weight',no = 'Value')))
  })
  
  output$sea.evo4 <- renderPlotly({
    
    data <- sea.evo(DATA.SEA.GX(), GX.level = 'G4', 
                    weight = input$weight.value, 
                    SFC = input$surface.air)
    df.gxname <-  Seabury.SQL.get.name(CargoDB, names(data)[-1])
    nam = names(data)[2]
    p <- plot_ly(data, x = Year, y = eval(parse(text = nam)), name = df.gxname$GXNAME[1])
    for(namcol in names(data)[c(-1,-2)]){
      p <- add_trace(data, x = Year, y = eval(parse(text = namcol)), 
                     name = df.gxname$GXNAME[df.gxname$GXCODE == namcol], evaluate = TRUE)
      
    }
    p %>% layout(showlegend = TRUE, yaxis = list(title = ifelse(input$weight.value,yes = 'Weight',no = 'Value')))
  })



  #===============================================
  # __________FlightRadar24_______  =============
  #=============================================== 
  
  #--------------------------------
  # idea: 
  # 1. generate query by using boolean (with different parameters)
  # 2. send query, to FR24 database with different input
  # 3. return datatable {FR24_query()}
  # 4. renderUI: return the reg_number of FR24 datatable
  # 5. query BIO with the returned reg_number, get the return 
  # 6. join the two datatables, generate {FR24_BIO_AC()}
  #---------------------------------
  # ____________________PAGE 1 LOAD DATA  ___________________ ####
  # 1.FR24_query: Generate the sql query ####
  FR24_query <- #eventReactive(input$go_fr24, {
    reactive({
    fr24_input_daterange <- paste(as.character(input$fr24_daterange), collapse = "' AND '")
    
    Body <- "
######################################
# DO NOT CHANGE THE FOLLOWING QUERY 
#  IF YOU ARE NOT FAMILIAR WITH MySQL 
######################################
SELECT DISTINCT
  FLIGHT_NUMBER, AIRLINE, ORG, DST, AC_TYPE, REG_NUMBER, TAKE_OFF_TIME, LANDING_TIME

FROM  FLIGHT_DETAIL_VIEW fdv
WHERE COMPLETION_STATUS=0\n"

    select_date_range <- paste("AND substr(TAKE_OFF_TIME,1,10) BETWEEN '",fr24_input_daterange,"'\n",sep = '')
    
    # generate airline code query
    if(input$fr24_airline.code != ''){
      select_airline <- paste("AND AIRLINE = '", input$fr24_airline.code,"'\n", sep = '')
    } else {
      select_airline <- ''
    }
    
    # generate ORG DST query
    # if both of them are not NA: 
    # |--> if the ORG = DST: ORG = CDG OR DST = CDG
    # |--> if they are different: ORG = CDG AND DST = MEM
    # if both of them are NA:
    # |--> query = NULL
    # if one of them is NA:
    # |--> if ORG = NA: 
    # |--> if DST = NA:
    if(input$fr24_select_org != '' & input$fr24_select_dst != ''){
      
      if(input$fr24_select_org == input$fr24_select_dst){
        select_org_dst <- paste("AND (ORG = '", input$fr24_select_org, "' OR DST = '",
                                input$fr24_select_dst, "')\n",
                                sep = '')
      } else {
        select_org_dst <- paste("AND ORG = '", input$fr24_select_org, "'\nAND DST = '", input$fr24_select_dst, "'\n", 
                                sep = '')
      }

    } else if (input$fr24_select_org == '' & input$fr24_select_dst == ''){
      select_org_dst <- ''
      
    } else if (input$fr24_select_org != '' | input$fr24_select_dst != ''){
      # select_org_dst <- paste("AND (ORG = '", input$fr24_select_org,input$fr24_select_dst, "' OR DST = '", 
      #                         input$fr24_select_org,input$fr24_select_dst, "')\n", 
      #                         sep = '')
      if(input$fr24_select_org == ''){
        select_org_dst <- paste("AND DST = '", input$fr24_select_dst, "'\n", 
                                sep = '')
      } else {
        select_org_dst <- paste("AND ORG = '", input$fr24_select_org, "'\n", 
                                sep = '')
      }
    } 
    
    # generate REG_number query
    if(input$fr24_select_reg_number != ''){
      
      tails <- strsplit(input$fr24_select_reg_number, ';')[[1]] # split string by ';'
      
      tails <- gsub(' ', '', tails)
      
      tails_ <- paste(tails, collapse = "' OR REG_NUMBER = '") # add [OR REG_NUMBER = ]
        
      select_reg_number <- paste("AND (REG_NUMBER = '", tails_, "')\n", sep = '') 
                                                                  # Finish the query string
    } else {
      select_reg_number <- ''
    }
    
    # generate AC_TYPE query
    if(input$fr24_select_ac_type != ''){
      ac <- strsplit(input$fr24_select_ac_type, ';')[[1]] # split string by ';'
      ac <- gsub(' ', '', ac)
      ac_ <- paste(ac, collapse = "' OR AC_TYPE = '") # add [OR AC_TYPE = ]
      
      select_ac_type <- paste("AND (AC_TYPE = '", ac_, "')\n", sep = '') 
      # Finish the query string
    } else {
      select_ac_type <- ''
    }

    paste(Body,select_date_range,select_airline,select_org_dst,select_reg_number,select_ac_type,sep = '')
  })
  
  # 2. output$textaera_fr24: Render the {sql query} to UI ####
  output$textaera_fr24 <- renderUI({
    x <- FR24_query()
    tags$textarea(id="foo", rows=20, style='width:100%', x)
  })

  # 3. FR24_datatable: query the database and render dataTable ####
  FR24_datatable <- eventReactive(input$go2_fr24, {
    # fr24_input_daterange <-paste(as.character(input$fr24_daterange), collapse = ' AND ')      # fr24_input_daterange
    # fr24_input_daterange <- paste(as.character(input$fr24_daterange), collapse = ' AND ')
    # select_date_range <- paste("AND substr(TAKE_OFF_TIME,1,10) BETWEEN ",fr24_input_daterange,sep = '')
    # select_date_range
    query <- input$foo
    FR24.query(query)
    
  })
  
  # 4. DATA CLEANER ####
  ## 4.1 renderUI: reg number, flight number ####
  
  ### fr24ui_select_reg_number2 ####
  output$fr24ui_select_reg_number2 <- renderUI({
    # reg_number <- c(sort(unique(FR24_datatable()$REG_NUMBER)))
    
    # dt <- data.table(reg= FR24_datatable()$REG_NUMBER, freq = 1)
    # freq <- dt[, .N ,by = reg]
    # # freq[, L1 := substring(reg, 0, 1L) ]
    # freq[, L_2 := substring(reg, nchar(reg)-1L, nchar(reg)) ]
    # 
    # L_2 <- aggregate(x = list('freq' = freq$N), by = list('ending' = freq$L_2), FUN = sum)
    # abnormal_ending <- L_2[L_2$freq < sum(L_2$freq)/(length(freq$reg)*2),]$ending
    # 
    # abnormal_reg <- sort(freq[L_2 %in% abnormal_ending]$reg)
    uniquereg <- unique(as.character(FR24_datatable()$REG_NUMBER))
    
    # ______methode with jw distance #
    if(length(uniquereg) > 2){
      
      distancemodels <- stringdistmatrix(uniquereg,uniquereg,method = "jw", p = 0.1, nthread = parallel::detectCores()-1)
      rownames(distancemodels) <- uniquereg
      hc <- hclust(as.dist(distancemodels))
      
      # k = round(log(length(uniquereg)^2),0)
      if(length(uniquereg)<10){
        
        k <- length(uniquereg)
        
      } else {
        
        k <- 10
        
      }
      
      dfClust <- data.table(uniquereg, cutree(hc, k=k))
      freq <- dfClust[, .N ,by = V2]
      abnormal_cluster <- freq[freq$N < length(uniquereg)/k]$V2
      
      abnormal_reg <- sort(dfClust[V2 %in% abnormal_cluster]$uniquereg)
      
    } else {
      
      abnormal_reg <- NA
      
    }
    
    selectInput('fr24_select_reg_number2','Abnormal registration number, select if you want to remove them',
                choices = abnormal_reg, #selected = abnormal_reg,
                multiple = TRUE)
    
    
    # selectInput('fr24_select_reg_number2','Abnormal registration number, uncheck them if you do not want them',
    #                    choices = reg_number,
    #                    selected = reg_number, multiple = TRUE)
    # checkboxGroupInput('fr24_select_reg_number2','Choose the correct registration number',
    #                    choices = reg_number,
    #                    selected = reg_number, inline = TRUE)

  })
  
  ### fr24ui_select_AIRLINE ####
  output$fr24ui_select_flight_number2 <- renderUI({
    
    airline <- c(sort(unique(FR24_datatable()$AIRLINE)))

    selectInput('fr24_select_flight_number2','Choose the correct flight number',
                       choices = airline,
                       selected = airline, multiple = TRUE)
  })
  
  ### fr24ui_select_org2
  output$fr24ui_select_org2 <- renderUI({
    org <- c(sort(unique(FR24_datatable()$ORG)))
    selectInput('fr24_select_org2','Choose Origin airport(s)',
                       choices = org,
                       selected = org, multiple = TRUE)
  })
  ### fr24ui_select_dst2
  output$fr24ui_select_dst2 <- renderUI({
    dst <- c(sort(unique(FR24_datatable()$DST)))
    selectInput('fr24_select_dst2','Choose Destination airport(s)',
                       choices = dst,
                       selected = dst, multiple = TRUE)
  })
  
  BIO_AC <- eventReactive(input$fr24_refresh,{

    x <-unique(FR24_datatable()$REG_NUMBER)#input$fr24_select_reg_number2
    y <- paste(x, collapse = "', '-', '') OR replace(lc.REG_NUMBER, '-', '') = replace('")
    yy <- paste("AND (replace(lc.REG_NUMBER, '-', '') = replace('", y, "', '-', ''))", sep = "")
    # y <- paste(x, collapse = "' OR replace(lc.REG_NUMBER, '-', '') = '")
    # yy <- paste("AND (replace(lc.REG_NUMBER, '-', '') = '", y, "')", sep = "")

    body <- "
    SELECT DISTINCT
    REG_NUMBER,
    acct.SHORT_NAME AS OPERATOR_NAME,
    acct.IATA AS IATA_OP,
    acctma.SHORT_NAME AS MANAGER_NAME,
    acctma.IATA AS IATA_MN,
    AC_TYPE_CODE,
    AC_VAR_DESC,
    ORG_MANUFACTURER AS AC_MANU,
    acet.NAME AS EG_NAME,
    MANUFACTURER_NAME AS EG_MANU,
    acevt.NAME AS STATUS,
    START_DATE,
    END_DATE,
    acu.NAME AS USAGE
    FROM BIO_NEO_PROD.bio.AC_LIFECYCLE lc
    INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acct ON lc.OPERATOR_ACCOUNT_ID = acct.ACCOUNT_ID
    INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acctma ON lc.MANAGER_ACCOUNT_ID = acctma.ACCOUNT_ID
    INNER JOIN BIO_NEO_PROD.bio.AC_TYPE_VARIANT actv ON lc.AC_TYPE_VARIANT_ID = actv.AC_TYPE_VARIANT_ID
    INNER JOIN BIO_NEO_PROD.bio.AC_ENGINE_TYPE acet ON lc.ENGINE_TYPE_ID = acet.AC_ENGINE_TYPE_ID
    INNER JOIN BIO_NEO_PROD.bio.AC_EVENT_TYPE acevt ON lc.CURRENT_EVENT_TYPE_ID = acevt.AC_EVENT_TYPE_ID
    INNER JOIN BIO_NEO_PROD.bio.AC_USAGE acu ON lc.AC_USAGE_ID = acu.AC_USAGE_ID
    WHERE (END_DATE > getdate() and START_DATE < getdate())
    "
    query <- paste(body, yy, sep = '')
    # output$test2_fr24 <- renderText({query}) # show the sql query

    bio <- sqlQuery(BIO, query) # send query
    bio

  })

  
  # 4.3 join FR24 and BIO ####
  FR24_BIO_AC <- eventReactive(BIO_AC(), {
    fr24 <- data.table(FR24_datatable())
    bio <- data.table(BIO_AC())
    fr24 <- subset(fr24, !(REG_NUMBER %in% input$fr24_select_reg_number2) & 
                     AIRLINE %in% input$fr24_select_flight_number2 &
                     ORG %in% input$fr24_select_org2 &
                     DST %in% input$fr24_select_dst2)
    bio$match_reg <- gsub('-', '', bio$REG_NUMBER)
    fr24$match_reg <- gsub('-', '', fr24$REG_NUMBER)
    fr24 <- merge(fr24, bio, by = 'match_reg')
    #----------
    
    # output$fr24_bio_ac_colnames <- renderText({paste(names(fr24), collapse = ',')}) # show colnames
    #----------
    fr24
    
    # fr24 colnames: 
    # FLIGHT_NUMBER,AIRLINE,ORG,DST,AC_TYPE,
    # REG_NUMBER,TAKE_OFF_TIME,LANDING_TIME,REG_NUMBER.y,
    # OPERATOR_NAME,IATA_OP,MANAGER_NAME,IATA_MN,AC_TYPE_CODE,
    # AC_VAR_DESC,AC_MANU,EG_NAME,EG_MANU,STATUS,START_DATE,
    # END_DATE,USAGE
  })
  
  # 5. SUMMARY ####
  output$fr24_info_nb_flight <- renderInfoBox({
    nb_flight <- nrow(FR24_datatable())
    nb_flight_number <- length(unique(FR24_datatable()$FLIGHT_NUMBER))
    # p('There are ',nb_flight, ' flights recorded in the database.')
    infoBox(title = paste(nb_flight, " flights: ", sep = ''), 
                          paste('With ',nb_flight_number, 
                                ' flight numbers', sep = ''), 
                          icon = icon('rocket'), color = 'purple', fill = TRUE)
  })
  
  output$fr24_info_nb_tail <- renderInfoBox({
    tail <- length(unique(FR24_datatable()$REG_NUMBER))
    # p('There are ',nb_flight, ' flights recorded in the database.')
    infoBox(title = "Aircraft: ", paste(tail, ' diferent A/C', sep = ''),
            icon = icon('plane'), color = 'yellow', fill = TRUE)
  })
  
  output$fr24_info_nb_org <- renderInfoBox({
    nb_org <- length(unique(FR24_datatable()$ORG))
    # p('There are ',nb_flight, ' flights recorded in the database.')
    infoBox(title = "Origin: ", paste(nb_org, ' airport(s)', sep =''), 
            icon = icon('arrow-up'), color = 'green', fill = TRUE)
  })
  output$fr24_info_nb_dst <- renderInfoBox({
    nb_org <- length(unique(FR24_datatable()$DST))
    # p('There are ',nb_flight, ' flights recorded in the database.')
    infoBox(title = "Destination: ", paste(nb_org, ' airport(s)', sep =''), 
            icon = icon('arrow-down'), color = 'navy', fill = TRUE)
  })
  
  # 6. FREQUENCY ####
  ## 6.1 render selectinput####
  output$fr24ui_freq_sel_col <- renderUI({
    colname_fr24 <- names(FR24_BIO_AC())
    selectInput('fr24_freq_sel_col', label = 'Select column', choices = colname_fr24,
                selected = colname_fr24[6])
  })
  ## 6.2 make barplot####
  output$fr24_freq_check_dt_quality <- renderPlotly({
    data <- FR24_BIO_AC()
    df <- data.frame(table(data[,input$fr24_freq_sel_col]))
    
    p <- plot_ly(df[order(df$Freq, decreasing = TRUE),],
                 x = Var1,
                 y = Freq,
                 name = "Freq",
                 type = "bar")%>% 
      layout(title = paste('Distribution of ',input$fr24_freq_sel_col, sep = ''),
             scene = list(
               xaxis = list(title = "Variable"), 
               yaxis = list(title = "Frequency")
             ))
    p
    
    
    # if(nrow(df)< 10){
    #   height = 500
    # } else {
    #   height = nrow(df)*10 +450
    # }
    
    # vAxes <- paste("[{title: '", input$fr24_freq_sel_col, "', titleTextStyle: {color: '#000000'}}]")
    # gvisBarChart(df[order(df$Freq, decreasing = TRUE),],
    #                      options = list(legend = 'none', height = height, width = 800,
    #                                     title='Distribution',
    #                                     bar= "{groupWidth: '80%'}",
    #                                     # explorer= "{axis: 'vertical' }",
    #                                     hAxes = "[{title: 'Frequency', titleTextStyle: {color: '#000000'}}]",
    #                                     # vAxes = "[{title: 'Airport', titleTextStyle: {color: '#000000'}}]"
    #                                     vAxes = vAxes
    #                                     ))
    
  })
  
  # 7.Download data ####
  
  output$dl_fr24 <- downloadHandler(
    #- This function returns a string which tells the client
    #- browser what name to use when saving the file.
    filename <- function() {
      paste('FlightRadar24 ', substr(Sys.time(),1,19),'.csv', sep = "")
    },
    #- This function should write data to a file given to it by
    #- the argument 'file'.
    content <- function(file) {
      ### Write to a file specified by the 'file' argument
      write.table(data.frame(FR24_BIO_AC()), file, sep = ',',
                  row.names = FALSE, col.names = TRUE)
    }
  )
  # ****TEST OUTPUT**** ####
  output$test_fr24 <- renderDataTable({
    FR24_datatable()
    # BIO_AC()
  })

  output$text_fr24 <- renderDataTable({
    FR24_BIO_AC()

  })
  
  
  # ________ PAGE 2 fluidrow_FR24_airline ___________ ####

  # fr24_airline_network_map ####
  output$fr24_airline_network_map <- renderLeaflet({
    # content <- paste(sep = "<br/>",
    #                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
    #                  "606 5th Ave. S",
    #                  "Seattle, WA 98138"
    # )
    AP <- data.table(AIRPORT_LOCATION())
    AP_list <- unique(c(FR24_BIO_AC()$ORG,  FR24_BIO_AC()$DST))
    airport <- data.table('IATA' = AP_list)
    airport <- merge(airport, AP, by = 'IATA')

    leaflet(airport) %>% 
      addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/qxzzxq/cirm2rb5v001nh1m4efqtnd0x/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoicXh6enhxIiwiYSI6ImNpcm45emZzNDAwNWRodW5oNThobDVkenYifQ.0xoW4MMZgqDx_eimOz_Plg",
               attribution = 'Airbus Cargo Data Miner | Feedback: <a href="mailto:xuzhou.qin@gmail.com?Subject=CDM%20feedback Leaflet" target="_top">Xuzhou Qin</a>') %>%
      addMarkers(~long, ~lat, popup = ~htmlEscape(paste(IATA,name, sep = ": ")))

  })
  
#   output$fr24_airline_network_map <- renderUI({
#       AP <- data.table(AIRPORT_LOCATION())
#       AP_list <- unique(c(FR24_BIO_AC()$ORG,  FR24_BIO_AC()$DST))
#       airport <- data.table('IATA' = AP_list)
#       airport <- merge(airport, AP, by = 'IATA')
# 
#       ## create var planes
#       element <- paste('["',airport$name, '",', airport$lat, ',', airport$long, ']', sep = '')
#       list <- paste(element, collapse = ',')
#       var <- paste('var planes = [', list, '];', sep = "")
#       
#       ## create airport pairs
#       AP_pair <- unique(data.table(FR24_BIO_AC()$ORG,  FR24_BIO_AC()$DST))
#       AP_pair <- merge(AP_pair, AP, by.x = 'V1', by.y = 'IATA')
#       AP_pair <- merge(AP_pair, AP, by.x = 'V2', by.y = 'IATA')
#       colnames(AP_pair) <- c('ORG', 'DST', 'long.x', 'lat.x', 'name.x','long.y', 'lat.y', 'name.y')
#       
#       element2 <- paste('[',AP_pair$lat.x, ',', AP_pair$long.x, ',', AP_pair$lat.y,',', AP_pair$long.y, ']', sep = '')
#       list2 <- paste(element2, collapse = ',')
#       var_aptPairs <- paste('var airportPairs = [', list2, '];', sep = "")
# 
#     HTML('
# 
# <div id="mapid"></div>
# 
# <script>', var, var_aptPairs, '
# 
#           var mymap = L.map("mapid").setView([30, 90], 2);
# 
#           L.tileLayer("https://api.mapbox.com/styles/v1/mapbox/light-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoicXh6enhxIiwiYSI6ImNpcm0ycTF3NjAwM2xpMmt3c2I3ODRkZWMifQ.Kyuc_AtP_lsSUpb1KZNQhg",{
# attribution: "Airbus Cargo Data Miner",
# maxZoom: 18,
# id: "qxzzxq.12img075",
# accessToken: "pk.eyJ1IjoicXh6enhxIiwiYSI6ImNpcm0ycTF3NjAwM2xpMmt3c2I3ODRkZWMifQ.Kyuc_AtP_lsSUpb1KZNQhg"
# 
# }).addTo(mymap);
#     
#   L.Polyline.Arc([35, -89], [40, -75]).addTo(mymap);
# L.Polyline.Arc([1.6, 103], [5.3, 120]).addTo(mymap);
# 
# 		for (var i = 0; i < planes.length; i++) {
# 			marker = new L.marker([planes[i][1],planes[i][2]])
# 				.bindPopup(planes[i][0])
# 				.addTo(mymap);
# 		}
# 
# 		for (var m = 0; m < airportPairs.length; m++) {
# 			Polyline = new L.Polyline.Arc([airportPairs[m][0], airportPairs[m][1]], [airportPairs[m][2], airportPairs[m][3]]).addTo(mymap);
# 		}
# 
# </script>
# 
#          ')
# 
#   })
  
  
  
  
  
  
  
  
# ___________________END of SERVER.R__________________ ####
}



#### ////////////OLD CODE//////////// ####

# ## db_connection_check ####
# output$check_cargois = renderValueBox({
#   
#   
#   if(class(CargoDB) == 'try-error'){
#     statu <- 'Failed'
#   }else{
#     statu <- 'OK'
#   }
#   
#   output$check_sea = renderValueBox({
#     valueBox(
#       statu, "Seabury Trade",
#       icon = icon("database"), color = "red"
#     )
#   })
#   valueBox(
#     statu, "Cargo IS",
#     icon = icon("database"), color = "yellow"
#   )
# })
# 
# output$check_BIO = renderValueBox({
#   
#   if(class(BIO) == 'RODBC'){
#     statu <- 'OK'
#   }else{
#     statu <- 'Failed'
#   }
#   valueBox(
#     statu, "BIO",
#     icon = icon("database"), color = "blue"
#   )
# })
# 
# output$check_fr24 = renderValueBox({
#   
#   if(class(FR24) == 'try-error'){
#     statu <- 'Failed'
#   }else{
#     statu <- 'OK'
#     
#   }
#   # dbDisconnect(FR24)
#   valueBox(
#     statu, "FlightRadar24",
#     icon = icon("database"), color = "purple"
#   )
# })


# ## TREEMAP MANUEL ####
# G1
# output$tree.g1 = renderPlot({
#   tm=treemap(DATA.SEA(), index = 'G1NAME', vSize = substr(input$sfc.air,1,9),
#           align.labels = c('center','center'),
#           vColor = substr(input$sfc.air,11,18), type="value",
#           palette="RdYlBu", #position.legend = 'none' )
#           title = 'G1 level, size = weight, color = value')
# 
# #TODO: find the coordinates instead of using directly the values
#   output$summary <- renderTable({
#     x <- input$click1$x
#     y <- input$click1$y
#     if (!is.null(tm)) {
#       x <- (x + 0.023 + 0.000987730061349692)/1.0479754601227 #^1.0075/1.04734835284032
#       y <- (y - 0.1259)/0.827349151611328
#       l <- tmLocate(list(x=x, y=y), tm)
#       if (is.na(l[1,1])) {
#         return(NULL)
#       } else {
#         dt <- data.frame('Name' = l[1,1], 'Weight' = l[1,2], 'Value' = l[1,3])
#         return(dt)
#       }
#     } else {
#       return(NULL)
#     }
#   })
# })
# G2
# 
# output$tree.g2 = renderPlot({
#   tm2 <- treemap(DATA.SEA(), index = c('G1NAME','G2NAME'), vSize = substr(input$sfc.air,1,9),
#           align.labels = list(c('left','top'), c('center','center')),
#           vColor = substr(input$sfc.air,11,18), type="value", palette="RdYlBu",
#           title = 'G2 level, size = weight, color = value')
#   
#   output$summary2 <- renderTable({
#     x <- input$click2$x
#     y <- input$click2$y
#     if (!is.null(tm2)) {
#       x <- (x + 0.023 + 0.000987730061349692)/1.0479754601227 #^1.0075/1.04734835284032
#       y <- (y - 0.1259)/0.827349151611328
#       l <- tmLocate(list(x=x, y=y), tm2)
#       if (is.na(l[1,1])) {
#         return(NULL)
#       } else {
#       dt <- data.frame('G1NAME' = l[1,1], 'G2NAME'=l[1,2], 'Weight'=l[1,3], 'Value' = l[1,4])
#       return(dt)
#       }
#     } else {
#       return(NULL)
#     }
#   })
# })
# # G3
# output$tree.g3 = renderPlot({
#   tm3 <- treemap(DATA.SEA(), 
#                  index = c('G1NAME','G2NAME','G3NAME'), 
#                  vSize = substr(input$sfc.air,1,9),
#                  vColor = substr(input$sfc.air,11,18), 
#                  type="value", 
#                  palette="RdYlBu",
#                  align.labels = list(c('left','top'), c('center','center')),
#                  title = 'G3 level, size = weight, color = value', 
#                  border.col = c('#424949','#5F6A6A','#909497'), fontsize.labels = c(0,0,0))
#   
#   output$summary3 <- renderTable({ 
#     x <- input$click3$x
#     y <- input$click3$y
#     if (!is.null(tm3)) {
#       x <- (x + 0.023 + 0.000987730061349692)/1.0479754601227 #^1.0075/1.04734835284032
#       y <- (y - 0.1259)/0.827349151611328
#       l <- tmLocate(list(x=x, y=y), tm3)
#       if (is.na(l[1,1])) {
#         return(NULL)
#       } else {
#       dt <- data.frame('G1NAME' = l[1,1], 'G2NAME'=l[1,2], 'G3NAME'=l[1,3],
#                    'Weight'=l[1,4], 'Value' = l[1,5])
#       return(dt)
#       }
#     } else {
#       return(NULL)
#     }
#   })
# })
# # G4
# output$tree.g4 = renderPlot({
#   tm4 <- treemap(DATA.SEA(), index = c('G1NAME','G2NAME','G3NAME','G4NAME'), vSize = substr(input$sfc.air,1,9),
#           align.labels = list(c('right','bottom'), c('center','center')),
#           vColor = substr(input$sfc.air,11,18), type="value", palette="RdYlBu",
#           title = 'G4 level, size = weight, color = value',
#           border.col = c('#424949','#5F6A6A','#909497','#D0D3D4'), fontsize.labels = c(0,0,0,0))
#   output$summary4 <- renderTable({
#     x <- input$click4$x
#     y <- input$click4$y
#     if (!is.null(tm4)) {
#       x <- (x + 0.023 + 0.000987730061349692)/1.0479754601227 #^1.0075/1.04734835284032
#       y <- (y - 0.1259)/0.827349151611328
#       l <- tmLocate(list(x=x, y=y), tm4)
#       if (is.na(l[1,1])) {
#         return(NULL)
#       } else {
#       dt <- data.frame('G1NAME' = l[1,1], 'G2NAME'=l[1,2], 'G3NAME'=l[1,3],'G4NAME'=l[1,4],
#                    'Weight'=l[1,5], 'Value' = l[1,6])
#       return(dt)
#       }
#     } else {
#       return(NULL)
#     }
#   })
# })

# ## old code of evolution plot with plotly####
# Chart of yld evo
# output$evo_yield = renderPlotly({ 
#     if(input$yld_evo){
#       data2 = DATA2()
#       data2[is.na(data2)]=0
#       
#       year.range = c(2010:2016) # year range to loop
#       
#       ## define NULL output df * 3 
#       total_evolution_gy = data.frame(Month = NULL, x= NULL)
#       total_evolution_ny = data.frame(Month = NULL, x= NULL)
#       total_weight_evo = data.frame(Month = NULL, x= NULL)
#       
#       ## loop to compute the yld and weight
#       for (year in year.range){
#         evo = gross_yield_evolution(data2,year)
#         evo$Months = as.Date(as.yearmon(paste(year, evo$Months, sep = '-')))
#         total_evolution_gy = rbind(total_evolution_gy, evo)
#         
#         evo2 = net_yield_evolution(data2,year)
#         evo2$Months = as.Date(as.yearmon(paste(year, evo2$Months, sep = '-')))
#         total_evolution_ny = rbind(total_evolution_ny, evo2)
#         
#         evo_weight = mon_cargo_evolution(data2, year)
#         evo_weight$Months = as.Date(as.yearmon(paste(year, evo_weight$Months, sep = '-')))
#         total_weight_evo = rbind(total_weight_evo, evo_weight)
#       }
#       
#       evo_yld = data.frame(Months = total_evolution_gy[,1], gy = total_evolution_gy[,2], ny = total_evolution_ny[,2], 
#                            weight =  total_weight_evo[,2])
#       
#       # Chart of weight
#       output$evo_weight <-  
#         renderPlotly({
#           if(input$yld_evo == TRUE){
#             
#             title = paste('Cargo weight from', input$org, 'to', input$dst)
#             
#             plot_ly(evo_yld, x = Months, y = weight, name = 'Weight',  line = list(shape = "linear"))%>%
#               layout(yaxis = list(title = 'Weight: kt'), title= title)
#           }
#         })
#       
#       # output the table
#       output$evo_plot_dtable <-  
#         renderDataTable({
#           if(input$yld_evo == TRUE){
#             data.frame(Months = evo_yld[,1], 
#                        Gross_Yield = round(evo_yld[,2],digits = 3), 
#                        Net_Yield = round(evo_yld[,3], digits = 3),
#                        Weight = round(evo_yld[,4], digits = 2))
#           }
#         })
#       
#       title <- paste('Yield evolution from', input$org, 'to', input$dst)
#       p <- plot_ly(evo_yld, x = Months, y = gy, name = 'Gross yield',  line = list(shape = "linear"))%>%
#         add_trace(x = Months, y = ny, name = 'Net yield', line = list(shape = "linear"))%>%
#         # if(input$min_max_avg){
#         #   minmaxavg <- data.frame('Months' = evo_yld$Months, 
#         #                           'max_grs' = max(evo_yld$gy), 
#         #                           'max_net' = max(evo_yld$ny),
#         #                           'min_grs' = min(evo_yld$gy),
#         #                           'min_net' = min(evo_yld$ny),
#         #                           'avg_grs' = sum(data2$WEIGHT_CHARGES_CURR_YEAR_USD + data2$OTHER_CHARGES_CURR_YEAR_USD)/sum(data2$WEIGHT_CURRENT_YEAR),
#         #                           'avg_net' = sum(data2$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(data2$WEIGHT_CURRENT_YEAR)
#         #   )
#         #   
#         #   p %>% add_trace(minmaxavg, x=Months, y=max_grs, name = 'Max gross yield',  line = list(shape = "linear")) %>%
#         #     add_trace(minmaxavg, x=Months, y=max_net, name = 'Max net yield',  line = list(shape = "linear")) %>%
#         #     add_trace(minmaxavg, x=Months, y=min_grs, name = 'Min gross yield',  line = list(shape = "linear")) %>%
#         #     add_trace(minmaxavg, x=Months, y=min_net, name = 'Min net yield',  line = list(shape = "linear")) %>%
#         #     add_trace(minmaxavg, x=Months, y=avg_grs, name = 'average gross yield',  line = list(shape = "linear")) %>%
#         #     add_trace(minmaxavg, x=Months, y=avg_net, name = 'average net yield',  line = list(shape = "linear")) 
#         # }
#         layout(yaxis = list(title = 'Yield: USD/kg'), title = title)
#       
#     }
#     
#     else {
#       p <- plot_ly( x = 3, y =2,  text = 'Please check the box "Yield evolution plot"', mode = "markers+text", textposition  = 'top middle')%>%
#       layout(yaxis = list(title = 'Yield: USD/kg'))
#     }
#   
#     p # output the plot
# })

# ## TREEMAP OF ORG DST COUNTRIES CARGOIS####
# treemap of org countries
# output$tree.org_cty = renderPlot({
#   x = data.frame(country=factor(DATA()$ORI_COUNTRY), Weight = DATA()$WEIGHT_CURRENT_YEAR/1000000)
#   x[is.na(x)]=0
#   tree = aggregate(x=list(weight = x$Weight), by=list(country=x$country), FUN=sum)
#   treemap(tree, index = 'country', vSize = 'weight', vColor = 'weight', type="value", palette="RdYlBu", 
#           title = 'Treemap of origin countries (kt)')   
#   
#   ## table of the treemap
#   output$tree.org_cty_dtable = renderDataTable({
#     data.frame('Countries' = tree$country, 'Weight_kt' = round(tree$weight, digits = 2),
#                'Percent' = round(tree$weight/sum(tree$weight), digits = 2)*100)
#   })
# })


# treemap of dst countries
# output$tree.dst_cty = renderPlot({
#   x = data.frame(country=factor(DATA()$DST_COUNTRY), Weight = DATA()$WEIGHT_CURRENT_YEAR/1000000)
#   x[is.na(x)]=0
#   tree = aggregate(x=list(weight = x$Weight), by=list(country=x$country), FUN=sum)
#   treemap(tree, index = 'country', vSize = 'weight', vColor = 'weight', type="value", palette="RdYlBu",
#           title = 'Treemap of destination countries (kt)')  
#   
#   ## table of the treemap
#   output$tree.dst_cty_dtable = renderDataTable({
#     data.frame('Countries' = tree$country, 'Weight_kt' = round(tree$weight, digits = 2),
#                'Percent' = round(tree$weight/sum(tree$weight), digits = 2)*100)
#   })
# })

## 4.2 BIO load data ####
# BIO_AC <- reactiveValues()
# if(exists('input$fr24_select_reg_number2')){
#   BIO_AC$data<- eventReactive(input$fr24_refresh,{
#     x <- input$fr24_select_reg_number2
#     y <- paste(x, collapse = "' OR replace(lc.REG_NUMBER, '-', '') = '")
#     yy <- paste("AND (replace(lc.REG_NUMBER, '-', '') = '", y, "')", sep = "")
#     
#     body <- "
#     SELECT DISTINCT
#     REG_NUMBER,
#     acct.SHORT_NAME AS OPERATOR_NAME,
#     acct.IATA AS IATA_OP,
#     acctma.SHORT_NAME AS MANAGER_NAME,
#     acctma.IATA AS IATA_MN,
#     AC_TYPE_CODE,
#     AC_VAR_DESC,
#     ORG_MANUFACTURER AS AC_MANU,
#     acet.NAME AS EG_NAME,
#     MANUFACTURER_NAME AS EG_MANU,
#     acevt.NAME AS STATUS,
#     START_DATE,
#     END_DATE,
#     acu.NAME AS USAGE
#     FROM BIO_NEO_PROD.bio.AC_LIFECYCLE lc
#     INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acct ON lc.OPERATOR_ACCOUNT_ID = acct.ACCOUNT_ID
#     INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acctma ON lc.MANAGER_ACCOUNT_ID = acctma.ACCOUNT_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_TYPE_VARIANT actv ON lc.AC_TYPE_VARIANT_ID = actv.AC_TYPE_VARIANT_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_ENGINE_TYPE acet ON lc.ENGINE_TYPE_ID = acet.AC_ENGINE_TYPE_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_EVENT_TYPE acevt ON lc.CURRENT_EVENT_TYPE_ID = acevt.AC_EVENT_TYPE_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_USAGE acu ON lc.AC_USAGE_ID = acu.AC_USAGE_ID
#     WHERE (END_DATE > getdate() and START_DATE < getdate()) 
#     "
#     query <- paste(body, yy, sep = '')
#     output$test2_fr24 <- renderText({query}) # show the sql query
#     
#     bio <- sqlQuery(BIO, query) # send query
#     bio
#     
#   })
# } else {
#   BIO_AC$data<- eventReactive(input$fr24_select_reg_number2,{
#     x <- input$fr24_select_reg_number2
#     y <- paste(x, collapse = "' OR replace(lc.REG_NUMBER, '-', '') = '")
#     yy <- paste("AND (replace(lc.REG_NUMBER, '-', '') = '", y, "')", sep = "")
#     
#     body <- "
#     SELECT DISTINCT
#     REG_NUMBER,
#     acct.SHORT_NAME AS OPERATOR_NAME,
#     acct.IATA AS IATA_OP,
#     acctma.SHORT_NAME AS MANAGER_NAME,
#     acctma.IATA AS IATA_MN,
#     AC_TYPE_CODE,
#     AC_VAR_DESC,
#     ORG_MANUFACTURER AS AC_MANU,
#     acet.NAME AS EG_NAME,
#     MANUFACTURER_NAME AS EG_MANU,
#     acevt.NAME AS STATUS,
#     START_DATE,
#     END_DATE,
#     acu.NAME AS USAGE
#     FROM BIO_NEO_PROD.bio.AC_LIFECYCLE lc
#     INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acct ON lc.OPERATOR_ACCOUNT_ID = acct.ACCOUNT_ID
#     INNER JOIN BIO_NEO_PROD.bio.ACCOUNT acctma ON lc.MANAGER_ACCOUNT_ID = acctma.ACCOUNT_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_TYPE_VARIANT actv ON lc.AC_TYPE_VARIANT_ID = actv.AC_TYPE_VARIANT_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_ENGINE_TYPE acet ON lc.ENGINE_TYPE_ID = acet.AC_ENGINE_TYPE_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_EVENT_TYPE acevt ON lc.CURRENT_EVENT_TYPE_ID = acevt.AC_EVENT_TYPE_ID
#     INNER JOIN BIO_NEO_PROD.bio.AC_USAGE acu ON lc.AC_USAGE_ID = acu.AC_USAGE_ID
#     WHERE (END_DATE > getdate() and START_DATE < getdate()) 
#     "
#     query <- paste(body, yy, sep = '')
#     output$test2_fr24 <- renderText({query}) # show the sql query
#     
#     bio <- sqlQuery(BIO, query) # send query
#     bio
#     
#   })
# }

# 
# # 4.3 join FR24 and BIO ###
# FR24_BIO_AC <- eventReactive(BIO_AC(), {
#   fr24 <- FR24_datatable()
#   bio <- BIO_AC()
#   fr24 <- subset(fr24, REG_NUMBER %in% input$fr24_select_reg_number2 & 
#                    FLIGHT_NUMBER %in% input$fr24_select_flight_number2 &
#                    ORG %in% input$fr24_select_org2 &
#                    DST %in% input$fr24_select_dst2)
#   bio$match_reg <- gsub('-', '', bio$REG_NUMBER)
#   fr24 <- left_join(fr24, bio, by=c('REG_NUMBER'='match_reg'))
#   #----------
#   
#   # output$fr24_bio_ac_colnames <- renderText({paste(names(fr24), collapse = ',')}) # show colnames
#   #----------
#   fr24
#   # fr24 colnames: 
#   # FLIGHT_NUMBER,AIRLINE,ORG,DST,AC_TYPE,
#   # REG_NUMBER,TAKE_OFF_TIME,LANDING_TIME,REG_NUMBER.y,
#   # OPERATOR_NAME,IATA_OP,MANAGER_NAME,IATA_MN,AC_TYPE_CODE,
#   # AC_VAR_DESC,AC_MANU,EG_NAME,EG_MANU,STATUS,START_DATE,
#   # END_DATE,USAGE
# })