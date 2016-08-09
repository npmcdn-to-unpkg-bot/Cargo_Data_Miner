######## Plot.airport.html (Plotly) ##########

# Plot.airport.html(test2, projection = 'Mercator',plot.airport = TRUE, top = 30,pt.size = 90000)
# airport = cargo[cargo[,1] == 2015,]




Plot.airport.html = function(airport, projection = 'Mercator', top = 20, plot.airport = TRUE, pt.size = 800000, table = FALSE){
# airport: data frame with five columns
# c('ORIGIN_AIRPORT_CODE','DESTINATION_AIRPORT_CODE','WEIGHT_CURRENT_YEAR','WEIGHT_CHARGES_CURR_YEAR_USD','OTHER_CHARGES_CURR_YEAR_USD')
  airport_locations <- get.airport.loc()#airports[, c("IATA","long", "lat")]
  
  ## define geometric parameters
  if(projection=='Mercator'){
    geo <- list(
      # scope = 'world',
      projection = list(type = 'Mercator'),
      showland = TRUE,
      showcountries = TRUE,
      countrywidth = 0.5,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
  }
  if(projection=='orthographic'){
    geo <- list(
      showland = TRUE,
      showlakes = TRUE,
      showcountries = TRUE,
      showocean = TRUE,
      countrywidth = 0.5,
      landcolor = toRGB("grey90"),
      lakecolor = toRGB("white"),
      oceancolor = toRGB("white"),
      projection = list(
        type = 'orthographic',
        rotation = list(
          lon = -100,
          lat = 40,
          roll = 0
        )
      ),
      lonaxis = list(
        showgrid = TRUE,
        gridcolor = toRGB("gray40"),
        gridwidth = 0.5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridcolor = toRGB("gray40"),
        gridwidth = 0.5
      )
    )
  }
  
  x <- data.frame(org = airport$ORIGIN_AIRPORT_CODE,dst = airport$DESTINATION_AIRPORT_CODE,
                  weight = airport$WEIGHT_CURRENT_YEAR,
                  wgt_chg = airport$WEIGHT_CHARGES_CURR_YEAR_USD,
                  sur_chg = airport$OTHER_CHARGES_CURR_YEAR_USD, stringsAsFactors = FALSE)
  x[is.na(x)]=0

  ########### plot.airport = TRUE #########  
  if(plot.airport){
    ## reform data structure
    wgt.ori <- aggregate(list(wgt = x$weight, wgt_chg = x$wgt_chg, sur_chg = x$sur_chg),
                         by=list(airport=x$org), FUN=sum)
    
    wgt.ori$net_yld <- wgt.ori$wgt_chg/wgt.ori$wgt
    wgt.ori$grs_yld <- (wgt.ori$wgt_chg + wgt.ori$sur_chg)/wgt.ori$wgt
    wgt.ori <- wgt.ori[order(-wgt.ori$wgt),]
    
    wgt.dst <- aggregate(list(wgt = x$weight, wgt_chg = x$wgt_chg, sur_chg = x$sur_chg),
                         by = list(airport = x$dst), FUN = sum)
    
    wgt.dst$net_yld <- wgt.dst$wgt_chg/wgt.dst$wgt
    wgt.dst$grs_yld <- (wgt.dst$wgt_chg + wgt.dst$sur_chg)/wgt.dst$wgt
    wgt.dst <- wgt.dst[order(-wgt.dst$wgt),]
    
    cnt.dst <- wgt.dst
    cnt.ori <- wgt.ori
    
    if( nrow(cnt.dst) > top ){
      count.dst <- cnt.dst[1:top,]
    }else{
      count.dst <- cnt.dst
    }
    
    if( nrow(cnt.ori) > top ){
      count.ori <- cnt.ori[1:top,]
    }else{
      count.ori <- cnt.ori
    }
    
    count.dst$label <- 'Destination'
    count.ori$label <- 'Origin'
    
    count <- rbind(count.dst,count.ori)
    names(count) <- c('airport', 'weight', 'weight_charges', 'surcharges', 'net_yield', 'gross_yield', 'label')
    flights.ag <- count
    OD <- left_join(flights.ag, airport_locations, by=c("airport"="IATA") )
    OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair
    
    OD.nodata <- OD[is.na(OD$long.x)== T | is.na(OD$long.y)== T ,]
    
    ###########  plot.airport = TRUE, output data table = TRUE #########
    if(table){
      return(count)
      
    ###########  plot.airport = TRUE, output data table = FALSE #########
    } else {
      if (nrow(OD[is.na(OD$long.x),]) != 0 | nrow(OD[is.na(OD$long.y),]) != 0){
        plot <- OD.nodata
        warn <- paste("Can not find the airport code in the database:",OD.nodata$airport, '  
                        ')
        warning(warn)
        
      } else {
        
        p <- plot_ly(OD, lon = long, lat = lat, text = airport, type = 'scattergeo',
                    mode = "markers+text", textposition  = 'top middle', 
                    group = label,
                    marker = list(size = sqrt(weight/pt.size)+3, color = gross_yield),
                    inherit = FALSE) %>% layout(geo = geo)
      }
      return(p) 
    } 
    
  ###########  plot.airport = FALSE #########
  } else {
    
    wgt.route <- aggregate(list(wgt = x$weight, wgt_chg = x$wgt_chg, sur_chg = x$sur_chg),
                         by=list(org=x$org, dst = x$dst), FUN=sum)
    wgt.route$net_yld <- wgt.route$wgt_chg/wgt.route$wgt
    wgt.route$grs_yld <- (wgt.route$wgt_chg + wgt.route$sur_chg)/wgt.route$wgt
    wgt.route <- wgt.route[order(-wgt.route$wgt),]
    
    if(nrow(wgt.route) > top){
      route <- wgt.route[1:top,]
    } else {
      route <- wgt.route
    }
    
    names(route) <- c('Origin', 'Destination', 'weight', 'weight_charges', 'surcharges', 'net_yield', 'gross_yield')
    flights.ag <- route
    
    OD <- left_join(flights.ag, airport_locations, by=c("Origin"="IATA") )
    OD <- left_join(OD, airport_locations, by=c("Destination"="IATA") )
    OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair
    
    OD.nodata = OD[is.na(OD$long.x)== T | is.na(OD$long.y)== T ,]
    # OD = OD[is.na(OD$long.x)== F & is.na(OD$long.y)== F, ]
    
    ###########  plot.airport = FALSE, output data table = TRUE #########
    if(table){
      return(route)
      
    ###########  plot.airport = FALSE, output data table = FALSE #########
    } else {
      if (nrow(OD[is.na(OD$long.x),]) != 0 | nrow(OD[is.na(OD$long.y),]) != 0){
        plot = OD.nodata
        
        warn = paste("Can not find the airport code in the database:",OD.nodata$pair)
        warning(warn)
        
      } else {
        
        p <-  plot_ly(OD, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(OD$Origin, OD$Destination),
                    type = 'scattergeo',
                    mode = "markers+text", textposition  = 'top middle', group = 'Airports',
                    marker = list(size = 8, color = "#3498DB"), 
                    inherit = FALSE) %>%
              add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                    group = c(paste(OD$Origin,OD$Destination, sep = '-')),  data = OD,
                    opacity = (gross_yield/max(gross_yield)),
                    mode = 'lines', line = list(width = ((weight/max(weight))*3)^2,
                                                color = '#3498DB' ),
                    type = 'scattergeo') %>%
              layout(geo = geo)
      }
      return(p) 
    }
  } 
}


