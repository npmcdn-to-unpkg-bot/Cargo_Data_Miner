######## Plot.air.route.html (Plotly) ##########

Plot.air.route.html = function(apt_pair, pt.color = '#3498DB', ln.color = '#3498DB' , projection = 'Mercator', point_size = 8,width = 950, height = 600){
  x = apt_pair
  names(x) = c('airport', 'cnt', 'lsize')
  
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

  count = x
  count$airport1 = substr(count$airport,1,3)
  count$airport2 = substr(count$airport,5,7)
  colnames(count)=c('pair','cnt','lsize','airport1','airport2')
  
  airport_locations <- get.airport.loc()
  
  flights.ag = count
  colnames(flights.ag) = c('pair','freq','lsize','From','To')
  
  OD <- left_join(flights.ag, airport_locations, by=c("From"="IATA") )
  OD <- left_join(OD, airport_locations, by=c("To"="IATA") )
  OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair
  
  OD.nodata = OD[is.na(OD$long.x)== T | is.na(OD$long.y)== T ,]
  # OD = OD[is.na(OD$long.x)== F & is.na(OD$long.y)== F, ]
  
  if (nrow(OD[is.na(OD$long.x),]) != 0 | nrow(OD[is.na(OD$long.y),]) != 0){
    plot = OD.nodata
    
    warn = paste("Can not find the airport code in the database:",OD.nodata$pair)
    warning(warn)
  } else {
    p = plot_ly(OD, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(substr(OD$pair,1,3),substr(OD$pair,5,7)), type = 'scattergeo',
                mode = "markers+text", textposition  = 'top middle',
                marker = list(size = point_size, color = pt.color),
                inherit = FALSE) %>%
      
      add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                # group = id, opacity = (freq/max(freq))^1.5, data = OD,
                group = id, opacity = lsize , data = OD,
                mode = 'lines', line = list(width = (freq/max(freq)*3)^2, color = ln.color),
                type = 'scattergeo') %>%
      
      layout(geo = geo, showlegend = FALSE,width = width, height = height)
  }
  
  return(p)  
}
