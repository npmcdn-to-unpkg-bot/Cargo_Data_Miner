#######################################################
#                   PLOT AIR ROUTE                    #
#######################################################

Plot.air.route = function(apt_pair,color = 'Blues', map_center = 10, line_size = 0.2, point_size = 0.5){
  x = apt_pair
  names(x) = c('airport', 'cnt')
  
  airports <- read.table(file.path(DATA, 'Match table airport.csv'), 
                         header=T,  sep = ",",
                         col.names = c('IATA', 'name','city_code', 'city_name', 'country', 'lat','long','loc_type','lat_Rad','lon_Rad'))
  # col.names = c('ID', 'name','city','country', 'IATA', 'ICAO', 'lat','long','altitude','timezone','DST', 'Tz_DB_Timezone'))

  count = aggregate(x$cnt, by = list(airport = x$airport), FUN = sum)
  count$airport1 = substr(count$airport,1,3)
  count$airport2 = substr(count$airport,5,7)
  colnames(count)=c('pair','cnt','airport1','airport2')
  
  airport_locations <- airports[, c("IATA","long", "lat")]
  
  flights.ag = count
  colnames(flights.ag) = c('pair','freq','From','To')
  
  OD <- left_join(flights.ag, airport_locations, by=c("From"="IATA") )
  OD <- left_join(OD, airport_locations, by=c("To"="IATA") )
  OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair
  
  OD.nodata = OD[is.na(OD$long.x)== T | is.na(OD$long.y)== T ,]
  # OD = OD[is.na(OD$long.x)== F & is.na(OD$long.y)== F, ]
  
  if (nrow(OD[is.na(OD$long.x),]) != 0 | nrow(OD[is.na(OD$long.y),]) != 0){
    plot = OD.nodata
    
    warn = paste("Can not find the airport code in the database:",OD.nodata$pair)
    warning(warn)
  }
  
  else {
  routes <- gcIntermediate(OD[,c(5,6)], OD[,c(7,8)], 100, breakAtDateLine=TRUE, addStartEnd=TRUE, sp=TRUE)
  
  ids <- data.frame()
  # fill data frame with IDs for each line
  for (i in (1:length(routes))) {         
    id <- data.frame(routes@lines[[i]]@ID)
    ids <- rbind(ids, id)  }
  
  colnames(ids)[1] <- "ID" # rename ID column
  
  # convert SpatialLines into SpatialLinesDataFrame using IDs as the data frame
  routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = T)
  
  # Fortify routes (convert to data frame)  +++  join attributes
  routes_df <- fortify(routes, region= "ID") # convert into something ggplot can plot
  gcircles <- left_join(routes_df, OD, by= ("id"))
  head(gcircles)
  
  center <- map_center # positive values only - US centered view is 260
  
  gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long) 
  
  # shift coordinates to recenter worldmap
  worldmap <- map_data ("world")
  worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)
  
  ### Function to regroup split lines and polygons
  # takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
  RegroupElements <- function(df, longcol, idcol){  
    g <- rep(1, length(df[,longcol]))
    if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
      d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
      g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
      g[d] <- 2      # parts that are moved
    }
    g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
    df$group.regroup <- g
    df
  }
  
  ### Function to close regrouped polygons
  # takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
  ClosePolygons <- function(df, longcol, ordercol){
    if (df[1,longcol] != df[nrow(df),longcol]) {
      tmp <- df[1,]
      df <- rbind(df,tmp)
    }
    o <- c(1: nrow(df))  # rassign the order variable
    df[,ordercol] <- o
    df
  }
  
  # now regroup
  gcircles.rg <- ddply(gcircles, .(id), RegroupElements, "long.recenter", "id")
  worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
  
  # close polys
  worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var
  
  
  dst = gcircles.rg[gcircles.rg$order == 102,]
  org = gcircles.rg[gcircles.rg$order == 1,]
  # Flat map
  palette = color
  
  plot = ggplot() +
    geom_polygon(data=worldmap.cp, aes(long.recenter,lat,group=group.regroup), size = 0.1, colour= "white", fill="#E0E0E0") +
    geom_line(data= gcircles.rg, aes(long.recenter,lat,group=group.regroup, col = freq), size= line_size, alpha= 0.45) +
    scale_colour_distiller(palette = palette, name="Value", direction = 1, guide = 'colourbar') +
    theme_map()+
    theme(panel.background = element_rect(fill = "white"))+
    ylim(-60, 90) +
    coord_equal() +
    geom_point(data = dst, aes(long.recenter, lat), color = 'blue', size = point_size, stroke = 0, shape = 18, alpha = 0.6)+
    geom_point(data = org, aes(long.recenter, lat), color = 'red', size = point_size-0.2 , stroke = 0, shape = 16, alpha = 0.6)
  
  }
  return(plot)

}


# route = CargoIS.SQL.query.weight(CargoDB, min.Weight = 10400000, max.Weight = 187200000, 2015)
# 
# good.route = data.frame(paste(route$ORIGIN_AIRPORT_CODE, route$DESTINATION_AIRPORT_CODE, sep = '_'), route$WEIGHT_CURRENT_YEAR)
# Plot.air.route(good.route,line_size = 1, point_size = 1)
