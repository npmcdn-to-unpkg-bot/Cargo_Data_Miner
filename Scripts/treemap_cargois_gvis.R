######### googleVis Treemap #########

treemap_cargois_gvis <- function(Data, ORG = TRUE, PLOT = TRUE){
  if(ORG){
    # aggregate table for airport level
    # colonnes : { Group.1, Group.2, weight, grsyld}
    airport_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                    'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                               by = list(Data$ORIGIN_AIRPORT_CODE, Data$ORI_COUNTRY), FUN = sum)
    
    country_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                    'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                               by = list(Data$ORI_COUNTRY, Data$ORI_REGION), FUN = sum)
    
    region_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                   'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                              by = list(Data$ORI_REGION, Data$ORI_REGION), FUN = sum)
    
    
  }else{
    airport_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                    'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                               by = list(Data$DESTINATION_AIRPORT_CODE, Data$DST_COUNTRY), FUN = sum)
    
    country_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                    'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                               by = list(Data$DST_COUNTRY, Data$DSR_REGION), FUN = sum)
    
    region_level <- aggregate(list('weight'=Data$WEIGHT_CURRENT_YEAR,
                                   'grschg'=Data$WEIGHT_CHARGES_CURR_YEAR_USD + Data$OTHER_CHARGES_CURR_YEAR_USD),
                              by = list(Data$DSR_REGION, Data$DSR_REGION), FUN = sum)
    
  }
  
  region_level$Group.2 <- 'Globe'
  tt <- rbind(airport_level,country_level, region_level)
  globe <- data.frame('Globe', NA, sum(tt$weight), sum(tt$grschg))
  names(globe) <- names(tt)
  tt <- rbind(tt,globe)
  tt$grsyld <- tt$grschg/tt$weight
  tt$grsyld[tt$grsyld =='NaN'] <- 0
  
  
  tt$lgrsyld <- (log(5*tt$grsyld+20))^2
  tt$lgrsyld[tt$lgrsyld > 15] <- 15
  
  if(PLOT){
    gvisTreeMap(data = tt, idvar = 'Group.1', parentvar = 'Group.2', sizevar = 'weight',
                colorvar = 'lgrsyld',
                options=list( #width=650, height=400,
                             fontSize=16,
                             minColor='#F5B041',
                             midColor='#F0F3F4',
                             maxColor='#5499C7',
                             headerHeight=20,
                             fontColor='black',
                             showScale=TRUE))
  }else {
    df <- tt[,c(1,2,3,5)]
    names(df) <- c('Name','Parent','Weight','GrossYield')
    df[order(df$Weight,decreasing = TRUE),]
  }

}