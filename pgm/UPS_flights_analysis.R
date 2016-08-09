######### UPS FLIGHTs #########
con <- dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01")
rs <- dbSendQuery(con, "
                  SELECT *
                  FROM (
                    select * 
                    from FR24.FLIGHT data
                    where data.AIRLINE = 'FX') data2
                  INNER JOIN FR24.FLIGHT_DETAIL dtl ON data2.FLIGHT_ID = dtl.FLIGHT_ID
                  INNER JOIN FR24.AIRCRAFT acf ON dtl.AIRCRAFT_ID = acf.AIRCRAFT_ID
                  ;")
UPS.data <- fetch(rs, n = -1)
huh <- dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(con)
# write.csv(UPS.data, 'E:/DATA/FR24_2d.csv')
ups.routes <- UPS.data[,c('FLIGHT_NUMBER','ORG','DST','AIRLINE','TAKE_OFF_TIME', 'LANDING_TIME', 'AC_TYPE', 'REG_NUMBER')]

# remove rows with NA
ups.g.routes <- ups.routes[complete.cases(ups.routes[,c(2,3)]),]
# ups.good.routes <- data.frame(airport = paste(ups.g.routes$ORG,ups.g.routes$DST,sep = '-'), cnt = 1, lsize = 1)
plot.df <- data.frame(ups.g.routes)

geo <- list(
  # scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  showcountries = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)
airport_locations <- get.airport.loc()
OD <- left_join(plot.df, airport_locations, by=c("ORG"="IATA") )
OD <- left_join(OD, airport_locations, by=c("DST"="IATA") )

ac.type = names(table(OD$AC_TYPE))
color <- brewer.pal(length(ac.type), 'Set1')

p <- plot_ly(OD, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(OD$ORG, OD$DST),
        type = 'scattergeo',
        mode = "markers+text", textposition  = 'top middle', group = 'Airports',
        marker = list(size = 6, color = 'grey'),
        inherit = FALSE)
  

for(i in 1:length(ac.type)){
  p <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                group = c(paste(AC_TYPE, ORG,DST, FLIGHT_NUMBER,sep = '-')),data = OD[OD$AC_TYPE == ac.type[i],],opacity = 0.5,
                mode = 'lines', line = list(width =2, color = color[i] ),
                type = 'scattergeo', evaluate = TRUE)
}
p  %>%  layout(geo = geo)


write.csv(ups.routes, file = paste(OUTPUT,'ASL Airlines France_FR24_data.csv', sep = "/"))

