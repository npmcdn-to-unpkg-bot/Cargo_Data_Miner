######### 5X FR24 1month data #########
FR24_5X <- read.csv(file.path(DATA,'FR24_1month_5X_fdp.csv'),header = TRUE)
head(FR24_5X)

table(FR24_5X$FLIGHT_NUMBER)

fn_5X1269 <- subset(FR24_5X, FLIGHT_NUMBER == '5X15')
fn_5X1269 <- data.frame(fn_5X1269, stringsAsFactors = FALSE)
(as.POSIXlt(fn_5X1269$POS_TIME))
head(fn_5X1269)

fn_5X1269$POS_TIME <- as.POSIXlt(fn_5X1269$POS_TIME)
fn_5X1269 <- fn_5X1269[order(fn_5X1269$POS_TIME),]

# create variable: flight time
fn_5X1269$flight_time <- fn_5X1269$POS_TIME - as.POSIXlt(fn_5X1269$MIN_TIME)
fn_5X1269$date <- substr(fn_5X1269$POS_TIME,1,10)
# fn_5X1269$time <- substr(fn_5X1269$POS_TIME,)
# fn_5X1269 <- fn_5X1269[fn_5X1269$ALTITUDE!=0,]
# test <- fn_5X1269[fn_5X1269$FLIGHT_DETAIL_ID == "123789882",]
# test2 <- fn_5X1269[fn_5X1269$FLIGHT_DETAIL_ID == "122339334",]
# p <- plot_ly(data = test, x = flight_time, y = ALTITUDE) 
# P <-   add_trace(test2, x = flight_time, y = ALTITUDE)

p <- plot_ly(data = fn_5X1269, x = flight_time, y = ALTITUDE,group = FLIGHT_DETAIL_ID, text = paste(ORG,DST,sep = '-'))  


#########################################

FR24_5X.fd <- read.csv(file.path(DATA,'FR24_1month_5X_fd.csv'), header = TRUE)

FR24_5X.fd <- data.frame(FR24_5X.fd, stringsAsFactors = FALSE)


path <- data.frame('From' = ORG, 'To' = DST, 'freq' = 1, 'AC_type' = AC_TYPE)

Freq.5X_AC <- aggregate(x = list('Total_Frequency' = path$freq), by=list('From' = path$From, 'To' = path$To, 'AC' = AC_TYPE), FUN = sum)
Freq.5X <- aggregate(x = list('Total_Frequency' = path$freq), by=list('From' = path$From, 'To' = path$To), FUN = sum)


#############################################
freq.MD11 <- Freq.5X_AC[Freq.5X_AC$AC == 'MD11',]



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
OD <- left_join(freq.MD11, airport_locations, by=c("From"="IATA") )
OD <- left_join(OD, airport_locations, by=c("To"="IATA") )

p <- plot_ly(OD, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(OD$From, OD$To),
             type = 'scattergeo',
             mode = "markers+text", textposition  = 'top middle', group = 'Airports',
             marker = list(size = 6, color = 'grey'),
             inherit = FALSE)


p <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
               group = c(paste(OD$From, OD$To, sep = '-')),data = OD,opacity = 0.5,
               mode = 'lines', line = list(width =(Total_Frequency/max(Total_Frequency))*8, color = '#2471A3' ),
               type = 'scattergeo', evaluate = TRUE)

p  %>%  layout(geo = geo, title = "MD11 routing (sept. 2015), thickness = frequency")
############################################
freq.B744 <- Freq.5X_AC[Freq.5X_AC$AC == 'B744',]

OD744 <- left_join(freq.B744, airport_locations, by=c("From"="IATA") )
OD744 <- left_join(OD744, airport_locations, by=c("To"="IATA") )

p744 <- plot_ly(OD744, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(OD744$From, OD744$To),
             type = 'scattergeo',
             mode = "markers+text", textposition  = 'top middle', group = 'Airports',
             marker = list(size = 6, color = 'grey'),
             inherit = FALSE)


p744 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
               group = c(paste(OD744$From, OD744$To, sep = '-')),data = OD744, opacity = 0.5,
               mode = 'lines', line = list(width =(Total_Frequency/max(Total_Frequency))*8, color = '#C0392B' ),
               type = 'scattergeo', evaluate = TRUE)

p744  %>%  layout(geo = geo, title = "B744 routing (sept. 2015), thickness = frequency")