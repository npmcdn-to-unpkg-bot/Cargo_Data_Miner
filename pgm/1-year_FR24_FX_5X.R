options(stringsAsFactors = FALSE)
FR24 <- read.csv(file.path(DATA,'FR24_2yo_FX_5X.csv'),header = TRUE)
head(FR24)
FR24$arrival_date_time[FR24$arrival_date_time == ''] <- NA

install.packages("leaflet")
library(leaflet)

# FR24 <- data.frame(FR24, stringsAsFactors = FALSE)
# FR24 <- FR24[,-c(9,10,11)]

Missing_ORG <- table(is.na(FR24$departure_airport_code))[2]*100/nrow(FR24) # missing value 0.86%
Missing_DST <- table(is.na(FR24$arrival_airport_code))[2]*100/nrow(FR24) 

print(paste('There are', round(Missing_ORG,2),'% missing origin and', round(Missing_DST,2),'% missing destination'))

avail.data <-table(complete.cases(FR24))
avail.data_rate <- avail.data[2]*100/nrow(FR24)
print(paste('There are', round(avail.data_rate,2),'% of data which are complete'))


FR24 <- FR24[complete.cases(FR24),]
FEDEX <- subset(FR24, substr(FR24$flight_number,1,2) == 'FX')

##########################
# UPS data analysis
##########################
UPS <- subset(FR24, substr(FR24$flight_number,1,2) == '5X')
UPS <- subset(UPS, substr(UPS$aircraft_registration,1,1) == 'N')
UPS <- subset(UPS, substr(UPS$aircraft_registration,5,6) == 'UP')
UPS <- UPS[order(UPS$departure_date_time),]

# write.csv(UPS,file.path(DATA,'UPS_2yo.csv'))
UPS <- read.csv(file.path(DATA,'UPS_2yo.csv'))
names(UPS)[2] <- "flight_id"
UPS <- UPS[,-1]
# A/C type of UPS
ac_5x <- names(table(UPS$equipment))
length(table(UPS$aircraft_registration)) #237 AC



path <- data.frame('From' = UPS$departure_airport_code, 'To' = UPS$arrival_airport_code, 
                   'freq' = 1, 'AC_type' = UPS$equipment)

Freq.5X_AC <- aggregate(x = list('Total_Frequency' = path$freq), by=list('From' = path$From, 'To' = path$To, 'AC' = path$AC_type), FUN = sum)
Freq.5X <- aggregate(x = list('Total_Frequency' = path$freq), by=list('From' = path$From, 'To' = path$To), FUN = sum)

Freq.5X <- Freq.5X[order(Freq.5X$Total_Frequency,decreasing = TRUE),]
Freq.5X_AC <- Freq.5X_AC[order(Freq.5X_AC$Total_Frequency,decreasing = TRUE),]


###### create plot
ac <- 'MD11'
p <- plot_ly(Freq.5X_AC[Freq.5X_AC$AC == ac,],
  x = c(paste(From,To,sep = '-')),
  y = Total_Frequency,
  name = "Freq",
  type = "bar")%>% 
  layout(title = paste('UPS', ac,'aircraft fleet flight estimation in 2014-2015'),
         scene = list(
           xaxis = list(title = "O&D"), 
           yaxis = list(title = "Frequency")
           ))
p

## plot
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