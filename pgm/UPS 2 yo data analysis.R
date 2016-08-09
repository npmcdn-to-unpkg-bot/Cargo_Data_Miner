###### UPS 2 yo data analysis #####

## get UPS data
UPS <- subset(FR24, substr(FR24$flight_number,1,2) == '5X')
## take all the rows with *aircraft_registration* starting with 5X' and ending with 'UP'
UPS <- subset(UPS, substr(UPS$aircraft_registration,1,1) == 'N')
UPS <- subset(UPS, substr(UPS$aircraft_registration,5,6) == 'UP')
## order them by the departure time
UPS <- UPS[order(UPS$departure_date_time),]
UPS <- unique(UPS)
UPS <- UPS[complete.cases(UPS),]
length(table(UPS$aircraft_registration))
UPS_2015 <- UPS[substr(UPS$departure_date_time,1,4) == 2015, ]

## A/C type of UPS
ac_5x <- names(table(UPS$equipment))
ac_5x_good <- c('A306', 'B744', 'B752','B763', 'MD11')

UPS_2015_good <- UPS_2015[UPS_2015$equipment %in% ac_5x_good,] # keep only the good AC type
length(table(UPS_2015_good$aircraft_registration)) #check #AC-type ->237 AC

# ________________NEW ANALYSIS____________________ ####

## create globale variables ####
apttest <- sqlQuery(BIO, 
                    'SELECT from_.IATA AS ORG,
                    to_.IATA AS DST,
                    apt.DISTANCE,
                    apt.ESAD
                    FROM BIO_NEO_PROD.bio.AIRPORT_PAIR apt
                    INNER JOIN BIO_NEO_PROD.bio.AIRPORT from_ ON apt.ORG_AIRPORT_ID = from_.AIRPORT_ID
                    INNER JOIN BIO_NEO_PROD.bio.AIRPORT to_ ON apt.DST_AIRPORT_ID = to_.AIRPORT_ID ')


# create data.frame ESAD:
# {route (ORGDST)}, {ESAD}
ESAD <- data.frame('route'=paste(apttest$ORG, apttest$DST, sep = ''), 'ESAD' = apttest$ESAD)

### get airports locations
airport_locations <- read.csv(file.path(DATA,'bio.airports.csv'), header = TRUE)
airport_locations <- airport_locations[,c('IATA','LONGITUDE', 'LATITUDE')]
names(airport_locations) <- c('IATA', 'long', 'lat')

### geo parameters
geo <- list(
  # scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  showcountries = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)
#_________________________________________________________

# B744 ANALYSIS ####
B744_5X <- subset(UPS_2015_good, equipment == 'B744')
route <- names(table(paste(B744_5X$departure_airport_code,B744_5X$arrival_airport_code, sep = '')))

# df only for B744_5X's air routes ESAD
B744_5X_esad <- ESAD[ESAD$route %in% route, ]

# add additional column in B744_5X: route (ORGDST)
B744_5X$route <- paste(B744_5X$departure_airport_code,B744_5X$arrival_airport_code, sep = '')

# join the ESAD to the table FX_77L
B744_5X_with_dist <- left_join(B744_5X,B744_5X_esad, by = c('route' = 'route'))
# check the tail number
reg_num <- names(table(B744_5X_with_dist$aircraft_registration))
### tail number: 13 good!

# add org_country and dst_country
airport_openflight <- read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat', header = FALSE)
names(airport_openflight) <- c('Airport_ID','Name', 'City', 'Country',
                               'IATA', 'ICAO', 'Latitude', 'Longitude',
                               'Altitude', 'Timezone', 'DST', 'Tz')
Apt_Country <- airport_openflight[,c('IATA','Country')]
Apt_Country <- Apt_Country[!Apt_Country$IATA == '',]

#### filter: international
B744_5X_with_dist <- left_join(B744_5X_with_dist, Apt_Country, by = c('departure_airport_code' = 'IATA'))
B744_5X_with_dist <- left_join(B744_5X_with_dist, Apt_Country, by = c('arrival_airport_code' = 'IATA'))

table(is.na(B744_5X_with_dist$Country.x))
table(is.na(B744_5X_with_dist$Country.y))

## get data of HNL
HNL_B744 <- subset(B744_5X_with_dist, B744_5X_with_dist$departure_airport_code == 'HNL' | B744_5X_with_dist$arrival_airport_code =='HNL')
# # rename the table
# FX_df_final <- FX_77L_with_dist

subset1 <- subset(B744_5X_with_dist, B744_5X_with_dist$Country.x != B744_5X_with_dist$Country.y) # about 2300 flight removed

#### filter: distance > 2000km
subset2 <- subset(subset1, subset1$ESAD > 2000) #500 flights removed
subset2 <- rbind(subset2, HNL_B744)

subset2 <- unique(subset2)
# proportion of short range flights
prop_domestic_short_flight <- (nrow(B744_5X_with_dist)-nrow(subset2))/nrow(B744_5X_with_dist) # 39.7% domestic flights 

# frequency of tail number
df <- data.frame('reg_number' = subset2$aircraft_registration, 'freq' = 1)
freq_reg_num <- aggregate(x = list('freq' = df$freq), by=list('reg' = df$reg_number), FUN = sum)

###########################
# the most frequent air routes for each AC

top_route_AC <- data.frame()
for(tail in reg_num){
  df3 <- subset(subset2, subset2$aircraft_registration == tail)
  freq_table <- data.frame('From' = df3$departure_airport_code, 'To' = df3$arrival_airport_code, 'freq' = 1)
  freq_route <- aggregate(x = list('freq' = freq_table$freq), by=list('From' = freq_table$From, 'To' = freq_table$To), FUN = sum)
  freq_route <- freq_route[order(freq_route$freq, decreasing = TRUE),]
  top20 <- paste(freq_route$From, freq_route$To, sep = '-')[1:20]
  
  col <- data.frame(top20)
  names(col) <- tail
  names_row <- paste('top', c(1:20), sep = ' ')
  rownames(col) <- names_row
  
  row <- data.frame(t(col))
  top_route_AC <- rbind(top_route_AC, row)
}

tb_complete <- cbind(freq_reg_num, top_route_AC)
# freq_reg_num <- freq_reg_num[order(freq_reg_num$freq, decreasing = TRUE),]
# output result
write.csv(tb_complete, file.path(TEMPS,'B744_5X_tailnumber_frequency.csv'))

#######################
# frequency of air route
df2 <- data.frame('From' = subset2$departure_airport_code, 'To' = subset2$arrival_airport_code, 'freq' = 1)
freq_air_route <- aggregate(x = list('freq' = df2$freq), by=list('From' = df2$From, 'To' = df2$To), FUN = sum)
freq_air_route <- freq_air_route[order(freq_air_route$freq, decreasing = TRUE),]
# output result

write.csv(freq_air_route, file.path(TEMPS,'B744_5X_air_routes_frequency.csv'))

subset2 <- arrange(subset2, aircraft_id, departure_date_time)
write.csv(subset2,  file.path(TEMPS,'B744_5X.csv'))


########################## MD11 analysis: ##############################
# 1. select all the B77L
MD11_5X <- subset(UPS_2015_good, UPS_2015_good$equipment == 'MD11')

# create route vector, {ORGDST}
route_md11 <- names(table(paste(MD11_5X$departure_airport_code,MD11_5X$arrival_airport_code, sep = '')))
# df only for FX_B77L's air routes ESAD
MD11_5X_esad <- ESAD[ESAD$route %in% route_md11, ]
# add additional column in FX_MD11: route (ORGDST)
MD11_5X$route <- paste(MD11_5X$departure_airport_code,MD11_5X$arrival_airport_code, sep = '')

# join the ESAD to the table FX_77L
MD11_5X_with_dist <- left_join(MD11_5X,MD11_5X_esad, by = c('route' = 'route'))
MD11_5X_with_dist <- MD11_5X_with_dist[MD11_5X_with_dist$aircraft_registration != 'N153UP',]
# check the tail number
reg_num_MD11 <- names(table(MD11_5X_with_dist$aircraft_registration)) # there are some wrong aircraft in the dataset

#### filter: international
MD11_5X_with_dist2 <- left_join(MD11_5X_with_dist, Apt_Country, by = c('departure_airport_code' = 'IATA'))
MD11_5X_with_dist2 <- left_join(MD11_5X_with_dist2, Apt_Country, by = c('arrival_airport_code' = 'IATA'))

HNL_MD11 <- subset(MD11_5X_with_dist2, MD11_5X_with_dist2$departure_airport_code == 'HNL' | MD11_5X_with_dist2$arrival_airport_code =='HNL')

subset_md1 <- subset(MD11_5X_with_dist2, MD11_5X_with_dist2$Country.x != MD11_5X_with_dist2$Country.y) # about 5000 flight removed

#### filter: distance > 2000km
subset_md2 <- subset(subset_md1, subset_md1$ESAD > 2000) #1600+ flights removed
prop_domestic_flight_md11 <- (nrow(MD11_5X_with_dist2)-nrow(subset_md2))/nrow(MD11_5X_with_dist2) # 11.4 short range flights 

subset_md2 <- rbind(subset_md2, HNL_MD11)
subset_md2 <- unique(subset_md2)

reg_num_MD11_filted <- names(table(subset_md2$aircraft_registration))
# proportion of short range flights

# frequency of tail number
df <- data.frame('reg_number' = subset_md2$aircraft_registration, 'freq' = 1)
freq_reg_num_md11 <- aggregate(x = list('freq' = df$freq), by=list('reg' = df$reg_number), FUN = sum)

###########################
# the most frequent air routes for each AC

top_route_AC_MD11 <- data.frame()
for(tail in reg_num_MD11_filted){
  df3 <- subset(subset_md2, subset_md2$aircraft_registration == tail)
  freq_table <- data.frame('From' = df3$departure_airport_code, 'To' = df3$arrival_airport_code, 'freq' = 1)
  freq_route <- aggregate(x = list('freq' = freq_table$freq), by=list('From' = freq_table$From, 'To' = freq_table$To), FUN = sum)
  freq_route <- freq_route[order(freq_route$freq, decreasing = TRUE),]
  top20 <- paste(freq_route$From, freq_route$To, sep = '-')[1:20]
  
  col <- data.frame(top20)
  names(col) <- tail
  names_row <- paste('top', c(1:20), sep = ' ')
  rownames(col) <- names_row
  
  row <- data.frame(t(col))
  top_route_AC_MD11 <- rbind(top_route_AC_MD11, row)
}

tb_complete_md11 <- cbind(freq_reg_num_md11, top_route_AC_MD11)
# freq_reg_num <- freq_reg_num[order(freq_reg_num$freq, decreasing = TRUE),]
# output result
write.csv(tb_complete_md11, file.path(TEMPS,'MD11_5X_tailnumber_frequency.csv'))

#######################
# frequency of air route
df2 <- data.frame('From' = subset_md2$departure_airport_code, 'To' = subset_md2$arrival_airport_code, 'freq' = 1)
freq_air_route_md11 <- aggregate(x = list('freq' = df2$freq), by=list('From' = df2$From, 'To' = df2$To), FUN = sum)
freq_air_route_md11 <- freq_air_route_md11[order(freq_air_route_md11$freq, decreasing = TRUE),]
# output result
subset_md2 <- arrange(subset_md2, aircraft_id, departure_date_time)
write.csv(subset_md2, file.path(TEMPS,'5X_MD11.csv'))

write.csv(freq_air_route_md11, file.path(TEMPS,'5X_MD11_air_routes_frequency.csv'))


# descriptive stat####
b744_esad <- B744_5X_with_dist[!is.na(B744_5X_with_dist$ESAD),]
mean(b744_esad$ESAD)
md11_esad <- MD11_5X_with_dist2[!is.na(MD11_5X_with_dist2$ESAD),]
mean(md11_esad$ESAD)

# Monthly ESAD ####
B744_monthly_ESAD <- data.frame()

for(tail in reg_num){
  df <- subset(subset2, subset2$aircraft_registration == tail)
  month_esad <- data.frame('Month' = substr(df$departure_date_time,1,7), 'ESAD' = df$ESAD)
  agg_month_esad <- aggregate(x = list('ESAD' = month_esad$ESAD), by = list('Month' = month_esad$Month), FUN = sum)
  month_esad <- data.frame(t(agg_month_esad$ESAD))
  # rownames(month_esad) <- tail
  colnames(month_esad) <- agg_month_esad$Month
  B744_monthly_ESAD <- rbind.fill(B744_monthly_ESAD, month_esad)
}
rownames(B744_monthly_ESAD) <- reg_num


MD11_monthly_ESAD <- data.frame()
subset_md2[is.na(subset_md2)] <- 0
for(tail in reg_num_MD11_filted){
  df <- subset(subset_md2, subset_md2$aircraft_registration == tail)
  month_esad <- data.frame('Month' = substr(df$departure_date_time,1,7), 'ESAD' = df$ESAD)
  agg_month_esad <- aggregate(x = list('ESAD' = month_esad$ESAD), by = list('Month' = month_esad$Month), FUN = sum)
  month_esad <- data.frame(t(agg_month_esad$ESAD))
  # rownames(month_esad) <- tail
  colnames(month_esad) <- agg_month_esad$Month
  MD11_monthly_ESAD <- rbind.fill(MD11_monthly_ESAD, month_esad)
}
rownames(MD11_monthly_ESAD) <- reg_num_MD11_filted


# _________________PLOT_________________####
### create
OD_B744 <- left_join(freq_air_route, airport_locations, by=c("From"="IATA") )
OD_B744 <- left_join(OD_B744, airport_locations, by=c("To"="IATA") )

OD_MD11 <- left_join(freq_air_route_md11, airport_locations, by=c("From"="IATA") )
OD_MD11 <- left_join(OD_MD11, airport_locations, by=c("To"="IATA") )

### MD11 mapping####
# dataFX <- ODFX[ODFX$AC == 'MD11', ][1:100,]
pmd11 <- plot_ly(OD_MD11, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(From, To),
                 type = 'scattergeo',
                 mode = "markers+text", textposition  = 'top middle', group = 'Airports',
                 marker = list(size = 6, color = 'grey'),
                 inherit = FALSE)

pmd11 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                   group = c(paste(From, To, sep = '-')), data = OD_MD11, opacity = 0.5,
                   mode = 'lines', line = list(width =(freq/max(freq))*8, color = '#2471A3' ),
                   type = 'scattergeo', evaluate = TRUE)


pmd11  %>%  layout(geo = geo, title = "UPS MD11 air routes (2015), thickness = frequency")
write.csv(ODFX[ODFX$AC == 'MD11', ], file.path(TEMPS,'FX_MD11_routing.csv'))

###B77X mapping####
# dataFX <- ODFX[ODFX$AC == 'B77L', ][1:100,]
pb744 <- plot_ly(OD_B744, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(From, To),
                 type = 'scattergeo',
                 mode = "markers+text", textposition  = 'top middle', group = 'Airports',
                 marker = list(size = 6, color = 'grey'),
                 inherit = FALSE)

pb744 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                   group = c(paste(From, To, sep = '-')), data = OD_B744, opacity = 0.5,
                   mode = 'lines', line = list(width =(freq/max(freq))*8, color = '#C0392B' ),
                   type = 'scattergeo', evaluate = TRUE)


pb744  %>%  layout(geo = geo, title = "UPS B744 air routes (2015), thickness = frequency")
write.csv(ODFX[ODFX$AC == 'B77L', ], file.path(TEMPS,'FX_B77X_routing.csv'))


##### daily take off ######
# subset 2
df <- data.frame('Date' = as.Date(substr(subset2$departure_date_time,1,10)), 'freq' = 1)
B744_take_off <- aggregate(x = list('freq' = df$freq), by=list('Date' = df$Date), FUN = sum)

df2 <- data.frame('Date' = as.Date(substr(subset_md2$departure_date_time,1,10)), 'freq' = 1)
MD11_take_off <- aggregate(x = list('freq' = df2$freq), by=list('Date' = df2$Date), FUN = sum)

Cal <- gvisCalendar(MD11_take_off, 
                    datevar="Date", 
                    numvar="freq",
                    options=list(
                      title="Daily take off UPS MD11",
                      height=300,
                      width = 1000,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                      fontSize: 32, color: '#1A8763', bold: true},
                      cellSize: 15,
                      cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                      focusedCellColor: {stroke:'red'}}")
                    )
plot(Cal)




# _________________OLD ANALYSIS DUPLICATED______________ ####
# ## create table *path* with routing, frequency and A/C type
# path <- data.frame('From' = UPS$departure_airport_code, 'To' = UPS$arrival_airport_code, 
#                    'freq' = 1, 'AC_type' = UPS$equipment)
# 
# ## create frequency table with AC type
# Freq.5X_AC <- aggregate(x = list('Total_Frequency' = path$freq), 
#                         by=list('From' = path$From, 'To' = path$To, 'AC' = path$AC_type), FUN = sum)
# Freq.5X_AC <- Freq.5X_AC[order(Freq.5X_AC$Total_Frequency,decreasing = TRUE),]
# 
# ## create frequency table without AC type
# Freq.5X <- aggregate(x = list('Total_Frequency' = path$freq), 
#                      by=list('From' = path$From, 'To' = path$To), FUN = sum)
# Freq.5X <- Freq.5X[order(Freq.5X$Total_Frequency,decreasing = TRUE),]
# 
# ## barplot for each AC-type (not prefered, see next section ##stacked barplot## )
# ac <- 'MD11'
# p <- plot_ly(Freq.5X_AC[Freq.5X_AC$AC == ac,],
#              x = c(paste(From,To,sep = '-')),
#              y = Total_Frequency,
#              name = "MD11",
#              type = "bar")
# 
# p <- plot_ly(
#   Freq.5X_AC[Freq.5X_AC$AC == 'B744',],
#   x = c(paste(From,To,sep = '-')),
#   y = Total_Frequency,
#   name = "B744",
#   type = "bar")
# 
# 
# p <- plot_ly(
#   Freq.5X_AC[Freq.5X_AC$AC == 'A306',],
#   x = c(paste(From,To,sep = '-')),
#   y = Total_Frequency,
#   name = "A306",
#   type = "bar")
# 
# p <- plot_ly(
#   Freq.5X_AC[Freq.5X_AC$AC == 'B752',],
#   x = c(paste(From,To,sep = '-')),
#   y = Total_Frequency,
#   name = "B752",
#   type = "bar")
# 
# p <- plot_ly(
#   Freq.5X_AC[Freq.5X_AC$AC == 'B763',],
#   x = c(paste(From,To,sep = '-')),
#   y = Total_Frequency,
#   name = "B763",
#   type = "bar")
# 
# ## stacked barplot 
# plot_ly(Freq.5X_AC,group = AC,
#         x = c(paste(From,To,sep = '-')),
#         y = Total_Frequency,
#         name = AC,
#         type = "bar") %>%
#   layout(p, title = paste('UPS aircraft fleet estimation in 2014-2015'),
#          scene = list(
#            xaxis = list(title = "O&D"), 
#            yaxis = list(title = "Frequency")
#          ),  barmode = "stack")
# 
# 
# ## plot routing map
# 
# ### geo parameters
# geo <- list(
#   # scope = 'world',
#   projection = list(type = 'Mercator'),
#   showland = TRUE,
#   showcountries = TRUE,
#   countrywidth = 0.5,
#   landcolor = toRGB("gray95"),
#   countrycolor = toRGB("gray80")
# )
# 
# ### get airports locations
# airport_locations <- read.csv(file.path(DATA,'bio.airports.csv'), header = TRUE)
# airport_locations <- airport_locations[,c('IATA','LONGITUDE', 'LATITUDE')]
# names(airport_locations) <- c('IATA', 'long', 'lat')
# 
# ### create
# OD <- left_join(Freq.5X_AC, airport_locations, by=c("From"="IATA") )
# OD <- left_join(OD, airport_locations, by=c("To"="IATA") )
# 
# 
# ### MD11 mapping
# pmd11 <- plot_ly(OD[OD$AC == 'MD11', ], lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(From, To),
#                  type = 'scattergeo',
#                  mode = "markers+text", textposition  = 'top middle', group = 'Airports',
#                  marker = list(size = 6, color = 'grey'),
#                  inherit = FALSE)
# 
# pmd11 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
#                    group = c(paste(From, To, sep = '-')), data = OD[OD$AC == 'MD11', ], opacity = 0.5,
#                    mode = 'lines', line = list(width =(Total_Frequency/max(Total_Frequency))*8, color = '#2471A3' ),
#                    type = 'scattergeo', evaluate = TRUE)
# 
# 
# pmd11  %>%  layout(geo = geo, title = "UPS MD11 air routes (2014-2015), thickness = frequency")
# write.csv(OD[OD$AC == 'MD11', ], file.path(TEMPS,'5X_MD11_routing.csv'))
# 
# ###B744 mapping
# pb744 <- plot_ly(OD[OD$AC == 'B744', ], lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(From, To),
#                  type = 'scattergeo',
#                  mode = "markers+text", textposition  = 'top middle', group = 'Airports',
#                  marker = list(size = 6, color = 'grey'),
#                  inherit = FALSE)
# 
# pb744 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
#                    group = c(paste(From, To, sep = '-')), data = OD[OD$AC == 'B744', ], opacity = 0.5,
#                    mode = 'lines', line = list(width =(Total_Frequency/max(Total_Frequency))*8, color = '#C0392B' ),
#                    type = 'scattergeo', evaluate = TRUE)
# 
# 
# pb744  %>%  layout(geo = geo, title = "UPS B744 air routes (2014-2015), thickness = frequency")
# 
# 
# ##### write csv #####
# 
# ## export MD11 and B744 data ##
# 
# ### 5X
# write.csv(OD[OD$AC == 'MD11', ], file.path(TEMPS,'5X_MD11_routing.csv'))
# write.csv(OD[OD$AC == 'B744', ], file.path(TEMPS,'5X_B744_routing.csv'))
# 
# 
# ## export all the data ##
# 
# ### 5X
# write.csv(Freq.5X_AC, file.path(TEMPS,'5X_routing.csv'))
# write.csv(Freq.FX_AC, file.path(TEMPS,'FX_routing.csv'))
