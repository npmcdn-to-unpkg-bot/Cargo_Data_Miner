#########################################
######### ONE YEAR FR24 FX 5X ###########
#########################################

##### read the data #####
options(stringsAsFactors = FALSE)
FR24 <- read.csv(file.path(DATA,'FR24_2yo_FX_5X.csv'),header = TRUE, stringsAsFactors = FALSE)
head(FR24)
FR24$arrival_date_time[FR24$arrival_date_time == ''] <- NA

##### Descriptive analysis #####

## missing value
Missing_ORG <- table(is.na(FR24$departure_airport_code))[2]*100/nrow(FR24) # missing value 0.86%
Missing_DST <- table(is.na(FR24$arrival_airport_code))[2]*100/nrow(FR24) 
print(paste('There are', round(Missing_ORG,2),'% missing origin and', round(Missing_DST,2),'% missing destination'))

## Proportion of complete rows
avail.data <-table(complete.cases(FR24))
avail.data_rate <- avail.data[2]*100/nrow(FR24)
print(paste('There are', round(avail.data_rate,2),'% of data which are complete'))

##### cleaning data ######
FR24 <- FR24[complete.cases(FR24),] # remove rows with NA

###### FedEX data analysis ######
FEDEX <- subset(FR24, substr(FR24$flight_number,1,2) == 'FX')
# FEDEX <- subset(FEDEX, substr(FEDEX$aircraft_registration,1,1) == 'N' | substr(FEDEX$aircraft_registration,1,1) == 'C' | substr(FEDEX$aircraft_registration,1,1) == 'E' )
FEDEX <- data.frame(FEDEX, stringsAsFactors = FALSE)

gen_expr <- c('N...FX','N...FD','N...FE','N68077','N68078','N68079','N68053','N10060', 'ECK..','EIFX.','CFEX.','CFM..','CFT..')
exp <- paste(gen_expr,collapse = '|')
FEDEX <- FEDEX[grepl(exp, FEDEX$aircraft_registration),]


## order them by the departure time
FEDEX <- FEDEX[order(FEDEX$departure_date_time),]


## REG_number of FX's AC
# number of FX aircraft
length(table(FEDEX$aircraft_registration)) #should be 642 AC now 529
## create table c(aircraft_registration, Freq) and find reg_number which has freq = 1

tab <- table(FEDEX$aircraft_registration)

## A/C type of FX
ac_fx <- names(table(FEDEX$equipment))
table(FEDEX$equipment)

AC_FX_true <- c('A306','A310','AT43','AT72', 'B752',"B763", 'B77L','C208','DC10','MD11')
# error
# A30B = A306



## save and read .csv
# write.csv(FEDEX,file.path(DATA,'FEDEX_2yo.csv'))

########## READ FEDEX CSV (CLEANED) ############
FEDEX <- read.csv(file.path(DATA,'FEDEX_2yo.csv'),stringsAsFactors = FALSE)
names(FEDEX)[2] <- "flight_id"
FEDEX <- FEDEX[,-1]
FEDEX <- unique(FEDEX)

FEDEX_2015 <- FEDEX[substr(FEDEX$departure_date_time,1,4) == 2015, ]


############### data cleaning for FX ####################
# order by AC_id and departure time

########################## B77L analysis: ############################## 
# 1. select all the B77L
FX_77L <- subset(FEDEX_2015, FEDEX_2015$equipment == 'B77L')

# create route vector, {ORGDST}
route <- names(table(paste(FX_77L$departure_airport_code,FX_77L$arrival_airport_code, sep = '')))

# create data.frame ESAD:
# {route (ORGDST)}, {ESAD}
ESAD <- data.frame('route'=paste(apttest$ORG, apttest$DST, sep = ''), 'ESAD' = apttest$ESAD)

# df only for FX_B77L's air routes ESAD
FX_77L_esad <- ESAD[ESAD$route %in% route, ]

# add additional column in FX_77L: route (ORGDST)
FX_77L$route <- paste(FX_77L$departure_airport_code,FX_77L$arrival_airport_code, sep = '')

# join the ESAD to the table FX_77L
FX_77L_with_dist <- left_join(FX_77L,FX_77L_esad, by = c('route' = 'route'))
# check the tail number
reg_num <- names(table(FX_77L_with_dist$aircraft_registration))
### tail number: 25 good!

# add org_country and dst_country
airport_openflight <- read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat', header = FALSE)
names(airport_openflight) <- c('Airport_ID','Name', 'City', 'Country',
                               'IATA', 'ICAO', 'Latitude', 'Longitude',
                               'Altitude', 'Timezone', 'DST', 'Tz')
Apt_Country <- airport_openflight[,c('IATA','Country')]
Apt_Country <- Apt_Country[!Apt_Country$IATA == '',]


#### filter: international
FX_77L_with_dist <- left_join(FX_77L_with_dist, Apt_Country, by = c('departure_airport_code' = 'IATA'))
FX_77L_with_dist <- left_join(FX_77L_with_dist, Apt_Country, by = c('arrival_airport_code' = 'IATA'))

## get data of HNL
HNL_77L <- subset(FX_77L_with_dist, FX_77L_with_dist$departure_airport_code == 'HNL' | FX_77L_with_dist$arrival_airport_code =='HNL')
# # rename the table
# FX_df_final <- FX_77L_with_dist

subset1 <- subset(FX_77L_with_dist, FX_77L_with_dist$Country.x != FX_77L_with_dist$Country.y) # about 5000 flight removed

#### filter: distance > 2000km
subset2 <- subset(subset1, subset1$ESAD > 2000) #1600+ flights removed
subset2 <- rbind(subset2, HNL_77L)

subset2 <- unique(subset2)
# proportion of short range flights
prop_domestic_flight <- (nrow(FX_77L_with_dist)-nrow(subset2))/nrow(FX_77L_with_dist) # 16.29% domestic flights 


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
write.csv(tb_complete, file.path(TEMPS,'FX_B77L_tailnumber_frequency.csv'))

#######################
# frequency of air route
df2 <- data.frame('From' = subset2$departure_airport_code, 'To' = subset2$arrival_airport_code, 'freq' = 1)
freq_air_route <- aggregate(x = list('freq' = df2$freq), by=list('From' = df2$From, 'To' = df2$To), FUN = sum)
freq_air_route <- freq_air_route[order(freq_air_route$freq, decreasing = TRUE),]
# output result

write.csv(freq_air_route, file.path(TEMPS,'FX_B77L_air_routes_frequency.csv'))

subset2 <- arrange(subset2, aircraft_id, departure_date_time)
write.csv(subset2,  file.path(TEMPS,'FX_B77L.csv'))

############### add time (do this AFTER left_join)
# subset3 <- subset2
# subset3$departure_date_time <- as.POSIXct(subset3$departure_date_time, "%Y-%m-%d %H:%M:%S")
# subset3$arrival_date_time <- as.POSIXct(subset3$arrival_date_time, "%Y-%m-%d %H:%M:%S")
# 
# FX_77L_with_dist <- arrange(FX_77L_with_dist, aircraft_id, departure_date_time)

#-------------------------------------------------------------------------------
#                                      PLOTS
#-------------------------------------------------------------------------------
# AC freq



########################## MD11 analysis: ##############################
# 1. select all the B77L
FX_MD11 <- subset(FEDEX_2015, FEDEX_2015$equipment == 'MD11')

# create route vector, {ORGDST}
route_md11 <- names(table(paste(FX_MD11$departure_airport_code,FX_MD11$arrival_airport_code, sep = '')))

# create data.frame ESAD:
# {route (ORGDST)}, {ESAD}
ESAD <- data.frame('route'=paste(apttest$ORG, apttest$DST, sep = ''), 'ESAD' = apttest$ESAD)

# df only for FX_B77L's air routes ESAD
FX_MD11_esad <- ESAD[ESAD$route %in% route_md11, ]

# add additional column in FX_MD11: route (ORGDST)
FX_MD11$route <- paste(FX_MD11$departure_airport_code,FX_MD11$arrival_airport_code, sep = '')

# join the ESAD to the table FX_77L
FX_MD11_with_dist <- left_join(FX_MD11,FX_MD11_esad, by = c('route' = 'route'))
# check the tail number
reg_num_MD11 <- names(table(FX_MD11_with_dist2$aircraft_registration)) # there are some wrong aircraft in the dataset
### remove AC with wrong tail number
gen_expr_MD11 <- c('N52.FE','N57.FE','N58.FE','N59.FE','N60.FE','N61.FE','N62.FE','N64.FE')
exp_MD11 <- paste(gen_expr_MD11,collapse = '|')

FX_MD11_with_dist2 <- FX_MD11_with_dist[grepl(exp_MD11, FX_MD11_with_dist$aircraft_registration),]



# add org_country and dst_country
airport_openflight <- read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat', header = FALSE)
names(airport_openflight) <- c('Airport_ID','Name', 'City', 'Country',
                               'IATA', 'ICAO', 'Latitude', 'Longitude',
                               'Altitude', 'Timezone', 'DST', 'Tz')
Apt_Country <- airport_openflight[,c('IATA','Country')]
Apt_Country <- Apt_Country[!Apt_Country$IATA == '',]


#### filter: international
FX_MD11_with_dist2 <- left_join(FX_MD11_with_dist2, Apt_Country, by = c('departure_airport_code' = 'IATA'))
FX_MD11_with_dist2 <- left_join(FX_MD11_with_dist2, Apt_Country, by = c('arrival_airport_code' = 'IATA'))

HNL_MD11 <- subset(FX_MD11_with_dist2, FX_MD11_with_dist2$departure_airport_code == 'HNL' | FX_MD11_with_dist2$arrival_airport_code =='HNL')

# # rename the table
# FX_df_final <- FX_77L_with_dist

subset_md1 <- subset(FX_MD11_with_dist2, FX_MD11_with_dist2$Country.x != FX_MD11_with_dist2$Country.y) # about 5000 flight removed

#### filter: distance > 2000km
subset_md2 <- subset(subset_md1, subset_md1$ESAD > 2000) #1600+ flights removed
subset_md2 <- rbind(subset_md2, HNL_MD11)
subset_md2 <- unique(subset_md2)
reg_num_MD11_filted <- names(table(subset_md2$aircraft_registration))
# proportion of short range flights
prop_domestic_flight_md11 <- (nrow(subset_md1)-nrow(subset_md2))/nrow(subset_md1) # 36.4% domestic flights 


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
write.csv(tb_complete_md11, file.path(TEMPS,'FX_MD11_tailnumber_frequency.csv'))

#######################
# frequency of air route
df2 <- data.frame('From' = subset_md2$departure_airport_code, 'To' = subset_md2$arrival_airport_code, 'freq' = 1)
freq_air_route_md11 <- aggregate(x = list('freq' = df2$freq), by=list('From' = df2$From, 'To' = df2$To), FUN = sum)
freq_air_route_md11 <- freq_air_route_md11[order(freq_air_route_md11$freq, decreasing = TRUE),]
# output result
subset_md2 <- arrange(subset_md2, aircraft_id, departure_date_time)
write.csv(subset_md2, file.path(TEMPS,'FX_MD11.csv'))

write.csv(freq_air_route_md11, file.path(TEMPS,'FX_MD11_air_routes_frequency.csv'))


############### plot ############### 
## create table *path* with routing, frequency and A/C type
pathFX <- data.frame('From' = FEDEX$departure_airport_code, 'To' = FEDEX$arrival_airport_code, 
                   'freq' = 1, 'AC_type' = FEDEX$equipment)

## create frequency table with AC type
Freq.FX_AC <- aggregate(x = list('Total_Frequency' = pathFX$freq), 
                        by=list('From' = pathFX$From, 'To' = pathFX$To, 'AC' = pathFX$AC_type), FUN = sum)
Freq.FX_AC <- Freq.FX_AC[order(Freq.FX_AC$Total_Frequency,decreasing = TRUE),]

## create frequency table without AC type
Freq.FX <- aggregate(x = list('Total_Frequency' = pathFX$freq), 
                     by=list('From' = pathFX$From, 'To' = pathFX$To), FUN = sum)
Freq.FX <- Freq.FX[order(Freq.FX$Total_Frequency,decreasing = TRUE),]

## stacked barplot 
plot_ly(Freq.FX_AC,group = AC,
        x = c(paste(From,To,sep = '-')),
        y = Total_Frequency,
        name = AC,
        type = "bar") %>%
  layout(p, title = paste('FedEx aircraft fleet estimation in 2014-2015'),
         scene = list(
           xaxis = list(title = "O&D"), 
           yaxis = list(title = "Frequency")
         ),  barmode = "stack")


## plot routing map

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

### get airports locations
airport_locations <- read.csv(file.path(DATA,'bio.airports.csv'), header = TRUE)
airport_locations <- airport_locations[,c('IATA','LONGITUDE', 'LATITUDE')]
names(airport_locations) <- c('IATA', 'long', 'lat')

### create
OD_B77F <- left_join(freq_air_route, airport_locations, by=c("From"="IATA") )
OD_B77F <- left_join(OD_B77F, airport_locations, by=c("To"="IATA") )

OD_MD11 <- left_join(freq_air_route_md11, airport_locations, by=c("From"="IATA") )
OD_MD11 <- left_join(OD_MD11, airport_locations, by=c("To"="IATA") )

### MD11 mapping
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


pmd11  %>%  layout(geo = geo, title = "FedEx MD11 air routes (2015), thickness = frequency")
write.csv(ODFX[ODFX$AC == 'MD11', ], file.path(TEMPS,'FX_MD11_routing.csv'))

###B77X mapping
# dataFX <- ODFX[ODFX$AC == 'B77L', ][1:100,]
pb744 <- plot_ly(OD_B77F, lon = c(long.x,long.y), lat = c(lat.x, lat.y), text = c(From, To),
                 type = 'scattergeo',
                 mode = "markers+text", textposition  = 'top middle', group = 'Airports',
                 marker = list(size = 6, color = 'grey'),
                 inherit = FALSE)

pb744 <- add_trace(lon = list(long.x, long.y), lat = list(lat.x, lat.y),
                   group = c(paste(From, To, sep = '-')), data = OD_B77F, opacity = 0.5,
                   mode = 'lines', line = list(width =(freq/max(freq))*8, color = '#C0392B' ),
                   type = 'scattergeo', evaluate = TRUE)


pb744  %>%  layout(geo = geo, title = "FedEx B77F air routes (2015), thickness = frequency")
write.csv(ODFX[ODFX$AC == 'B77L', ], file.path(TEMPS,'FX_B77X_routing.csv'))
############### ############### 

##### daily take off ######
# subset 2
df <- data.frame('Date' = as.Date(substr(subset2$departure_date_time,1,10)), 'freq' = 1)
B77L_take_off <- aggregate(x = list('freq' = df$freq), by=list('Date' = df$Date), FUN = sum)

df2 <- data.frame('Date' = as.Date(substr(subset_md2$departure_date_time,1,10)), 'freq' = 1)
MD11_take_off <- aggregate(x = list('freq' = df2$freq), by=list('Date' = df2$Date), FUN = sum)

Cal <- gvisCalendar(B77L_take_off, 
                    datevar="Date", 
                    numvar="freq",
                    options=list(
                      title="Daily take off FedEx B77F",
                      height=300,
                      width = 1000,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                      fontSize: 32, color: '#1A8763', bold: true},
                      cellSize: 15,
                      cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                      focusedCellColor: {stroke:'red'}}")
                    )
plot(Cal)


#------------------------------------------
##### find out trans-oceanic flights ######
#------------------------------------------

ct_rg_match <- read.csv(file.path(DATA,'country_region_match.csv'), stringsAsFactors = FALSE)

ct_rg_match[ct_rg_match$COUNTRY_NAME == 'Korea, Republic of',1] <- 'South Korea'
ct_rg_match[ct_rg_match$COUNTRY_NAME == 'Chinese Taipei',1] <- 'Taiwan'         # change the name in Cargo IS
                                                                                # replace it by the name of FR24

match_B77F <- left_join(subset2,ct_rg_match, by = c('Country.x' = 'COUNTRY_NAME'))
match_B77F <- left_join(match_B77F,ct_rg_match, by = c('Country.y' = 'COUNTRY_NAME'))

match_MD11 <- left_join(subset_md2,ct_rg_match, by = c('Country.x' = 'COUNTRY_NAME'))
match_MD11 <- left_join(match_MD11,ct_rg_match, by = c('Country.y' = 'COUNTRY_NAME'))
# check NAs
match_MD11[is.na(match_MD11$REGION_NAME.x),] # 0 === good
match_MD11[is.na(match_MD11$REGION_NAME.y),] # 0 === good

write.csv(match_B77F,  file.path(TEMPS,'FX_B77F_region.csv'))
write.csv(match_MD11,  file.path(TEMPS,'FX_MD11_region.csv'))
###### trans_oceanic flights for B77F
eur_nam_B77F <- subset(match_B77F, 
                       match_B77F$REGION_NAME.x %in% c('Europe','North Atlantic & North America') & 
                         match_B77F$REGION_NAME.y %in% c('Europe','North Atlantic & North America')
                       )

nasia_nam_B77F <- subset(match_B77F, 
                       match_B77F$REGION_NAME.x %in% c('North Asia','North Atlantic & North America') & 
                         match_B77F$REGION_NAME.y %in% c('North Asia','North Atlantic & North America')
)

aspac_nam_B77F <- subset(match_B77F, 
                         match_B77F$REGION_NAME.x %in% c('Asia Pacific','North Atlantic & North America') & 
                           match_B77F$REGION_NAME.y %in% c('Asia Pacific','North Atlantic & North America')
)

trans_ocea_B77F <- unique(rbind(eur_nam_B77F,nasia_nam_B77F,aspac_nam_B77F))  # observations of trans_oceanic flights
AC_trans_ocea_B77F <- table(trans_ocea_B77F$aircraft_registration)            # get the reg_number of AC which flown 
                                                                              # trans-oceanic air routes

routes_B77F <- data.frame(trans_ocea_B77F$Country.x,trans_ocea_B77F$Country.y)
test <- data.frame(table(paste(routes_B77F$trans_ocea_B77F.Country.x, routes_B77F$trans_ocea_B77F.Country.y, sep = ' to ')))
test <- test[order(test$Freq, decreasing = TRUE),]

write.csv(data.frame(AC_trans_ocea_B77F), file.path(TEMPS, 'trans_ocea_B77F.csv'))

###### trans_oceanic flights for MD11
eur_nam_MD11 <- subset(match_MD11, 
                       REGION_NAME.x == 'Europe' & REGION_NAME.y == 'North Atlantic & North America'
)
nam_eur_MD11 <- subset(match_MD11, 
                       REGION_NAME.y == 'Europe' & REGION_NAME.x == 'North Atlantic & North America'
)
HNL_MD11 <- subset(match_MD11, 
                   match_MD11$departure_airport_code == 'HNL' | match_MD11$arrival_airport_code == 'HNL'
)

nasia_nam_MD11 <- subset(match_MD11, 
                         REGION_NAME.y == 'North Asia' & REGION_NAME.x == 'North Atlantic & North America'
)
nam_nasia_MD11 <- subset(match_MD11, 
                         REGION_NAME.x == 'North Asia' & REGION_NAME.y == 'North Atlantic & North America'
)

aspac_nam_MD11 <- subset(match_MD11, 
                         REGION_NAME.y == 'Asia Pacific' & REGION_NAME.x == 'North Atlantic & North America'
)

nam_aspac_MD11 <- subset(match_MD11, 
                         REGION_NAME.x == 'Asia Pacific' & REGION_NAME.y == 'North Atlantic & North America'
)

aspac_MD11 <- subset(match_MD11, 
                         REGION_NAME.x == 'Asia Pacific' & REGION_NAME.y == 'Asia Pacific' 
)

trans_ocea_MD11 <- unique(rbind(eur_nam_MD11,
                                nam_eur_MD11,
                                HNL_MD11,
                                nasia_nam_MD11,
                                nam_nasia_MD11,
                                aspac_nam_MD11,
                                nam_aspac_MD11,
                                aspac_MD11))  # observations of trans_oceanic flights

routes_md <- data.frame(trans_ocea_MD11$Country.x,trans_ocea_MD11$Country.y)

test <- data.frame(table(paste(routes_md$trans_ocea_MD11.Country.x, routes_md$trans_ocea_MD11.Country.y, sep = ' to ')))
test <- test[order(test$Freq, decreasing = TRUE),]

AC_trans_ocea_MD11 <- table(trans_ocea_MD11$aircraft_registration)            # get the reg_number of AC which flown 
# trans-oceanic air routes
write.csv(data.frame(AC_trans_ocea_MD11), file.path(TEMPS, 'trans_ocea_MD11.csv'))

#-------------------------------------------------------------

################# main air routes of each aircraft ###################

reg_num_MD11_filted
reg_num
match_B77F
match_MD11

# create a dataframe containing AC's monthly ESAD, total ESAD, main air routes,
tail <- reg_num[1]

B77F_monthly_ESAD <- data.frame()

for(tail in reg_num){
  df <- subset(match_B77F, match_B77F$aircraft_registration == tail)
  month_esad <- data.frame('Month' = substr(df$departure_date_time,1,7), 'ESAD' = df$ESAD)
  agg_month_esad <- aggregate(x = list('ESAD' = month_esad$ESAD), by = list('Month' = month_esad$Month), FUN = sum)
  month_esad <- data.frame(t(agg_month_esad$ESAD))
  # rownames(month_esad) <- tail
  colnames(month_esad) <- agg_month_esad$Month
  B77F_monthly_ESAD <- rbind.fill(B77F_monthly_ESAD, month_esad)
}
rownames(B77F_monthly_ESAD) <- reg_num
write.csv()


MD11_monthly_ESAD <- data.frame()

for(tail in reg_num_MD11_filted){
  df <- subset(match_MD11, match_MD11$aircraft_registration == tail)
  month_esad <- data.frame('Month' = substr(df$departure_date_time,1,7), 'ESAD' = df$ESAD)
  agg_month_esad <- aggregate(x = list('ESAD' = month_esad$ESAD), by = list('Month' = month_esad$Month), FUN = sum)
  month_esad <- data.frame(t(agg_month_esad$ESAD))
  # rownames(month_esad) <- tail
  colnames(month_esad) <- agg_month_esad$Month
  MD11_monthly_ESAD <- rbind.fill(MD11_monthly_ESAD, month_esad)
}
rownames(MD11_monthly_ESAD) <- reg_num_MD11_filted