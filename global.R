#-------------------------------------------------------------
# This is the global environment definition of Cargo IS Miner. You can
# run the application by clicking 'Run App' above.
#
# Xuzhou QIN, xuzhou.qin@airbus.com
#-------------------------------------------------------------


# define environment
mainDir = 'C:/Users/QIN_XUZ/Documents/CARGOIS'
dir.create(mainDir, showWarnings = FALSE)

## create folders if not exist
TEMPS = paste(mainDir,'Temporaire',sep = '/');dir.create(TEMPS, showWarnings = FALSE)
OUTPUT = paste(mainDir,'output',sep = '/');dir.create(OUTPUT, showWarnings = FALSE)
FLATFILE = paste(mainDir,'CARGOIS_raw',sep = '/');dir.create(FLATFILE, showWarnings = FALSE)
SCRIPTS = paste(mainDir,'Scripts',sep = '/');dir.create(SCRIPTS, showWarnings = FALSE)
ANALYSIS = paste(mainDir,'Analysis',sep = '/');dir.create(ANALYSIS, showWarnings = FALSE)
DATA = paste(mainDir,'DATA',sep = '/');dir.create(DATA, showWarnings = FALSE)

# _____________load environment_______________ ####
list.of.packages <- c("ggplot2", "maps", 'rgeos', 'maptools', 'geosphere', 'ggmap','RColorBrewer',
                      "zoo", 'lubridate', "dplyr", "rworldmap",
                      'data.table', 'ggthemes','xts','gridExtra', 'reshape2', 'devtools','DT', 'shinydashboard',
                      'plotly', 'RMySQL','RODBC','googleVis', 'plyr', 'leaflet','htmltools', 'stringdist')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

check.ROracle <- 'ROracle'  %in% installed.packages()[,'Package']
if(!check.ROracle){install.packages(file.path(mainDir,'ROracle_1.2-1.zip'), repos = NULL, type="source")}


library(ROracle) # connect to CargoIS oracle DB
drv = dbDriver('Oracle')
host = 'p595dodmp01'
port = 1521
sid = 'DBUPA269'
connect.string <- paste("(DESCRIPTION=",
                        "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
                        "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
# dbConnect(drv, username = 'CARGO', password = 'dbu1_cargo', dbname = connect.string)
CargoDBcon <- function(){
  CargoDB <- try(dbConnect(drv, username = 'CARGO', password = 'dbu1_cargo', dbname = connect.string))
  return(CargoDB)
}
CargoDB <- CargoDBcon()

if(class(CargoDB) == 'try-error'){
  statu_cargois <- 'Failed'
  choice.year <- 'ERROR'
  choice_year_sea <- 'ERROR'
}else{
  statu_cargois <- 'OK'
  source(file.path(SCRIPTS,'CargoIS.SQL.query.R'))
  source(file.path(SCRIPTS,'CargoIS.SQL.query.weight.R'))
  source(file.path(SCRIPTS,'CargoIS.weight.evolution.R'))
  source(file.path(SCRIPTS,'CargoIS.yield.evolution.R'))
  source(file.path(SCRIPTS,'Code.finder.R'))
  source(file.path(SCRIPTS,'sea.evo.R'))
  source(file.path(SCRIPTS,'Seabury.SQL.evo.query.R'))
  source(file.path(SCRIPTS,'Seabury.SQL.get.code_name.R'))
  source(file.path(SCRIPTS,'Seabury.SQL.OD.query.R'))
  source(file.path(SCRIPTS,'Seabury.get.year.R'))
  source(file.path(SCRIPTS,'Yield_Cargo_evolution.R'))
  
  # function to get cargois year list
  list_cargois_year <- function(CargoDB) {
    query <- "SELECT YEAR_NUM
                FROM A269_CARGOIS.CARGOIS_PERIOD_TO_LOAD"
    data  <-  dbSendQuery(CargoDB, query)
    choice_cargois_year <- fetch(data)
    year <- sort(c(2010:max(choice_cargois_year$YEAR_NUM)),decreasing = TRUE) # create vector 2010:now
    
    # list_year <- lapply(year, function(x) x) 
    # names(list_year) <- sapply(list_year,paste,collapse="")      # create a list with the proper name
    
    return(year)
  }
  choice.year <- list_cargois_year(CargoDB)
  
  # function to get seabury year list
  list_sea_year <- function(CargoDB){
    query <- "SELECT CARGO_YEAR
              FROM A269_CARGO.A269_CARGO_AVAILABLE_YEAR
    WHERE DATA_ORIGIN = 'Seabury' "
    data  <-  dbSendQuery(CargoDB, query)
    choice_sea_year <- fetch(data)
    result <- c(sort(choice_sea_year$CARGO_YEAR, decreasing = TRUE))
    return(result)
  }
  choice_year_sea <- list_sea_year(CargoDB)
}
# CargoDB <- odbcConnect('p595dodmp01', uid = 'CARGO', pwd = 'dbu1_cargo', connection = connect.string)

library(RODBC) # connect to BIO SQL server
BIOcon <- function(){
  BIO <- try(odbcDriverConnect('driver={SQL Server};
                               server=fr0-bio-p01,10335;
                               trusted_connection=true'))
  return(BIO)
}
BIO <- BIOcon()

if(class(BIO) == 'RODBC'){
  statu_bio <- 'OK'
  source(file.path(SCRIPTS,'get.airport.loc.R'))
}else{
  statu_bio <- 'Failed'
}

library(RMySQL)
# connect to FR24
FR24con <- function(){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons){
    dbDisconnect(con)
  }
  FR24 <- try(dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01"))
  return(FR24)
}
FR24 <- FR24con()

if(class(FR24) == 'try-error'){
  statu_fr24 <- 'Failed'
}else{
  statu_fr24 <- 'OK'
  source(file.path(SCRIPTS,'Connecxion MySQL.R'))
  
}

library(googleVis)
library(zoo)
library(ggplot2)
library(plyr)
# library(parallel)
library(lubridate)
library(shinydashboard)
library(maps)
library(rgeos)
library(maptools)
library(geosphere)
library(ggmap)
library(dplyr)
library(rworldmap)

library(stringdist)

library(leaflet)
library(htmltools)

library(gridExtra)
library(data.table)
library(ggthemes)
library(xts)
library(reshape2)
library(devtools)
# library(treemap)
library(plotly)
library(DT)
library(RColorBrewer)


# gpclibPermit()

# load functions
# source(file.path(SCRIPTS,'CargoIS.SQL.query.R'))
# source(file.path(SCRIPTS,'CargoIS.SQL.query.weight.R'))
# source(file.path(SCRIPTS,'CargoIS.weight.evolution.R'))
# source(file.path(SCRIPTS,'CargoIS.yield.evolution.R'))
source(paste(SCRIPTS,'grs_net_yield.R', sep= '/'))
source(file.path(SCRIPTS,'Plot.air.route.html.R'))
# source(paste(SCRIPTS,'Plot.air.route.R', sep = '/'))
source(paste(SCRIPTS,'info_global.R', sep = '/'))
# source(paste(SCRIPTS,'Plot.air.route.R', sep = '/'))
# source(file.path(SCRIPTS,'Seabury.get.year.R'))
source(file.path(SCRIPTS,'Plot.airport.html.R'))
source(file.path(SCRIPTS,'region_gy_ny_evo.R'))
# source(file.path(SCRIPTS,'sea.evo.R'))
# source(file.path(SCRIPTS,'Seabury.SQL.evo.query.R'))
# source(file.path(SCRIPTS,'Seabury.SQL.get.code_name.R'))
# source(file.path(SCRIPTS,'Seabury.SQL.OD.query.R'))
# source(file.path(SCRIPTS,'Yield_Cargo_evolution.R'))
# source(file.path(SCRIPTS,'get.airport.loc.R'))
source(file.path(SCRIPTS,'treemap_cargois_gvis.R'))
# source(file.path(SCRIPTS,'Code.finder.R'))
# source(file.path(SCRIPTS,'Connecxion MySQL.R'))



######## load lists ######## 
choice.level = list("Region-Region"= 'R2R',
                    'Country-Region' ='C2R',
                    'Region-Country'='R2C',
                    'Country-Country'= 'C2C',
                    'Country-World'='C2W',
                    'World-Country'='W2C',
                    'Region-Airport'='R2A',
                    'Airport-Region'='A2R',
                    'Airport-Airport' = 'A2A',
                    'ALL' = 'ALL')
# 
# choice.year = list('2010' = 2010,
#                    '2011' = 2011,
#                    '2012' = 2012,
#                    "2013" = 2013,
#                    '2014' = 2014,
#                    '2015' = 2015,
#                    '2016' = 2016,
#                    'ALL' = NA)



choice.projection = list("Mercator" = 'Mercator', 
                         'Orthographic' = 'orthographic')

choice.compare = list('Gross yield' =  'gross_yield', 
                      'Net yield'= 'net_yield')

choice.level.sea = list("Region-Region"= 'R2R',
                        'Country-Region' ='C2R',
                        'Region-Country'='R2C',
                        'Country-Country'= 'C2C',
                        'Country-World' = 'C2W',
                        'World-Country' = 'W2C',
                        'Region-World' = 'R2W',
                        'World-Region' = 'W2R',
                        'ALL' = 'ALL')

choice.level.sea.GX = list("G1"= 'G1NAME',
                       'G2' ='G2NAME',
                       'G3'='G3NAME',
                       'G4'= 'G4NAME',
                       'NA'= '<NA>')

choice.level.sea.size = list('Air weight' = 'AIRWEIGHT',
                             'Air value' = 'AIRVALUE',
                             'Surface weight' = 'SFCWEIGHT',
                             'Surface value' = 'SFCVALUE')

choice.color = list('Red' = '#E74C3C',
                    'Green' = '#52BE80',
                    'Orange' = '#EB984E',
                    'Grey' = '#2C3E50',
                    'Blue' = '#3498DB')

choice.size = list( '8' = 8,
                    '1' = 1,
                    '2' = 2,
                    '3' = 3,
                    '4' = 4,
                    '5' = 5,
                    '10'= 10)

choice.sfc.air = list( 'Air' = 'AIRWEIGHT_AIRVALUE',
                       'Surface' = 'SFCWEIGHT_SFCVALUE')

choice.level.sea.GX.level= list("G1"= 'G1',
                           'G2' ='G2',
                           'G3'='G3',
                           'G4'= 'G4',
                           'ALL'= '<NA>')

choice.pt.size.top.apt = list('small'= 1000000,
                              'medium' = 500000,
                              'big' = 100000,
                              'very big' = 50000,
                              'huge' = 5000)

choice.codefinder <- list('Airport'='airport',
                          'Country'='country',
                          'Region (CargoIS)' = 'region(cargois)',
                          'Airline'='airline',
                          'GXCODE (Seabury trade)' = 'gxcode'
                          )



# ______________FUNCTIONS______________ ####


# tmLocate <- function(coor, tmSave) {    
#   tm <- tmSave$tm
#   
#   # retrieve selected rectangle
#   rectInd <- which(tm$x0 < coor[1] &
#                      (tm$x0 + tm$w) > coor[1] &
#                      tm$y0 < coor[2] &
#                      (tm$y0 + tm$h) > coor[2])
#   
#   return(tm[rectInd[1], ])
# }

# Function to create the data table for the plot of {AWB weight evolution}
ShipmentEvo = function(data){
  data$yearmonth <- paste(data$SALES_YEAR,data$SALES_MONTH,'1', sep = '-') # create new column = YEAR+MONTH
  x = data.frame(Months=data$yearmonth, wb=data$WEIGHT_BREAK,freq = data$AWB_COUNT_CURRENT_YEAR, stringsAsFactors = FALSE)
  evo = aggregate(x$freq, by=list(Months=x$Months, weight_break = x$wb), FUN=sum)
  evo$Months <- as.POSIXct(evo$Months)
  evo <- evo[order(evo$Months),]
  return(evo)
  # WeightBreak <- c('[0-45[','[45-100[','[100-300[', '[300-500[', '[500-1000[', '[1000+') #define weight breaks
  # date <- sort(unique(evo$Months))
  # # create dataframe
  # df <- data.frame()
  # 
  # for(wb in WeightBreak){
  #   row <- data.frame(t(subset(evo, evo$weight_break == wb)$x)) # create new row of the evo of 
  #                                                               # a weightbreake
  #   colnames(row) <- substr(date,1,10)
  #   # row.names(row) <- wb
  #   df <- rbind.fill(df, row)
  # }
  # 
  # 
  # df <- data.frame(t(df))
  # colnames(df) <- WeightBreak
  # return(df)
}

# Function to create treemap for SEABURY GXCODE
treemap_dt_sea_gvis <- function(data, AIR = TRUE){
  data[is.na(data)] <- 0
  if(AIR){
    G4 <- aggregate(x = list('WEIGHT' = data$AIRWEIGHT,
                             'VALUE' = data$AIRVALUE), 
                    by = list('NAME'= data$G4NAME, 'PARENT'=data$G3NAME), FUN = sum)
    
    G3 <- aggregate(x = list('WEIGHT' = data$AIRWEIGHT,
                             'VALUE' = data$AIRVALUE), 
                    by = list('NAME'= data$G3NAME, 'PARENT'=data$G2NAME), FUN = sum)
    
    G2 <- aggregate(x = list('WEIGHT' = data$AIRWEIGHT,
                             'VALUE' = data$AIRVALUE), 
                    by = list('NAME'= data$G2NAME, 'PARENT'=data$G1NAME), FUN = sum)
    
    G1 <- aggregate(x = list('WEIGHT' = data$AIRWEIGHT,
                             'VALUE' = data$AIRVALUE), 
                    by = list('NAME'= data$G1NAME, 'PARENT'= data$G1NAME), FUN = sum)
    G1$PARENT <- 'All commodities'
    
  } else {
    
    G4 <- aggregate(x = list('WEIGHT' = data$SFCWEIGHT,
                             'VALUE' = data$SFCVALUE), 
                    by = list('NAME'= data$G4NAME, 'PARENT'=data$G3NAME), FUN = sum)
    
    G3 <- aggregate(x = list('WEIGHT' = data$SFCWEIGHT,
                             'VALUE' = data$SFCVALUE), 
                    by = list('NAME'= data$G3NAME, 'PARENT'=data$G2NAME), FUN = sum)
    
    G2 <- aggregate(x = list('WEIGHT' = data$SFCWEIGHT,
                             'VALUE' = data$SFCVALUE), 
                    by = list('NAME'= data$G2NAME, 'PARENT'=data$G1NAME), FUN = sum)
    
    G1 <- aggregate(x = list('WEIGHT' = data$SFCWEIGHT,
                             'VALUE' = data$SFCVALUE), 
                    by = list('NAME'= data$G1NAME, 'PARENT'= data$G1NAME), FUN = sum)
    G1$PARENT <- 'All commodities'
  }
  
  tt <- rbind(G1,G2,G3,G4)
  all_comm <- data.frame('NAME'='All commodities', 'PARENT' = NA, 'WEIGHT' = sum(tt$WEIGHT), 'VALUE'=sum(tt$VALUE))
  tt <- rbind(all_comm, tt)
  # tt$NAME <- gsub("- ", "", tt$NAME)
  # tt$PARENT <- gsub("- ", "", tt$PARENT)
  tt <- tt[tt$WEIGHT != 0,]
  tt$VALUEPERKILO <- (tt$VALUE/tt$WEIGHT)
  tt$logVALUEPERKILO <- log((tt$VALUE/tt$WEIGHT)+1)
  tt$logVALUEPERKILO[is.na(tt$logVALUEPERKILO)] <- 0
  tt$VALUEPERKILO[is.na(tt$VALUEPERKILO)] <- 0
  return(tt)
}
treemap_sea_gvis <- function(data, ORG, DST, YEAR){
  title <- paste('Treemap of commodity types from ',ORG,' to ',DST,' ', YEAR, sep = '')
  gvisTreeMap(data = data, idvar = 'NAME', parentvar = 'PARENT', sizevar = 'WEIGHT',
              colorvar = 'logVALUEPERKILO',
              options=list(width=950, height=550,
                           fontSize=16,
                           minColor='#F5B041',
                           midColor='#F0F3F4',
                           maxColor='#5499C7',
                           headerHeight=20,
                           fontColor='black',
                           title= title ,
                           # highlightOnMouseOver= 'true',
                           # useWeightedAverageForAggregation= 'true',
                           showScale=TRUE))
}

# plot(treemap_sea_gvis(tt,'A','B','C')) tt1 <- tt # test
