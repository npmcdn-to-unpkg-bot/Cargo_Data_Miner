#################################################################
#                       DATA INPUT                              #
#################################################################
#       Xuzhou QIN              | last modif. 10/06/2016        #
#################################################################

# define environment
# mainDir = 'C:/Users/QIN_XUZ/Documents/CARGOIS'
# dir.create(mainDir, showWarnings = FALSE)
# ## create folders if not exist
# TEMPS = paste(mainDir,'Temporaire',sep = '/');dir.create(TEMPS, showWarnings = FALSE)
# OUTPUT = paste(mainDir,'output',sep = '/');dir.create(OUTPUT, showWarnings = FALSE)
# FLATFILE = paste(mainDir,'CARGOIS_raw',sep = '/');dir.create(FLATFILE, showWarnings = FALSE)
# SCRIPTS = paste(mainDir,'Scripts',sep = '/');dir.create(SCRIPTS, showWarnings = FALSE)
# ANALYSIS = paste(mainDir,'Analysis',sep = '/');dir.create(ANALYSIS, showWarnings = FALSE)
# DATA = paste(mainDir,'DATA',sep = '/');dir.create(DATA, showWarnings = FALSE)

list.of.packages <- c("ggplot2", "maps", 'rgeos', 'maptools', 'geosphere', 'ggmap','RColorBrewer',
                     "zoo", 'lubridate', "dplyr", "rworldmap", "plyr",
                     'data.table', 'ggthemes','xts','gridExtra', 'reshape2', 'devtools','DT', 'shinydashboard',
                     'treemap', 'plotly', 'RMySQL','RODBC','googleVis')

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
# CargoDB <- odbcConnect('p595dodmp01', uid = 'CARGO', pwd = 'dbu1_cargo', connection = connect.string)

library(RODBC) # connect to BIO SQL server
BIOcon <- function(){
  BIO <- try(odbcDriverConnect('driver={SQL Server};
                              server=fr0-bio-p01,10335;
                               trusted_connection=true'))
  return(BIO)
}
BIO <- BIOcon()

library(RMySQL)
# connect to FR24
FR24con <- function(){
  FR24 <- try(dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01"))
  return(FR24)
}
FR24 <- FR24con()

library(googleVis)
library(zoo)
library(ggplot2)
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
library(plyr)
library(gridExtra)
library(data.table)
library(ggthemes)
library(xts)
library(reshape2)
library(devtools)
library(treemap)
library(plotly)
library(DT)
library(RColorBrewer)


gpclibPermit()

# load functions
source(file.path(SCRIPTS,'CargoIS.SQL.query.R'))
source(file.path(SCRIPTS,'CargoIS.SQL.query.weight.R'))
source(file.path(SCRIPTS,'CargoIS.weight.evolution.R'))
source(file.path(SCRIPTS,'CargoIS.yield.evolution.R'))
source(paste(SCRIPTS,'grs_net_yield.R', sep= '/'))
source(file.path(SCRIPTS,'Plot.air.route.html.R'))
source(paste(SCRIPTS,'Plot.air.route.R', sep = '/'))
source(paste(SCRIPTS,'info_global.R', sep = '/'))
source(paste(SCRIPTS,'Plot.air.route.R', sep = '/'))
source(file.path(SCRIPTS,'Seabury.get.year.R'))
source(file.path(SCRIPTS,'Plot.airport.html.R'))
source(file.path(SCRIPTS,'region_gy_ny_evo.R'))
source(file.path(SCRIPTS,'sea.evo.R'))
source(file.path(SCRIPTS,'Seabury.SQL.evo.query.R'))
source(file.path(SCRIPTS,'Seabury.SQL.get.code_name.R'))
source(file.path(SCRIPTS,'Seabury.SQL.OD.query.R'))
source(file.path(SCRIPTS,'Yield_Cargo_evolution.R'))
source(file.path(SCRIPTS,'get.airport.loc.R'))
source(file.path(SCRIPTS,'treemap_cargois_gvis.R'))
source(file.path(SCRIPTS,'Code.finder.R'))

######################### READ CSV FLAT FILES ####################

# name = append(names(cargo_save),c('ORI_COUNTRY','ORI_REGION','DST_COUNTRY','DSR_REGION'))
# name = c('SALES_YEAR','SALES_MONTH','ORIGIN_AIRPORT_CODE','DESTINATION_AIRPORT_CODE','WEIGHT_CHARGES_CURR_YEAR_USD','OTHER_CHARGES_CURR_YEAR_USD','WEIGHT_CURRENT_YEAR','AWB_COUNT_CURRENT_YEAR','COMMISSIONS_CURRENT_YEAR_USD','DISCOUNTS_CURRENT_YEAR_USD','GREATCIRCLEDISTANCE_KM',
#          'ORI_COUNTRY','ORI_REGION','DST_COUNTRY','DSR_REGION')
# 
# 
# cargo_region = read.table('C:/Users/QIN_XUZ/Documents/CARGOIS/CARGOIS_raw/data_with_region_dist_final.csv', head = TRUE,
#                            sep = ';', fileEncoding="UTF-8")
# 
# wb = read.table('C:/Users/QIN_XUZ/Documents/CARGOIS/CARGOIS_raw/wb.csv', head = TRUE,
#                 sep = ';', fileEncoding="UTF-8")
# x = wb[,1]
# cargo_region$WEIGHT_BREAK = x
# colnames(cargo_region)
# 
# aprt_list = read.table(file.path(FLATFILE,'airport_code.csv'), head = TRUE, fileEncoding = 'UTF-8', sep = ',')
#  
# saveRDS(cargo_region,file.path(DATA,'cargoIS.rds'))   
###################################################################

# read RDS

# cargo = readRDS(file.path(DATA,'cargoIS.rds'))
# 
# 
# 
# # nb of observations each year
# table(cargo[,1])
# # 2013   2014   2015   2016 
# # 478097 816135 807325  63640 
# 
# names(cargo)
# head(cargo)
# 
# # create new variables
# cargo$Total.charges.USD = cargo$WEIGHT_CHARGES_CURR_YEAR_USD + cargo$OTHER_CHARGES_CURR_YEAR_USD
# cargo$average_gross_yield = cargo$Total.charges.USD/cargo$WEIGHT_CURRENT_YEAR
# cargo$average_net_yield = cargo$WEIGHT_CHARGES_CURR_YEAR_USD/cargo$WEIGHT_CURRENT_YEAR
# cargo$ave_weight_AWB = cargo$WEIGHT_CURRENT_YEAR/cargo$AWB_COUNT_CURRENT_YEAR
# cargo$pair = paste(cargo$ORIGIN_AIRPORT_CODE,cargo$DESTINATION_AIRPORT_CODE, sep = '_')
# cargo$date = as.Date(as.yearmon(paste(cargo$SALES_YEAR,cargo$SALES_MONTH,sep = '-')))
# cargo$FTK = cargo$WEIGHT_CURRENT_YEAR*cargo$GREATCIRCLEDISTANCE_KM/1000

# cargo[is.na(cargo)]=0
# 
# saveRDS(cargo,file.path(DATA,'cargoIS_final.rds'))
# 



# cargo = readRDS(file.path(DATA,'cargoIS_final.rds'))


# Seabury = readRDS(file.path(DATA,'Seabury_detail_G4.rds'))
############## Descriptive statistics ######################
# cargo[cargo$average_net_yield == max(cargo$average_net_yield),]

# subset for each year
