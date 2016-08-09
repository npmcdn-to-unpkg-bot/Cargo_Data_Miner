############################
### Load Old Cargo IS DB ###
############################
source('C:/Users/QIN_XUZ/Documents/CARGOIS/Scripts/Load_Oracle_DB.R')
drv2 = dbDriver('Oracle')
host2 = 'p595dodmv01'
port2 = 1521
sid2 = 'DBUVF260 '

connect.string2 <- paste("(DESCRIPTION=",
                         "(ADDRESS=(PROTOCOL=tcp)(HOST=", host2, ")(PORT=", port2, "))",
                         "(CONNECT_DATA=(SID=", sid2, ")))", sep = "")
CargoDB.old = dbConnect(drv2, username = 'CARGOIS', password = 'dbu1_cargois', dbname = connect.string2)

dataS = dbSendQuery(CargoDB.old, 
                    "
                SELECT 
                    data2.SALES_YEAR_MONTH,
                    data2.ORIGIN_AIRPORT_CODE,
                    data2.ORI_COUNTRY,
                    data2.ORI_REGION,
                    data2.DESTINATION_AIRPORT_CODE,
                    CARGOIS_AIRPORT.COUNTRY_CODE       AS DST_COUNTRY,
                    CARGOIS_AIRPORT.REGION_CODE        AS DSR_REGION,
                    SUM(data2.WEIGHT_CHARGES_CURR_YEAR_USD) AS WEIGHT_CHARGES_CURR_YEAR_USD,
                    SUM(data2.OTHER_CHARGES_CURR_YEAR_USD) AS OTHER_CHARGES_CURR_YEAR_USD,
                    SUM(data2.WEIGHT_CURRENT_YEAR) AS WEIGHT_CURRENT_YEAR
                FROM
                    (SELECT 
                        data.SALES_YEAR_MONTH,
                        data.ORIGIN_STATION_CODE           AS ORIGIN_AIRPORT_CODE,
                        CARGOIS_AIRPORT.COUNTRY_CODE       AS ORI_COUNTRY,
                        CARGOIS_AIRPORT.REGION_CODE        AS ORI_REGION,
                        data.DESTINATION_STATION_CODE      AS DESTINATION_AIRPORT_CODE,
                        data.NET_REVENUE_CURRENT_YEAR_CURR AS WEIGHT_CHARGES_CURR_YEAR_USD,
                        data.OTHER_CHARGES_CURR_YEAR_CURR  AS OTHER_CHARGES_CURR_YEAR_USD,
                        data.WEIGHT_CURRENT_YEAR
                    FROM CARGOIS_MONTHLYDATA data
                    INNER JOIN CARGOIS_AIRPORT
                    ON data.ORIGIN_STATION_CODE = CARGOIS_AIRPORT.AIRPORT_CODE
                    ) data2
                INNER JOIN CARGOIS_AIRPORT
                ON data2.DESTINATION_AIRPORT_CODE = CARGOIS_AIRPORT.AIRPORT_CODE 
                group by 
                    data2.SALES_YEAR_MONTH, 
                    data2.ORIGIN_AIRPORT_CODE, 
                    data2.ORI_COUNTRY, 
                    data2.ORI_REGION, 
                    data2.DESTINATION_AIRPORT_CODE, 
                    CARGOIS_AIRPORT.COUNTRY_CODE,
                    CARGOIS_AIRPORT.REGION_CODE
                ORDER BY data2.SALES_YEAR_MONTH 
                    ")
####
# data.old = fetch(dataS)
saveRDS(fetch(dataS),file.path(DATA,'cargoIS_old.rds')) 
cargo.old = readRDS(file.path(DATA,'cargoIS_old.rds'))
cargo.old[is.na(cargo.old)] = 0
summary(cargo.old)

cargo.old = cargo.old[cargo.old$SALES_YEAR_MONTH < 1306,] 
cargo.old$SALES_YEAR = as.numeric(paste('20',substr(cargo.old$SALES_YEAR_MONTH,1,2), sep = ''))
cargo.old$SALES_MONTH = as.numeric(substr(cargo.old$SALES_YEAR_MONTH,3,4))
cargo.old$SALES_YEAR_MONTH = NULL

cargo.new = cargo[,c("SALES_YEAR" ,"SALES_MONTH" ,"ORIGIN_AIRPORT_CODE","DESTINATION_AIRPORT_CODE" ,
                     "WEIGHT_CHARGES_CURR_YEAR_USD", "OTHER_CHARGES_CURR_YEAR_USD",
                     "WEIGHT_CURRENT_YEAR","ORI_COUNTRY","ORI_REGION", "DST_COUNTRY",
                     "DSR_REGION")]

cargo.toto = rbind(cargo.new, cargo.old)
saveRDS(cargo.toto,file.path(DATA,'cargoIS_complete.rds')) 


mon_cargo_evolution = function(cargo, year){
  database = cargo[cargo$SALES_YEAR == year,]
  x = data.frame(Months=factor(database$SALES_MONTH), Weight = database$WEIGHT_CURRENT_YEAR/1000000)
  evo = aggregate(x$Weight, by=list(Months=x$Months), FUN=sum)
  return(evo)
}

evo_mon_cargo = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = mon_cargo_evolution(cargo.toto,i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  evo_mon_cargo = rbind(evo_mon_cargo, evo)
}
evo_mon_cargo

cargo.ts = ts(evo_mon_cargo$x, start = c(2010,1), freq = 12)
plot(cargo.ts)
