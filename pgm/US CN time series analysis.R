######## US CN time series analysis ########

body = "
    SELECT       
      data.SALES_YEAR,
      data.SALES_MONTH, 
      data.WEIGHT_CHARGES_CURR_YEAR_USD, 
      data.OTHER_CHARGES_CURR_YEAR_USD,
      data.WEIGHT_CURRENT_YEAR
    From A269_CARGOIS.CARGOIS_MONTHLYDATA data
    INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT org ON data.ORIGIN_AIRPORT_CODE = org.AIRPORT_CODE
    INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT dst ON data.DESTINATION_AIRPORT_CODE = dst.AIRPORT_CODE 
    "
scp =  paste("WHERE (data.ORIGIN_COUNTRY_CODE = 'CN' OR  data.ORIGIN_COUNTRY_CODE = 'HK') ",
             " AND dst.COUNTRY_CODE = '",'US',"'",sep = '')

script <- paste(body,scp)

dataS = dbSendQuery(CargoDB, script)
# dataS = dbSendQuery(CargoDB, body)
CARGOIS_USCN <-  fetch(dataS)

CARGOIS_USCN <- data.frame(CARGOIS_USCN)
# CARGOIS <- CargoIS.SQL.query(CargoDB, level='ALL')
CARGOIS_USCN[is.na(CARGOIS_USCN)] <- 0

# Net yield evolution
total_evolution_ny = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = net_yield_evolution(CARGOIS_USCN,i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_ny = rbind(total_evolution_ny, evo)
}

as.ts(total_evolution_ny)
plot(total_evolution_ny, type = 'l', main = 'Cargo monthly evolution, unit = kt')

# gross yield evolution
total_evolution_gy = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = gross_yield_evolution(CARGOIS_USCN,i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_gy = rbind(total_evolution_gy, evo)
}

as.ts(total_evolution_gy)
plot(total_evolution_gy, type = 'l', main = 'Cargo monthly evolution from 06/2013, unit = kt')

oilprice <- read.csv(file.path(DATA,"10-16oilprice.csv"), header=FALSE, col.names='OilPrice')

evo_yld = data.frame(Months = total_evolution_gy[,1], gy = total_evolution_gy[,2], ny = total_evolution_ny[,2], op = oilprice)

# plot the yield evolution
YldPlot =  ggplot(evo_yld, aes(Months), ylab = 'USD/kg') + 
  geom_line(aes(y = gy, colour = "Gross yield"),cex = 1.5) + 
  geom_line(aes(y =  ny, colour = "Net yield"),cex = 1.5) +
  geom_line(aes(y =  OilPrice, colour = "Oil Price ($/Gallon)")) +
  geom_line(aes(y =  nmlzd.weight, colour = "Weight(normalized)")) +
  ylab('USD/kg') + 
  xlab('Date') +
  labs(title = 'Yield Evolution (China to US)')+
  theme_bw()+
  theme(legend.position=c(0.85, 0.85))

# weight evolution
total_evolution_weight = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = mon_cargo_evolution(CARGOIS_USCN,i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_weight = rbind(total_evolution_weight, evo)
}
nmlzd.weight <- total_evolution_weight$x/total_evolution_weight$x[1]

WgtPlot = ggplot(total_evolution_weight, aes(Months))+
  geom_line(aes(y = x),color = '#64B160', cex = 1.5)+
  xlab('Time')+
  ylab('Weight (kt)')+
  labs(title = 'Weight evolution (China to US)')+
  theme_bw()
  
  
  
######### Part 2 time series #########
# variables:
# total_evolution_ny
# total_evolution_gy
# total_evolution_weight

## for the gross yield 
gy_decomp = ts(total_evolution_gy$evo_gross_yield, freq=12, start=c(2010, 1))
gy_decomp <- decompose(gy_decomp)
plot(gy_decomp$x, ylab = 'Gross yield')
plot(gy_decomp)
## for the net yield
ny_decomp = ts(total_evolution_ny$evo_net_yield, freq=12, start=c(2010, 1))
ny_decomp <- decompose(ny_decomp)
plot(ny_decomp$x, ylab = 'Net yield')
plot(ny_decomp)

## acf and pacf for gy_decomp
rm_gy <- gy_decomp$random

rm_gy <- rm_gy[complete.cases(rm_gy)]
plot(rm_gy)

par(mfrow=c(1,1))
acf(rm_gy)
pacf(rm_gy)

test <- total_evolution_gy$evo_gross_yield
plot.ts(test)
acf(test)
pacf(test)


diff <- diff(test)
diff2 <- diff(diff)
plot.ts(diff)


acf(diff, lag.max = 50)
pacf(diff, lag.max = 50)

############### 
# weight ts
rm_wgt <- total_evolution_weight$x
plot.ts(rm_wgt)

diff.wgt <- diff(rm_wgt)
plot.ts(diff.wgt)

acf(diff.wgt, lag.max = 70)
pacf(diff.wgt, lag.max = 70)
