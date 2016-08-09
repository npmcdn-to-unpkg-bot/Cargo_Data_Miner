#################################################################
#                       BASIC ANALYSIS                          #
#################################################################
#       Xuzhou QIN              | last modif. 18/04/2016        #
#################################################################

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

# load functions
source(file.path(SCRIPTS,'Data_input.R'))
cargo = readRDS(file.path(DATA,'cargoIS_final.rds'))
cargo.total = readRDS(file.path(DATA,'cargoIS_complete.rds'))

############## Descriptive statistics ######################
# cargo[cargo$average_net_yield == max(cargo$average_net_yield),]

# subset for each year
cargo_2013 = cargo[cargo[,1] == 2013,]
cargo_2014 = cargo[cargo[,1] == 2014,]
cargo_2015 = cargo[cargo[,1] == 2015,]
cargo_2016 = cargo[cargo[,1] == 2016,]

summary(cargo_2013)
cargo_2013[cargo_2013$WEIGHT_CHARGES_CURR_YEAR_USD < 0,] # nagetive charges
cargo_2014[cargo_2014$WEIGHT_CHARGES_CURR_YEAR_USD < 0,]
cargo[cargo$WEIGHT_CHARGES_CURR_YEAR_USD < 0,]

summary(cargo_2014)
summary(cargo_2015)
summary(cargo_2016)
summary(cargo)
# we have 2 165 197 rows and each row corresponds to a monthly record of 
# the weight, charges of a given route 

# the most busy org/dst airport (max times of appereance )
barplot(sort(table(cargo$ORIGIN_AIRPORT_CODE), decreasing = T)[1:15],  main = 'Origin airport')
barplot(sort(table(cargo$DESTINATION_AIRPORT_CODE), decreasing = T)[1:15],  main = 'destination airport')

# pairs of airport
pairs_of_airport = table(cargo$pair)
length(pairs_of_airport)

length(table(cargo$ORI_COUNTRY)) 
sort(table(cargo$ORI_COUNTRY),decreasing = T)

length(table(cargo$DST_COUNTRY)) 
sort(table(cargo$DST_COUNTRY),decreasing = T)

length(table(cargo$ORIGIN_AIRPORT_CODE)) # 962 ORG airport
sort(table(cargo$ORIGIN_AIRPORT_CODE),decreasing = T)

length(table(cargo$DESTINATION_AIRPORT_CODE)) #2405 des airport
sort(table(cargo$DESTINATION_AIRPORT_CODE),decreasing = T)
# we have 139 370 different pairs of airports

total_weight = sum(cargo$WEIGHT_CURRENT_YEAR)
total_weight_2013 = sum(cargo_2013$WEIGHT_CURRENT_YEAR) # 8326265434 kg but data begins from June 
total_weight_2014 = sum(cargo_2014$WEIGHT_CURRENT_YEAR) # 14919869375 kg
total_weight_2015 = sum(cargo_2015$WEIGHT_CURRENT_YEAR) # 15227332717 kg
total_weight_2016 = sum(cargo_2016$WEIGHT_CURRENT_YEAR) # 1122436700 kg

# total_weight_2014 = sum(cargo_2014[cargo_2014$ORIGIN_AIRPORT_CODE == 'HKG',]$WEIGHT_CURRENT_YEAR) + sum(cargo_2014[cargo_2014$DESTINATION_AIRPORT_CODE == 'HKG',]$WEIGHT_CURRENT_YEAR)



## ORG-DST countries
database = cargo_2014
cargo_out = data.frame(origin=factor(database$ORI_COUNTRY), cargo = (database$WEIGHT_CURRENT_YEAR))
carg_out = aggregate(cargo_out$cargo, by=list(origin=cargo_out$origin), FUN=sum)
carg_out[with(carg_out, order(-x)), ][1:20,]

cargo_in = data.frame(dst=factor(database$DST_COUNTRY), cargo = (database$WEIGHT_CURRENT_YEAR))
carg_in = aggregate(cargo_in$cargo, by=list(dst=cargo_in$dst), FUN=sum)
carg_in[with(carg_in, order(-x)), ][1:20,]

# evolution of the monthly cargo



evo_mon_cargo = data.frame(Month = NULL, x= NULL)
for (i in 2013:2016){
  evo = mon_cargo_evolution(i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  evo_mon_cargo = rbind(evo_mon_cargo, evo)
}


evo_mon_FTK = data.frame(Month = NULL, x= NULL)
for (i in 2013:2016){
  evo = mon_FTK_evolution(i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  evo_mon_FTK = rbind(evo_mon_FTK, evo)
}

plot(evo_mon_FTK, type = 'l', ylim = c(6,10), xlab = 'Date' ,ylab = 'billion FTKs' , main = 'FTK monthly evolution from 06/2013, unit = billons FTK')

x = xts( x =evo_mon_FTK$x, order.by =  evo_mon_FTK$Months )
x.ts = ts(x, freq=12, start=c(2013, 6))
decomp = decompose(x.ts)
plot(decomp)

plot(x.ts - decomp$seasonal, ylab = 'billion FTKs', main = 'Air FTKs (seasonally adjusted)')

# We have a shock at the begining of each year (reason: new year holidays? )
# each year, the highest monthly value is reached by the end of the year
#==============================================================

x = c(1:25)
as.ts(x, start=c(2011,1),end = c(2012,12),frequency = 12)
# total yield
total_yield_2013 = sum(cargo_2013$Total.charges.USD) # 18334657808 usd for last 6 months
total_yield_2014 = sum(cargo_2014$Total.charges.USD) # 32317257834 
total_yield_2015 = sum(cargo_2015$Total.charges.USD) # 27299601780

sum(cargo_2015$WEIGHT_CHARGES_CURR_YEAR_USD)
sum(cargo_2014$WEIGHT_CHARGES_CURR_YEAR_USD)
# gross_yield 
gross_yield_2013 = 
  sum(cargo_2013$Total.charges.USD)/sum(cargo_2013$WEIGHT_CURRENT_YEAR) # 2.202027 usd/kg
gross_yield_2014 = 
  sum(cargo_2014$Total.charges.USD)/sum(cargo_2014$WEIGHT_CURRENT_YEAR) # 2.166055 usd/kg
gross_yield_2015 = 
  sum(cargo_2015$Total.charges.USD)/sum(cargo_2015$WEIGHT_CURRENT_YEAR) # 1.792803 usd/kg
gross_yield_2016 = 
  sum(cargo_2016$Total.charges.USD)/sum(cargo_2016$WEIGHT_CURRENT_YEAR) # 1.599761 usd/kg

# net_yield
net_yield_2013 = sum(cargo_2013$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(cargo_2013$WEIGHT_CURRENT_YEAR)
  # 1.280043 usd/kg
net_yield_2014 = sum(cargo_2014$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(cargo_2014$WEIGHT_CURRENT_YEAR)
  # 1.271312 usd/kg
net_yield_2015 = sum(cargo_2015$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(cargo_2015$WEIGHT_CURRENT_YEAR)
  # 1.291622 usd/kg
net_yield_2016 = sum(cargo_2016$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(cargo_2016$WEIGHT_CURRENT_YEAR)
  # 1.255676 usd/kg


# evolution of the  **NET** yield 


total_evolution_ny = data.frame(Month = NULL, x= NULL)
for (i in 2013:2016){
  evo = net_yield_evolution(i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_ny = rbind(total_evolution_ny, evo)
}

as.ts(total_evolution_ny)
plot(total_evolution_ny, type = 'l', main = 'Cargo monthly evolution from 06/2013, unit = kt')


# evolution of the **GROSS* yield

total_evolution_gy = data.frame(Month = NULL, x= NULL)
for (i in 2013:2016){
  evo = gross_yield_evolution(i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_gy = rbind(total_evolution_gy, evo)
}

as.ts(total_evolution_gy)
plot(total_evolution_gy, type = 'l', main = 'Cargo monthly evolution from 06/2013, unit = kt')


evo_yld = data.frame(Months = total_evolution_gy[,1], gy = total_evolution_gy[,2], ny = total_evolution_ny[,2])

p1 =  ggplot(evo_yld, aes(Months), ylab = 'USD/kg') + 
       geom_line(aes(y = gy, colour = "Gross yield")) + 
       geom_line(aes(y =  ny, colour = "Net yield")) +
       ylab('USD/kg') + 
       xlab('Date') +
       labs(title = 'Yield Evolution')+
       theme(legend.position=c(0.1, 0.5))

p2 = qplot(x = Months, y = x, data = evo_mon_cargo, geom = 'line', xlab = 'Date', ylab = 'Weight (Kt)', ylim = c(500,1500), main = 'Evolution of monthly cargo')
 
require(gridExtra)

grid.arrange(p1,p2)
############## Global view of data ######################


# source contains -| grs_net_yield( year, ORI, DST)   -> give results of a given year for the pair of airport
#                  | output_yield( name_list, year)   -> generate a file all the result for a list of airport pairs

grs_net_yield(2015, 'AMS', 'DWC')

# cp = read.csv('H:/compute_yield/1.csv', head = F, sep = ',')
system.time(output_yield(cp, 2015))

##################  ANALYSIS BY REGION  ##########################

names(table(cargo$ORI_REGION))

## 
database = cargo_2015
cargo_out = data.frame(origin=factor(database$ORI_REGION), cargo = (database$WEIGHT_CURRENT_YEAR))
carg_out = aggregate(cargo_out$cargo, by=list(origin=cargo_out$origin), FUN=sum)

cargo_in = data.frame(origin=factor(database$DSR_REGION), cargo = (database$WEIGHT_CURRENT_YEAR))
carg_in = aggregate(cargo_in$cargo, by=list(origin=cargo_in$origin), FUN=sum)


##########################################
# load info_global function, compute TOTAL WEIGHT/ AVE WEIGHT/ GRO YLD/ NET YLD/ AVE AWB WEIGHT
# / EVO MONSUELLE
list = names(table(cargo$ORI_REGION))
output = NA
for (i in 1:length(list)){
  for (j in 1:length(list)){
    x = info_global(cargo, list[i], list[j])
    output = rbind(output, x)
  }
}
out_all = output[-1,] # monthly evo is not correct !!!!


###
output = NA
for (i in 1:length(list)){
  for (j in 1:length(list)){
    x = info_global(cargo_2013, list[i], list[j])
    output = rbind(output, x)
  }
}
out_2013 = output[-1,]
###
output = NA
for (i in 1:length(list)){
  for (j in 1:length(list)){
    x = info_global(cargo_2014, list[i], list[j])
    output = rbind(output, x)
  }
}
out_2014 = output[-1,]
###
output = NA
for (i in 1:length(list)){
  for (j in 1:length(list)){
    x = info_global(cargo_2015, list[i], list[j])
    output = rbind(output, x)
  }
}
out_2015 = output[-1,]
###
output = NA
for (i in 1:length(list)){
  for (j in 1:length(list)){
    x = info_global(cargo_2016, list[i], list[j])
    output = rbind(output, x)
  }
}
out_2016 = output[-1,]

write.csv(out_all, file = paste(OUTPUT,'outall', sep = "/"))
write.csv(out_2013, file = paste(OUTPUT,'out_2013', sep = "/"))
write.csv(out_2014, file = paste(OUTPUT,'out_2014', sep = "/"))
write.csv(out_2015, file = paste(OUTPUT,'out_2015', sep = "/"))
write.csv(out_2016, file = paste(OUTPUT,'out_2016', sep = "/"))

evo_month = out_2013[,c(1,7:13)]
evo_month[,9:20] =  out_2014[,7:18]
evo_month[,21:32] = out_2015[,7:18]
evo_month[,33] = out_2016[,7]
write.csv(evo_month, file = paste(OUTPUT,'evo_month', sep = "/"))

# write.csv(cargo[cargo$average_gross_yield > 1000,], file = 'extreme_values')

list = names(table(cargo$ORI_REGION))
pal <- colorRampPalette(colors)
colors <- pal(length(list))


# For each region, plot monthly evolution of grs yld and net yld to all the other regions.
# images saved in the dictionary
for (i in 1:length(list)){
  for (j in 1:length(list)){
    
    total_evolution_gy = data.frame(Month = NULL, x= NULL)
    total_evolution_ny = data.frame(Month = NULL, x= NULL)
    
    for (year in 2013:2016){
      evo = region_gy_evo(year, list[i], list[j])
      evo$Months = as.Date(as.yearmon(paste(year, evo$Months, sep = '-')))
      total_evolution_gy = rbind(total_evolution_gy, evo)
      
      evo2 = region_ny_evo(year, list[i], list[j])
      evo2$Months = as.Date(as.yearmon(paste(year, evo2$Months, sep = '-')))
      total_evolution_ny = rbind(total_evolution_ny, evo2)
    }
    
    png(paste(list[i], list[j],sep = '-'))
    
    plot(total_evolution_gy,ylim = c(0,5),type = 'l', col = 'red',lwd = 2.5)
    lines(total_evolution_ny, col = 'blue',lwd = 2.5)
    legend('bottomright', legend = c('Gross yield', 'Net yield'), col = c('red','blue'),lty=c(1,1))
    title(paste('Monthly yield evolution','from',list[i],'to',list[j]))
    dev.off()
  }
}


#### the busiest air routes in 2015
head(cargo_2015)
air_route_weight = data.frame(pairs = factor(cargo_2015$pair), weight = cargo_2015$WEIGHT_CURRENT_YEAR)
arw = aggregate(air_route_weight$weight, by=list(pairs=air_route_weight$pairs), FUN=sum)
arw[with(arw, order(-x)), ][1:20,]

#### the busiest inter-regional air routes in 2015
air_route_weight = data.frame(pairs = factor(paste(cargo_2015$ORI_REGION,cargo_2015$DSR_REGION)), weight = cargo_2015$WEIGHT_CURRENT_YEAR)
arw = aggregate(air_route_weight$weight, by=list(pairs=air_route_weight$pairs), FUN=sum)
arw[with(arw, order(-x)), ][1:20,]

#### the busiest inter-national air routes in 2015
air_route_weight = data.frame(pairs = factor(paste(cargo_2015$ORI_COUNTRY,cargo_2015$DST_COUNTRY)), weight = cargo_2015$WEIGHT_CURRENT_YEAR)
arw = aggregate(air_route_weight$weight, by=list(pairs=air_route_weight$pairs), FUN=sum)
arw[with(arw, order(-x)), ][1:20,]

