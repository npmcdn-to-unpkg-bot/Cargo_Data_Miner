############################
### CargoIS new + old    ###
############################


###############################################

####################### computing #############################
total_evolution_ny = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = net_yield_evolution(i, cargo.total)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_ny = rbind(total_evolution_ny, evo)
}

as.ts(total_evolution_ny)
plot(total_evolution_ny, type = 'l', main = 'Cargo monthly evolution from 06/2013, unit = kt')

total_evolution_gy = data.frame(Month = NULL, x= NULL)
for (i in 2010:2016){
  evo = gross_yield_evolution(i, cargo.total)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_gy = rbind(total_evolution_gy, evo)
}

as.ts(total_evolution_gy)
plot(total_evolution_gy, type = 'l', main = 'Cargo monthly evolution from 06/2013, unit = kt')

evo_yld = data.frame(Months = total_evolution_gy[,1], gy = total_evolution_gy[,2], ny = total_evolution_ny[,2])

p1 =  ggplot(evo_yld, aes(Months), ylab = 'USD/kg') + 
  geom_line(aes(y = gy, colour = "Gross yield"),cex = 1.5) + 
  geom_line(aes(y =  ny, colour = "Net yield"),cex = 1.5) +
  ylab('USD/kg') + 
  xlab('Date') +
  labs(title = 'Yield Evolution')+
  theme(legend.position=c(0.1, 0.5))

p2 = qplot(x = Months, y = x, data = evo_mon_cargo, geom = 'line', xlab = 'Date', ylab = 'Weight (Kt)', ylim = c(500,1500), main = 'Evolution of monthly cargo')

require(gridExtra)

grid.arrange(p1,p2)
######################

####################### regional yield evolution ######################
list = names(table(cargo.total$ORI_REGION))
pal <- colorRampPalette(colors)
colors <- pal(length(list))


# For each region, plot monthly evolution of grs yld and net yld to all the other regions.
# images saved in the dictionary
for (i in 1:length(list)){
  for (j in 1:length(list)){
    
    total_evolution_gy = data.frame(Month = NULL, x= NULL)
    total_evolution_ny = data.frame(Month = NULL, x= NULL)
    
    for (year in 2010:2016){
      evo = region_gy_evo(year, cargo.total, list[i], list[j])
      evo$Months = as.Date(as.yearmon(paste(year, evo$Months, sep = '-')))
      total_evolution_gy = rbind(total_evolution_gy, evo)
      
      evo2 = region_ny_evo(year,cargo.total,  list[i], list[j])
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



# ####################### define functions #############################
# gross_yield_evolution = function(year, cargo){
#   database = cargo[cargo$SALES_YEAR == year,]
#   
#   tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = (database$WEIGHT_CHARGES_CURR_YEAR_USD + database$OTHER_CHARGES_CURR_YEAR_USD))
#   wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
#   
#   evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
#   evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
#   
#   evo_gross_yld = data.frame(Months = evo_tt_chg[,1], evo_gross_yield = evo_tt_chg[,2]/evo_wgt[,2])
#   
#   return(evo_gross_yld)
# }
# 
# net_yield_evolution = function(year, cargo){
#   database = cargo[cargo$SALES_YEAR == year,]
#   
#   wgt_chg = data.frame(Months=factor(database$SALES_MONTH), wgt_charge = database$WEIGHT_CHARGES_CURR_YEAR_USD)
#   wgt = data.frame(Months=factor(database$SALES_MONTH), weight = database$WEIGHT_CURRENT_YEAR)
#   
#   evo_wgt_chg = aggregate(wgt_chg$wgt_charge, by=list(Months=wgt_chg$Months), FUN=sum)
#   evo_wgt = aggregate(wgt$weight, by=list(Months=wgt$Months), FUN=sum)
#   
#   evo_net_yld = data.frame(Months = evo_wgt_chg[,1], evo_net_yield = evo_wgt_chg[,2]/evo_wgt[,2] )
#   
#   return(evo_net_yld)
# }
# 
# region_gy_evo = function(year, cargo, ORG, DST){
#   database = cargo[cargo$SALES_YEAR == year,]
#   database = subset(database, database$ORI_REGION == ORG)
#   database = subset(database, database$DSR_REGION == DST)
#   
#   tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = (database$WEIGHT_CHARGES_CURR_YEAR_USD + database$OTHER_CHARGES_CURR_YEAR_USD))
#   wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
#   
#   evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
#   evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
#   
#   evo_gross_yld = data.frame(Months = evo_tt_chg[,1], evo_gross_yield = evo_tt_chg[,2]/evo_wgt[,2])
#   return(evo_gross_yld)
# }
# 
# region_ny_evo = function(year, cargo, ORG, DST){
#   database = cargo[cargo$SALES_YEAR == year,]
#   database = subset(database, database$ORI_REGION == ORG)
#   database = subset(database, database$DSR_REGION == DST)
#   
#   tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = database$WEIGHT_CHARGES_CURR_YEAR_USD)
#   wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
#   
#   evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
#   evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
#   
#   evo_net_yld = data.frame(Months = evo_tt_chg[,1], evo_net_yield = evo_tt_chg[,2]/evo_wgt[,2])
#   return(evo_net_yld)
# }