# evolution of the monthly cargo (unit: kt)
mon_cargo_evolution = function(cargo,year){
  database = cargo[cargo$SALES_YEAR == year,]
  x = data.frame(Months=factor(database$SALES_MONTH), Weight = database$WEIGHT_CURRENT_YEAR/1000000)
  evo = aggregate(x$Weight, by=list(Months=x$Months), FUN=sum)
  return(evo)
}


mon_FTK_evolution = function(cargo,year){
  database = cargo[cargo$SALES_YEAR == year,]
  x = data.frame(Months=factor(database$SALES_MONTH), FTK = (database$WEIGHT_CURRENT_YEAR*database$GREATCIRCLEDISTANCE_KM)/10^12)
  evo = aggregate(x$FTK, by=list(Months=x$Months), FUN=sum)
  return(evo)
}

# evolution of the  **NET** yield 
# year = 2013
net_yield_evolution = function(cargo,year){
  database = cargo[cargo$SALES_YEAR == year,]
  
  wgt_chg = data.frame(Months=factor(database$SALES_MONTH), wgt_charge = database$WEIGHT_CHARGES_CURR_YEAR_USD)
  wgt = data.frame(Months=factor(database$SALES_MONTH), weight = database$WEIGHT_CURRENT_YEAR)
  
  evo_wgt_chg = aggregate(wgt_chg$wgt_charge, by=list(Months=wgt_chg$Months), FUN=sum)
  evo_wgt = aggregate(wgt$weight, by=list(Months=wgt$Months), FUN=sum)
  
  evo_net_yld = data.frame(Months = evo_wgt_chg[,1], evo_net_yield = evo_wgt_chg[,2]/evo_wgt[,2] )
  
  return(evo_net_yld)
}


# evolution of the  **GROSS** yield 
gross_yield_evolution = function(cargo,year){
  database = cargo[cargo$SALES_YEAR == year,]
  
  tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = (database$WEIGHT_CHARGES_CURR_YEAR_USD + database$OTHER_CHARGES_CURR_YEAR_USD))
  wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
  
  evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
  evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
  
  evo_gross_yld = data.frame(Months = evo_tt_chg[,1], evo_gross_yield = evo_tt_chg[,2]/evo_wgt[,2])
  
  return(evo_gross_yld)
}





