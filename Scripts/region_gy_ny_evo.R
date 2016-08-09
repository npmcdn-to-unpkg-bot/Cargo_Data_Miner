# region_gy_evo

# evolution inter-regionale
region_gy_evo = function(cargo,year, ORG, DST){
  database = cargo[cargo$SALES_YEAR == year,]
  database = subset(database, database$ORI_REGION == ORG)
  database = subset(database, database$DSR_REGION == DST)
  
  tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = (database$WEIGHT_CHARGES_CURR_YEAR_USD + database$OTHER_CHARGES_CURR_YEAR_USD))
  wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
  
  evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
  evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
  
  evo_gross_yld = data.frame(Months = evo_tt_chg[,1], evo_gross_yield = evo_tt_chg[,2]/evo_wgt[,2])
  return(evo_gross_yld)
}


region_ny_evo = function(cargo,year, ORG, DST){
  database = cargo[cargo$SALES_YEAR == year,]
  database = subset(database, database$ORI_REGION == ORG)
  database = subset(database, database$DSR_REGION == DST)
  
  tt_chg = data.frame(Months=factor(database$SALES_MONTH), tt_charge = database$WEIGHT_CHARGES_CURR_YEAR_USD)
  wgt    = data.frame(Months=factor(database$SALES_MONTH), weight    = database$WEIGHT_CURRENT_YEAR)
  
  evo_tt_chg = aggregate(tt_chg$tt_charge, by=list(Months = tt_chg$Months), FUN=sum)
  evo_wgt    = aggregate(wgt$weight,        by=list(Months = wgt$Months),    FUN=sum)
  
  evo_net_yld = data.frame(Months = evo_tt_chg[,1], evo_net_yield = evo_tt_chg[,2]/evo_wgt[,2])
  return(evo_net_yld)
}


# region_gy_evo(2014, 'AFI','EUR')