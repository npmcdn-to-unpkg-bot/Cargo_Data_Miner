#########################################################
#             CARGO IS Yield  EVOLUTION                 #
#########################################################

CargoIS.yield.evolution = function(ODBC, level, ORG, DST){
  dataset = CargoIS.SQL.query(ODBC, level = level, ORG = ORG, DST = DST)
  dataset[is.na(dataset)] = 0
  
  y = data.frame(Date=factor(as.Date(paste(dataset$SALES_YEAR,dataset$SALES_MONTH,'01',sep = '-'))),
                 Weight = dataset$WEIGHT_CURRENT_YEAR, Weight.Chgs = dataset$WEIGHT_CHARGES_CURR_YEAR_USD,
                 Other.Chgs = dataset$OTHER_CHARGES_CURR_YEAR_USD)
  
  evo = aggregate( x = list(Weight.sum= y$Weight, Weight.Chgs.sum = y$Weight.Chgs, Other.Chgs.sum = y$Other.Chg), by=list(Date=y$Date), FUN=sum)
  yld.evo = data.frame(Date = evo$Date, Grs.yield = (evo$Weight.Chgs.sum+evo$Other.Chgs.sum)/evo$Weight.sum, Net.yield = evo$Weight.Chgs.sum/evo$Weight.sum)
}

