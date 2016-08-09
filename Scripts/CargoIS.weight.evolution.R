#########################################################
#             CARGO IS wEIGHT EVOLUTION                 #
#########################################################


CargoIS.weight.evolution = function(ODBC, level, ORG, DST){
  
  dataset = CargoIS.SQL.query(ODBC, level = level, ORG = ORG, DST = DST)
  dataset[is.na(dataset)] = 0
  y = data.frame(Date=factor(as.Date(paste(dataset$SALES_YEAR,dataset$SALES_MONTH,'01',sep = '-'))),
                 Weight = dataset$WEIGHT_CURRENT_YEAR)
  
  evo = aggregate( x = list(Weight.sum= y$Weight), by=list(Date=y$Date), FUN=sum)
  
  return(evo)
}



# CargoIS.weight.evolution(CargoDB, 'W2C', DST = 'TW')
