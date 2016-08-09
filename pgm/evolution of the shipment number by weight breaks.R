######### evolution of the shipment number ##########
script.dist = "
SELECT   

data.SALES_YEAR,
data.SALES_MONTH,
data.AWB_COUNT_CURRENT_YEAR,
data.WEIGHT_BREAK

From A269_CARGOIS.CARGOIS_MONTHLYDATA data
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT org ON data.ORIGIN_AIRPORT_CODE = org.AIRPORT_CODE
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT dst ON data.DESTINATION_AIRPORT_CODE = dst.AIRPORT_CODE
WHERE org.REGION_CODE = 'MENA'
"

dataS = dbSendQuery(CargoDB, script.dist)
data= fetch(dataS)
data <- data.frame(data)
data <- data[complete.cases(data),]


ShipmentEvo = function(data, year){
  database = data[data$SALES_YEAR == year,]
  x = data.frame(Months=factor(database$SALES_MONTH), freq = database$AWB_COUNT_CURRENT_YEAR)
  evo = aggregate(x$freq, by=list(Months=x$Months), FUN=sum)
  return(evo)
}

# ShipmentEvo(data, 2015)
ShipmentEvo(data)

#####################
WeightBreak <- c('[0-45[','[45-100[','[100-300[', '[300-500[', '[500-1000[', '[1000+')

Toto_ShipmentEvo <- data.frame(matrix(NA, ncol = length(WeightBreak), nrow = 76))
colnames(Toto_ShipmentEvo) <- WeightBreak

for (WB in WeightBreak){
  df <- subset(data, data$WEIGHT_BREAK == WB)
  
  WB_ShipmentEvo = data.frame(Month = NULL, weight_break= NULL)
  for (i in 2010:2016){
    evo = ShipmentEvo(df, i)
    evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
    WB_ShipmentEvo = rbind(WB_ShipmentEvo, evo)
  }
  names(WB_ShipmentEvo)<- c('Months',WB)
  
  date <- WB_ShipmentEvo$Month
  rownames(Toto_ShipmentEvo) <- date
  Toto_ShipmentEvo[,WB] <- WB_ShipmentEvo[,2]
}


################ plot
ggplot(Toto_ShipmentEvo, aes(date), ylab = 'frequency') + 
  geom_line(aes(y = Toto_ShipmentEvo$`[0-45[`, colour = "0-45"),cex = 1) + 
  geom_line(aes(y =  Toto_ShipmentEvo$`[45-100[`, colour = "45-100"),cex = 1) +
  geom_line(aes(y =  Toto_ShipmentEvo$`[100-300[`, colour = "100-300"), cex = 1) +
  geom_line(aes(y =  Toto_ShipmentEvo$`[300-500[`, colour = "300-500"), cex = 1) +
  geom_line(aes(y =  Toto_ShipmentEvo$`[500-1000[`, colour = "500-1000"), cex = 1) +
  geom_line(aes(y =  Toto_ShipmentEvo$`[1000+`, colour = "1000+"), cex = 1) +
  
  ylab('frequency') + 
  xlab('Date') +
  labs(title = 'Shipments evolution of Middle East North Africa')+
  theme_bw()

