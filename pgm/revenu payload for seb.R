######### REVENUE PAYLOAD #########


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
scp =  paste("WHERE (data.ORIGIN_AIRPORT_CODE = 'IST' OR data.ORIGIN_AIRPORT_CODE = 'SAW')
              AND (data.DESTINATION_AIRPORT_CODE = 'CDG' )
              AND data.SALES_YEAR >= 2015 ")

title1 = 'Yield Evolution (SAW+IST to CDG)'
script <- paste(body,scp)

dataS = dbSendQuery(CargoDB, script)
# dataS = dbSendQuery(CargoDB, body)
CARGOIS <-  fetch(dataS)

CARGOIS <- data.frame(CARGOIS)

CARGOIS[is.na(CARGOIS)] <- 0

# Net yield evolution

total_evolution_gy = data.frame(Month = NULL, x= NULL)
for (i in 2015:2016){
  evo = gross_yield_evolution(CARGOIS,i)
  evo$Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  total_evolution_gy = rbind(total_evolution_gy, evo)
}

total_evolution_gy <- total_evolution_gy[4:16,]
average_gy <- sum(CARGOIS$WEIGHT_CHARGES_CURR_YEAR_USD + CARGOIS$OTHER_CHARGES_CURR_YEAR_USD)/sum(CARGOIS$WEIGHT_CURRENT_YEAR)

total_evolution_gy$lm <- fitted(lm(evo_gross_yield~Months, data = total_evolution_gy))

ggplot(total_evolution_gy, aes(Months), ylab = 'USD/kg') + 
  geom_line(aes(y = evo_gross_yield, colour = "Gross yield"),cex = 1.5) + 
  geom_line(aes(y =  average_gy, colour = "Average"),cex = 1) +
  ylab('USD/kg') + 
  xlab('Date') +
  labs(title = title1)+
  theme_bw()
