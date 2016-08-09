######### Plot the top 20 air routes #########

top20 <- read.csv(file.path(DATA,'TOP20AIRROUTES.csv'),header = FALSE)
colnames(top20) <- c('ORIGIN_AIRPORT_CODE','DESTINATION_AIRPORT_CODE','WEIGHT_CURRENT_YEAR','WEIGHT_CHARGES_CURR_YEAR_USD','OTHER_CHARGES_CURR_YEAR_USD')

Plot.airport.html(airport = top20, plot.airport = FALSE)



