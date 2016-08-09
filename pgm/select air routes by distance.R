######### distance of airport #########
script.dist = "
SELECT       
org.COUNTRY_NAME AS ORI_COUNTRY,
dst.COUNTRY_NAME AS DSR_COUNTRY,
dst.REGION_CODE



From A269_CARGOIS.CARGOIS_MONTHLYDATA data
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT org ON data.ORIGIN_AIRPORT_CODE = org.AIRPORT_CODE
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT dst ON data.DESTINATION_AIRPORT_CODE = dst.AIRPORT_CODE 
WHERE data.SALES_YEAR = 2015

"
# AND data.GREATCIRCLEDISTANCE_KM > 3750
# AND data.GREATCIRCLEDISTANCE_KM < 5500



dataS = dbSendQuery(CargoDB, script.dist)
data= fetch(dataS)

data <- data.frame(data, stringsAsFactors = FALSE)
data <- data[complete.cases(data$GREATCIRCLEDISTANCE_KM),]

data$nm <- data$GREATCIRCLEDISTANCE_KM/1.852
ggplot(data, aes(nm, weight = AWB_COUNT_CURRENT_YEAR)) + 
  geom_histogram() +
  theme_bw()


five <- subset(data, data$nm < 5500)
five <- subset(five, five$nm > 4500)
list.five <- table(paste(five$ORI_COUNTRY, five$DSR_COUNTRY, sep = ' to '))
sort(list.five, decreasing = TRUE)[1:50]
names(sort(list.five, decreasing = TRUE)[1:50])

list.five <- table(paste(five$ORIGIN_AIRPORT_CODE, five$DESTINATION_AIRPORT_CODE, sep = ' to '))
sort(list.five, decreasing = TRUE)[1:50]
names(sort(list.five, decreasing = TRUE)[1:50])

five <- subset(data, data$nm < 1500)
five <- subset(five, five$nm > 200)
list.five <- table(paste(five$ORI_COUNTRY, five$DSR_COUNTRY, sep = ' to '))
sort(list.five, decreasing = TRUE)[1:50]
names(sort(list.five, decreasing = TRUE)[1:50])
