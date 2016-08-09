
ylim <- c(-62.039321, 87.856229)


airports <- read.table("C:/Users/QIN_XUZ/Documents/000_Original_data/Airport_data/airports.dat", 
                       header=F,  sep = ",",
                       col.names = c('ID', 'name','city','country', 'IATA', 'ICAO', 'lat','long','altitude','timezone','DST', 'Tz_DB_Timezone'))


org_country = 'FR'
des_country = 'US'


subset1 = subset(cargo_2015,cargo_2015$ORI_COUNTRY==org_country)
subset = subset(subset1,subset1$DST_COUNTRY==des_country)
# subset = cargo_2015
x = data.frame(airport = subset$pair, cnt = subset$WEIGHT_CURRENT_YEAR)

# x = data.frame(airport = cargo$pair, cnt = cargo$WEIGHT_CURRENT_YEAR)
count = aggregate(x$cnt, by = list(airport = x$airport), FUN = sum)
count$airport1 = substr(count$airport,1,3)
count$airport2 = substr(count$airport,5,7)
colnames(count)=c('pair','cnt','airport1','airport2')
# count = count[count$cnt > mean(count$cnt),]

# Color
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)

map("world", col="#191919", fill=TRUE, bg="#000000", lwd=0.05, mar = c(0,0,0,0),wrap = T)
  fsub <- count#[count$airport1 == 'CDG',]
  fsub <- fsub[order(fsub$cnt),]
  maxcnt <- max(fsub$cnt)
  
  
  # air1 <- airports[airports$IATA == fsub[1,]$airport1,]
  # 
  # map("world", col="#191919", orientation=c(90,120,180), wrap=TRUE, fill=TRUE, bg="#000000", lwd=0.05,ylim=ylim, mar = c(0,0,0,0))
  # 
  
  for (j in 1:length(fsub$cnt)) {
    air1 <- airports[airports$IATA == fsub[j,]$airport1,]
    air2 <- airports[airports$IATA == fsub[j,]$airport2,]
    
    if (nrow(air1) > 0 & nrow(air2) > 0){
        inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE,breakAtDateLine = FALSE, sp = TRUE)
        colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )

        lines(inter, col=colors[colindex], lwd=0.000001)
      points(air1[1,]$long, air1[1,]$lat, col = "#FFFFFF", cex = 0.1)
      points(air2[1,]$long, air2[1,]$lat, pch = '.', col = "#FFFFFF" , cex = 0.1)
      }
    }

  