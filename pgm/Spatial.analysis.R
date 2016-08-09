################################################################
################## Spatial analysis of the US ##################
################################################################




CARGOIS <- CargoIS.SQL.query(CargoDB, level = 'C2W', year = 2016, ORG = 'US')
CARGOIS[is.na(CARGOIS)] = 0

Cargo.df <- CARGOIS[,c('ORIGIN_AIRPORT_CODE','WEIGHT_CHARGES_CURR_YEAR_USD','OTHER_CHARGES_CURR_YEAR_USD',
                       'WEIGHT_CURRENT_YEAR')]
Cargo.df2 <- aggregate(x = list(Weight= Cargo.df$WEIGHT_CURRENT_YEAR, Weight.Chgs = Cargo.df$WEIGHT_CHARGES_CURR_YEAR_USD, 
                                Other.Chgs= Cargo.df$OTHER_CHARGES_CURR_YEAR_USD), 
                       by=list(Airport = Cargo.df$ORIGIN_AIRPORT_CODE), FUN=sum)

# Cargo.df2$grs.yld <- (Cargo.df2$Weight.Chgs + Cargo.df2$Other.Chgs)/Cargo.df2$Weight
# Cargo.df2$net.yld <-   Cargo.df2$Weight.Chgs/Cargo.df2$Weight
######### get States info ######### 
apt <- read.csv(file.path(DATA,'airport-codes_with_states_us.csv'))
apt.us <- apt[apt$iso_country == 'US',]
apt.us <- apt.us[complete.cases(apt.us[,c('iata_code')]),]
AIRPORT.US <- apt.us[,c('name','iso_region','iata_code')]
AIRPORT.US$iso_region <- substr(AIRPORT.US$iso_region,start = 4,stop = 5)

######### add region code #########
AIRPORT_LOC <- get.airport.loc()
Cargo.df2 <- left_join(Cargo.df2, AIRPORT_LOC, by=c("Airport"="IATA") )
Cargo.df2 <- left_join(Cargo.df2, AIRPORT.US, by = c('Airport'='iata_code'))

## no region code
no.reg <- Cargo.df2[is.na(Cargo.df2$iso_region),]
Cargo.df2 <- Cargo.df2[complete.cases(Cargo.df2[,c('iso_region')]),]

## write csv file for no.reg
# write.csv(no.reg, file.path(TEMPS, 'no.reg.csv'))
no.reg <- read.csv(file.path(TEMPS, 'no.reg.csv'))[,-1]
## good table 
Cargo.df3 <- rbind(Cargo.df2, no.reg)

## aggregate by iso_region
Cargo.df3 <- aggregate(x = list(Weight= Cargo.df3$Weight, 
                                Weight.Chgs = Cargo.df3$Weight.Chgs, 
                                Other.Chgs= Cargo.df3$Other.Chgs), 
                       by=list(Region = Cargo.df3$iso_region), FUN=sum)

######### add region name #########
states.names <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv')[,c(1,2)]

Cargo.df3 <- left_join(Cargo.df3, states.names, by=c("Region"="code") )

us.states <- Cargo.df3[!Cargo.df3$Weight==0,]

us.states$grs.yld <- (us.states$Weight.Chgs + us.states$Other.Chgs)/us.states$Weight
us.states$net.yld <-   us.states$Weight.Chgs/us.states$Weight
us.states[is.na(us.states)] <- 0
us.states$hover <- paste(us.states$state, '<br>Gross yield:', round(us.states$grs.yld,2), '<br>Net yield:', 
                         round(us.states$net.yld,2))


######### geo parameters for the plots #########
l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

######### Plots: weight/ny/gy ########
plot_ly(us.states, z = Weight, text = hover, locations = Region, type = 'choropleth',
        locationmode = 'USA-states', color = Weight, colors = 'RdPu',
        marker = list(line = l), colorbar = list(title = "kg")) %>%
  layout(title = '2016 US export: Weight <br>(Hover for breakdown)', geo = g)

us.states$grs.yld[us.states$grs.yld>4] <- 4
us.states$net.yld[us.states$net.yld>4] <- 4

plot_ly(us.states, z = grs.yld, text = hover, locations = Region, type = 'choropleth',
        locationmode = 'USA-states', color = grs.yld, colors = 'RdYlBu',
        marker = list(line = l), colorbar = list(title = "USD/kg")) %>%
  layout(title = '2016 US export: Gross Yield <br>(Hover for breakdown)', geo = g)

plot_ly(us.states, z = net.yld, text = hover, locations = Region, type = 'choropleth',
        locationmode = 'USA-states', color = net.yld, colors = 'RdYlBu',
        marker = list(line = l), colorbar = list(title = "USD/kg")) %>%
  layout(title = '2016 US export: Net Yield <br>(Hover for breakdown)', geo = g)


