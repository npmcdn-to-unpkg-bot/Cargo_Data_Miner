CARGOIS <- CargoIS.SQL.query(CargoDB, level = 'C2W', year = 2016, ORG = 'US')
CARGOIS[is.na(CARGOIS)] = 0

Cargo.df <- CARGOIS[,c('ORIGIN_AIRPORT_CODE','WEIGHT_CHARGES_CURR_YEAR_USD','OTHER_CHARGES_CURR_YEAR_USD',
                       'WEIGHT_CURRENT_YEAR')]
Cargo.df2 <- aggregate(x = list(Weight= Cargo.df$WEIGHT_CURRENT_YEAR, Weight.Chgs = Cargo.df$WEIGHT_CHARGES_CURR_YEAR_USD, 
                                                 Other.Chgs= Cargo.df$OTHER_CHARGES_CURR_YEAR_USD), 
                       by=list(Airport = Cargo.df$ORIGIN_AIRPORT_CODE), FUN=sum)

Cargo.df2$grs.yld <- (Cargo.df2$Weight.Chgs + Cargo.df2$Other.Chgs)/Cargo.df2$Weight
Cargo.df2$net.yld <-   Cargo.df2$Weight.Chgs/Cargo.df2$Weight
  
  
AIRPORT <- names(table(CARGOIS$ORIGIN_AIRPORT_CODE))
length(AIRPORT) # 145 airports in US

AIRPORT_LOC <- get.airport.loc()
# write.csv(AIRPORT_LOC, 'E:/DATA/airport_coords.csv')


######### get States info ######### 
apt <- read.csv(file.path(DATA,'airport-codes_with_states_us.csv'))
apt.us <- apt[apt$iso_country == 'US',]
apt.us <- apt.us[complete.cases(apt.us[,c('iata_code')]),]
AIRPORT.US <- apt.us[,c('name','iso_region','iata_code')]
AIRPORT.US$iso_region <- substr(AIRPORT.US$iso_region,start = 4,stop = 5)

######### read US airport file (which has been done properly) #########
# cn.us.df <- read.csv(file.path(DATA, 'US.airport.csv'))
states.names <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv')
# states.names <- states.names[,c('code','state')]



us.states <- aggregate(x = list(Weight= cn.us.df$Weight, Weight.Chgs = cn.us.df$Weight.Chgs, 
                                Other.Chgs = cn.us.df$Other.Chgs), 
                       by=list(States = cn.us.df$iso_region), FUN=sum)

us.states$grs.yld <- (us.states$Weight.Chgs + us.states$Other.Chgs)/us.states$Weight
us.states$net.yld <-   us.states$Weight.Chgs/us.states$Weight
us.states[is.na(us.states)] <- 0
us.states <- left_join(us.states,states.names, by = c('States'='code'))
us.states$hover <- paste(us.states$state, '<br>Gross yield:', round(us.states$grs.yld,2), '<br>Net yield:', 
                         round(us.states$net.yld,2))

l <- list(color = toRGB("white"), width = 2)

plot_ly(us.states, z = Weight, text = hover, locations = States, type = 'choropleth',
        locationmode = 'USA-states', color = Weight, colors = 'RdPu',
        marker = list(line = l), colorbar = list(title = "kg")) %>%
  layout(title = '2016 US export: Weight <br>(Hover for breakdown)', geo = g)


us.states$grs.yld[us.states$grs.yld>4] <- 4
us.states$net.yld[us.states$net.yld>4] <- 4
plot_ly(us.states, z = grs.yld, text = hover, locations = States, type = 'choropleth',
        locationmode = 'USA-states', color = grs.yld, colors = 'RdYlBu',
        marker = list(line = l), colorbar = list(title = "USD/kg")) %>%
  layout(title = '2016 US export: Gross Yield <br>(Hover for breakdown)', geo = g)

plot_ly(us.states, z = net.yld, text = hover, locations = States, type = 'choropleth',
        locationmode = 'USA-states', color = net.yld, colors = 'RdYlBu',
        marker = list(line = l), colorbar = list(title = "USD/kg")) %>%
  layout(title = '2016 US export: Net Yield <br>(Hover for breakdown)', geo = g)

#######################################################
geo <- list(
  # scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  showcountries = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

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
cn.us.df <- left_join(Cargo.df2, AIRPORT_LOC, by=c("Airport"="IATA") )
cn.us.df <- left_join(cn.us.df, AIRPORT.US, by = c('Airport'='iata_code'))
# write.csv(cn.us.df,  file = paste(OUTPUT,'US.airport.csv', sep = "/"))
# cn.us.df <- read.csv(file.path(OUTPUT, 'US.airport.csv'))


cn.us.df[is.na(cn.us.df)] = 0
cn.us.df$grs.yld[cn.us.df$grs.yld>5] <- 5
cn.us.df$hover <- paste(cn.us.df$Airport, '<br>Gross yield:', round(cn.us.df$grs.yld,2), '<br>Net yield:', 
                      round(cn.us.df$net.yld,2))

plot_ly(cn.us.df, lon = long, lat = lat, text = hover,
        marker = list(size = sqrt(Weight/50000) + 8), color = grs.yld, colors = 'RdYlBu',
        type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'USA airports', geo = g)

