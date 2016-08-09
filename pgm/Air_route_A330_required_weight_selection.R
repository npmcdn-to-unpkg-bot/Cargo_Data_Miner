A330_weight_min= 30000*52
A330_weight_max= 60000*52*3

source(file.path(SCRIPTS, 'CargoIS.SQL.query.R'))

A330_200F = CargoIS.SQL.query.weight(CargoDB, A330_weight_min/0.15, A330_weight_max/0.05, 2015)

apt.pairs = paste(A330_200F$ORIGIN_AIRPORT_CODE, A330_200F$DESTINATION_AIRPORT_CODE, sep = '_')

good.routes = data.frame(apt.pairs, A330_200F$WEIGHT_CURRENT_YEAR,stringsAsFactors=FALSE)

good.routes.grs.yield = data.frame(apt.pairs, (A330_200F$WEIGHT_CHARGES_CURR_YEAR_USD+A330_200F$OTHER_CHARGES_CURR_YEAR_USD)/A330_200F$WEIGHT_CURRENT_YEAR)
good.routes.net.yield = data.frame(apt.pairs, A330_200F$WEIGHT_CHARGES_CURR_YEAR_USD/A330_200F$WEIGHT_CURRENT_YEAR)

x = Plot.air.route(good.routes)

x = Plot.air.route(good.routes.net.yield, 'Accent')
x = Plot.air.route(good.routes.grs.yield, 'Accent')







routes = 
## replace the missing value
good.routes[good.routes$apt.pairs =='ZJF_CAI',]$apt.pairs = 'DXB_CAI'

Plot.air.route(good.routes)



# rt_reach_min_weight = cg_route_annual[cg_route_annual$x > A330_weight_min/0.15,]
# route_reach_required_weight = rt_reach_min_weight[rt_reach_min_weight$x < A330_weight_max/0.001,]
# 
# good.routes <- route_reach_required_weight[order(route_reach_required_weight$x),] 

# pair_ =  route_reach_required_weight[80,][1]
# 
# typ_sel = cargo[cargo$pair == as.character(pair_$Air_route),]
# y = data.frame(Months=factor(typ_sel$date), Weight = typ_sel$WEIGHT_CURRENT_YEAR)
# evo = aggregate(y$Weight, by=list(Months=y$Months), FUN=sum)
# evo[,1] = as.Date(evo[,1])
# tss = as.ts(evo[,2][8:31], frequency = 4, start = c(2014,1))
# 
# 
# plot.ts(tss,  main = pair_$Air_route)
# 
# dcptss = decompose(tss)


# cargo per airport
# x = data.frame(Airport=factor(cargo_2015$ORG_AIRPORT_CODE), Weight = cargo_2015$WEIGHT_CURRENT_YEAR)
# evo = aggregate(x$Weight, by=list(Airport=x$Airport), FUN=sum)
# 
# ORG_airport = evo[order(evo$x, decreasing = T),]
# length(ORG_airport[,2])
# hist(ORG_airport[,2])
# 
# x = data.frame(Airport=factor(cargo_2015$DST_AIRPORT_CODE), Weight = cargo_2015$WEIGHT_CURRENT_YEAR)
# evo = aggregate(x$Weight, by=list(Airport=x$Airport), FUN=sum)
# DST_airport = evo[order(evo$x, decreasing = T),]
# length(DST_airport[,2])
# 
# y= rbind(ORG_airport,DST_airport)
# x = data.frame(Airport=factor(y$Airport), Weight = y$x)
# evo = aggregate(x$Weight, by=list(Airport=x$Airport), FUN=sum)
# tt_airport = evo[order(evo$x, decreasing = T),]

# cargo per air route




