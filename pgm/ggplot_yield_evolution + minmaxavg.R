###### ggplot yield evolution + min/max/avg ######
# evolution of the **GROSS* yield
# 
ORG = 'CAI'
DST = 'SVO'
body = "
    SELECT       
data.SALES_YEAR,
data.SALES_MONTH, 
data.WEIGHT_CHARGES_CURR_YEAR_USD, 
data.OTHER_CHARGES_CURR_YEAR_USD,
data.WEIGHT_CURRENT_YEAR
From A269_CARGOIS.CARGOIS_MONTHLYDATA data
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT org ON data.ORIGIN_AIRPORT_CODE = org.AIRPORT_CODE
INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT dst ON data.DESTINATION_AIRPORT_CODE = dst.AIRPORT_CODE "

scp =  paste("WHERE data.ORIGIN_AIRPORT_CODE = '",ORG,
             "' AND data.DESTINATION_AIRPORT_CODE = '",DST,
             "' AND data.SALES_YEAR >= '2015' ",sep = '')

# scp = "WHERE (data.ORIGIN_AIRPORT_CODE = 'SAW' OR data.ORIGIN_AIRPORT_CODE = 'IST')
#        AND data.DESTINATION_AIRPORT_CODE = 'FRA'
#        AND data.SALES_YEAR >= 2015
#       "
# plot.title <- paste('Yield 2015 (SAW + IST to FRA)')

script = paste(body, scp, sep = '')
dataS = dbSendQuery(CargoDB, script)
DATA = fetch(dataS)
DATA[is.na(DATA)] <- 0


# df = gross_yield_evolution(DATA, 2015)
# Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
# total_evolution_gy = data.frame(Month = Months, gy= df$evo_gross_yield)
total_evolution_gy = data.frame(Month = NULL, x= NULL)
# total_evolution_ny = data.frame(Month = NULL, x= NULL)
for (i in 2015:2016){
  evo = gross_yield_evolution(DATA,i)
  evo2 <- net_yield_evolution(DATA,i)
  weight <- aggregate(x =list(weight = DATA[DATA$SALES_YEAR==i,]$WEIGHT_CURRENT_YEAR), 
                      by = list(Months = DATA[DATA$SALES_YEAR==i,]$SALES_MONTH), 
                      FUN = sum)
  
  Months = as.Date(as.yearmon(paste(i, evo$Months, sep = '-')))
  
  df <- data.frame('Months' = Months, 'total_evolution_gy' = evo$evo_gross_yield, 
                   'total_evolution_ny' = evo2$evo_net_yield, 'weight'=weight$weight)
  total_evolution_gy = rbind(total_evolution_gy, df)
}

evo_yld = data.frame(Months = total_evolution_gy[,1], 
                     gy = total_evolution_gy[,2],
                     ny = total_evolution_gy[,3],
                     weight = total_evolution_gy[,4])[4:16,]



max <- max(evo_yld$gy)
min <- min(evo_yld$gy)

max_ny <- max(evo_yld$ny)
min_ny <- min(evo_yld$ny)

avg <- sum(DATA$WEIGHT_CHARGES_CURR_YEAR_USD+DATA$OTHER_CHARGES_CURR_YEAR_USD)/sum(DATA$WEIGHT_CURRENT_YEAR)
avg_ny <- sum(DATA$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(DATA$WEIGHT_CURRENT_YEAR)

evo_yld2 = data.frame(Months = evo_yld$Months, 
                      Gross = evo_yld$gy,
                      Max_gross=max,
                      Avg_gross=avg,
                      Min_gross=min,
                      Net = evo_yld$ny,
                      Max_net=max_ny,
                      Avg_net=avg_ny,
                      Min_net=min_ny,
                      Weight = evo_yld$weight)

plot.title <- paste('Yield 2015 (',ORG, 'to', DST, ')')
plot.subtitle <- paste( 'GROSS YIELD Min: ', round(min,2), '; Avg: ', round(avg,2), '; Max: ', round(max,2),
                        '\nNET YIELD Min: ',round(min_ny,2), '; Avg: ', round(avg_ny,2), '; Max: ', round(max_ny,2),sep = '')
# 
# p1 =  ggplot(evo_yld) + 
#   geom_line(aes(x = Months, y = gy),group = 'Gross yield', size = 1.5, color = '#17A589') + 
#   geom_line(aes(x = Months, y = ny),group = 'Net yield', size = 1.5, color = '#CB4335') + 
#   
#   
#   geom_hline(aes(yintercept = max), group = 'Max gross yield',linetype = 2, color = '#1D8348')+
#   geom_hline(aes(yintercept = min), group = 'Min gross yield',linetype = 2, color = '#1D8348')+
#   geom_hline(aes(yintercept = avg), 
#              group = 'average yield',linetype = 4, color = '#45B39D')+
#   
#   geom_hline(aes(yintercept = max_ny), group = 'Max gross yield',linetype = 2, color = '#BA4A00')+
#   geom_hline(aes(yintercept = min_ny), group = 'Min gross yield',linetype = 2, color = '#BA4A00')+
#   geom_hline(aes(yintercept = avg_ny), 
#              group = 'average yield',linetype = 4, color = '#F1948A')+
#   
#   ylab('USD/kg') + 
#   # xlab('Month') +
#   # scale_y_continuous(breaks = c(0,round(min,2),round(avg,2),max,max+1))+
#   # labs(title = paste('Gross yield 2015 (',ORG, 'to', DST, ')'))+
#   ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
#   theme(legend.position=c(0.1, 0.5))+
#   theme_bw()
# 
# p2 =  ggplot(evo_yld) + 
#   geom_line(aes(x = Months, y = weight), size = 1.5, color = '#17A589')+
#   theme(legend.position=c(0.1, 0.5))+
#   theme_bw()
# 
# grid.arrange(p1, p2,  heights=c(0.6, 0.4), nrow = 2)
# 
# 
# 
# write.csv(collect, 'C:/Users/QIN_XUZ/Desktop/FR24')


Line2 <- gvisLineChart(evo_yld2, "Months", c("Gross",
                                             "Net",                                           
                                             "Weight",
                                             'Max_gross',
                                             "Avg_gross",
                                             "Min_gross",
                                            
                                             'Max_net',
                                             'Avg_net',
                                             'Min_net'),
                       options=list(width=1600,height=600,legend = "bottom",
                                    # chart="[
                                    # title:'title'
                                    # ]",
                                    title=paste(plot.title,'\n', plot.subtitle,sep = ''),
                                    # subtitle=plot.subtitle,
                         series="[
{color:'#db3236',targetAxisIndex: 0,lineWidth: 3},
{color: '#f4c20d',targetAxisIndex: 0,lineWidth: 3},
{color: '#4885ed',targetAxisIndex:1,lineDashStyle: [2,2,20,2,20,2],lineWidth: 3},

{color:'#db3236',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]},
{color:'#db3236',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]},
{color:'#db3236',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]},
                                  

{color: '#f4c20d',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]},
{color: '#f4c20d',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]},
{color: '#f4c20d',targetAxisIndex: 0,lineWidth: 0.7,lineDashStyle: [4, 1]}]",
                         vAxes="[{title:'Yield (USD/kg)'}, {title:'Weight (kg)'}]"
                       ))
plot(Line2)
