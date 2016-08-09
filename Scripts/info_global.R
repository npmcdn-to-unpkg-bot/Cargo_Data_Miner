# COMPUTE (does not work for the whole data )
# |
# -- total weight between two regions
# -- average weight/airport
# -- Gross/net yield of air route within two regions
# -- average AWB weighte
# -- monthly weight evolution

info_global = function(dataframe, ORI, DST){
  ori_select = dataframe[dataframe$ORI_REGION == ORI, ]
  
  dst_select = ori_select[ori_select$DSR_REGION == DST, ]
  
  total_weight = sum(dst_select$WEIGHT_CURRENT_YEAR)
  ave_weight = sum(dst_select$WEIGHT_CURRENT_YEAR)/(length(table(append(as.character(dst_select$ORIGIN_AIRPORT_CODE),as.character(dst_select$DESTINATION_AIRPORT_CODE))))*12)
                # total weight / total number of airport 
  
  ave_weight_route = sum(dst_select$WEIGHT_CURRENT_YEAR)/ length(table(dst_select$pair))
                # total_weight/numbers of airport pairs
  g_yield = sum(dst_select$Total.charges.USD)/sum(dst_select$WEIGHT_CURRENT_YEAR)
  n_yield = sum(dst_select$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(dst_select$WEIGHT_CURRENT_YEAR)
  AWB_weight = sum(dst_select$WEIGHT_CURRENT_YEAR)/sum(dst_select$AWB_COUNT_CURRENT_YEAR)
  
  
  y = data.frame(Months=factor(dst_select$SALES_MONTH), Weight = dst_select$WEIGHT_CURRENT_YEAR)
  evo = aggregate(y$Weight/length(table(dst_select$SALES_YEAR)), by=list(Months=y$Months), FUN=sum)
  
  output = data.frame(route = paste(ORI,DST, sep = "-"), 
                      Weight = total_weight, 
                      Average_Weight = ave_weight,
                      Average_weight_route = ave_weight_route,
                      Gross_yield = g_yield,
                      Net_yield = n_yield,
                      Average_AWB_weight = AWB_weight,
                      evomonth = t(evo[,2])
  )
  return(output)
}



