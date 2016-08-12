##################################################
#               grs_net_yield                    #
##################################################

#########################
grs_net_yield = function(dataset, ori_airport, dst_airport){
  # # select proper year
  # database = cargo[cargo$SALES_YEAR == year,]
  # # create proper subset
  # subset = database[database["ORIGIN_AIRPORT_CODE"] == ori_airport, ]
  
  # subset_2 = CargoIS.SQL.query(ODBC, level = 'A2A',ORG = ori_airport,DST = dst_airport, year = year)
  # subset_2[is.na(subset_2)] = 0
  
  subset = subset(dataset, dataset$ORIGIN_AIRPORT_CODE == ori_airport) 
  
  if (nrow(subset) != 0) {  
    subset_2 = subset(subset, subset$DESTINATION_AIRPORT_CODE == dst_airport)
    subset_2[is.na(subset_2)] = 0
    
    # computation
    grs_yld_tt = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD + subset_2$OTHER_CHARGES_CURR_YEAR_USD)/sum(subset_2$WEIGHT_CURRENT_YEAR)
    net_yld_tt = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(subset_2$WEIGHT_CURRENT_YEAR)
    weight_tt = sum(subset_2$WEIGHT_CURRENT_YEAR)
    grs_chg = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD + subset_2$OTHER_CHARGES_CURR_YEAR_USD)
    net_chg = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD)
    # output
    output = data.frame( gross_yield =as.numeric( paste(round(grs_yld_tt, digits = 2))),
                         net_yield = as.numeric(paste(round(net_yld_tt,digits = 2))),
                         total_weight = as.numeric(paste(round(weight_tt,digits = 2))),
                         gross_charges = as.numeric(paste(round(grs_chg,digits = 2))),
                         net_charges = as.numeric(paste(round(net_chg,digits = 2))),
                         row.names = paste(ori_airport,dst_airport, sep = '-')
    )
  } 
  
  else {output = data.frame(gross_yield = 0,
                            net_yield = 0,
                            total_weight = 0,
                            gross_charges = 0,
                            net_charges = 0,
                            row.names = paste(ori_airport,dst_airport, sep = '-')
  )
  
  }
  return(output)
}

## 
output_yield = function(dataset, city_list, path = paste(TEMPS,'/yield',sep = '') ){
  output = NA
  for (i in 1:nrow(city_list)){
    ORI = as.character(city_list[i,1])
    DST = as.character(city_list[i,2])
    output = rbind(output, grs_net_yield(dataset, ORI, DST))
  }
  
  out = output[-1,]
  # write.csv(out, file = path)
  return(out)
}

# city_list <- data.frame( ORG = c('CDG',"PVG"), DST = c('LAX', 'FRA'))
# 
# 
# output_yield = function(dataset, city_list, path = paste(TEMPS,'/yield',sep = '') ){
#   # output <- data.table('ORG', 'DST', 'gross_yield','net_yield','total_weight',
#   #                      'gross_charges', 'net_charges')
#   output <- data.table(ORG =city_list$DST,DST = city_list$ORG)
# 
# 
# 
#   out = output[-1,]
#   write.csv(out, file = path)
#   return(out)
# }




# cargo2015 =CargoIS.SQL.query(CargoDB, 'A2A', ORG = 'MEX', DST = 'MAD', year = 2015)
# cargo2015 =CargoIS.SQL.query(CargoDB, 'ALL',  year = 2015)
# cargo2014 =CargoIS.SQL.query(CargoDB, 'ALL',  year = 2014)

# grs_net_yield(cargo2015, 'MAD', 'MEX')
#
 # cp = read.csv('H:/compute_yield/05-11.csv', head = F, sep = ',')
 # x = output_yield(cargo2014, cp)

###################################################################
# grs_net_yield = function( cargo, year, ori_airport, dst_airport){
#   # select proper year
#   database = cargo[cargo$SALES_YEAR == year,]
#   # create proper subset
#   subset = database[database["ORIGIN_AIRPORT_CODE"] == ori_airport, ]
#   
#   if (nrow(subset) != 0) {  
#     subset_2 = subset[subset["DESTINATION_AIRPORT_CODE"] == dst_airport,]
#     # computation
#     grs_yld_tt = sum(subset_2$Total.charges.USD)/sum(subset_2$WEIGHT_CURRENT_YEAR)
#     net_yld_tt = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD)/sum(subset_2$WEIGHT_CURRENT_YEAR)
#     weight_tt = sum(subset_2$WEIGHT_CURRENT_YEAR)
#     grs_chg = sum(subset_2$Total.charges.USD)
#     net_chg = sum(subset_2$WEIGHT_CHARGES_CURR_YEAR_USD)
#     # output
#     output = data.frame( gross_yield =as.numeric( paste(round(grs_yld_tt, digits = 2))),
#                          net_yield = as.numeric(paste(round(net_yld_tt,digits = 2))),
#                          total_weight = as.numeric(paste(round(weight_tt,digits = 2))),
#                          gross_charges = as.numeric(paste(round(grs_chg,digits = 2))),
#                          net_charges = as.numeric(paste(round(net_chg,digits = 2))),
#                          row.names = paste(ori_airport,dst_airport, sep = ';')
#     )
#   } 
#   
#   else {output = data.frame(gross_yield = 0,
#                             net_yield = 0,
#                             total_weight = 0,
#                             gross_charges = 0,
#                             net_charges = 0,
#                             row.names = paste(ori_airport,dst_airport, sep = ';')
#   )
#   
#   }
#   return(output)
# }
# 

### test git 
