#################################################################
#               sea.evo: Seabury evolution                      #
#################################################################



sea.evo= function( sea.DB, GX.level = 'G1', weight = TRUE, SFC = FALSE ){
  if(SFC){
    if(weight){
      df = sea.DB[,c('CARGO_YEAR','SFCWEIGHT','G4CODE')]
    }
    else{
      df = sea.DB[,c('CARGO_YEAR','SFCVALUE','G4CODE')]
    }
  } else {
    if(weight){
      df = sea.DB[,c('CARGO_YEAR','AIRWEIGHT','G4CODE')]
    }
    else{
      df = sea.DB[,c('CARGO_YEAR','AIRVALUE','G4CODE')]
    }
  }
  
  if(GX.level == 'G1'){
    df.agg = aggregate(df[,2], by=list(Year = df[,1], Name = substr(df[,3],1,1)), FUN=sum)
  }
  else if(GX.level == 'G2'){
    df.agg = aggregate(df[,2], by=list(Year = df[,1], Name = substr(df[,3],1,2)), FUN=sum)
  }
  else if(GX.level == 'G3'){
    df.agg = aggregate(df[,2], by=list(Year = df[,1], Name = substr(df[,3],1,3)), FUN=sum)
  }
  else if(GX.level == 'G4'){
    df.agg = aggregate(df[,2], by=list(Year = df[,1], Name = substr(df[,3],1,4)), FUN=sum)
  }
  
  df.list <- split(df.agg, df.agg$Name, drop = FALSE)
  res <- lapply(df.list, function(t) t[,c(1,3)])
  
  year2 <- Seabury.get.year(CargoDB)
  
  output <- data.frame(Year = year2)
  
  for(name in names(res)){
    output <- merge(output, res[[name]], by='Year', all = TRUE)
  }
  colnames(output) <- c('Year',names(res))
  output[is.na(output)] = 0
  return(output)
}

# test <- sea.evo(data, GX.level = 'G2', SFC = TRUE)
# x = Seabury.SQL.get.name(CargoDB,names(test)[-1])
# # names(test)[-1] = as.character(x$GXNAME)
# 
# nam = names(test)[2]
# p <- plot_ly(test, x = Year, y = eval(parse(text = nam)), name = x$GXNAME[1])
# for(namcol in names(test)[c(-1,-2)]){
#   p <- add_trace(test, x = Year, y = eval(parse(text = namcol)), name = x$GXNAME[x$GXCODE == namcol], evaluate = TRUE)
# 
# }
# 
# p







# 
# test[is.na(test)] = 0
# for(i in names(test)){
#   print(add_trace(test, x = Year, y = test$i, name = names(res), line = list(shape = "linear",evaluate = TRUE)))
# }
# plotly_empty()
# 
# 
# plot_ly(test, x = Year, y = 'DBBF', name = 'Net yield', line = list(shape = "linear"))
# plot_ly(test, x = Year, y = test[,2:19], name = names(res), line = list(shape = "linear"))
# test <- left_join(x$dbbe, x$DBBF, by = c('Year' = 'Year'))
# # 
# year2 <- c(2000:2014)  # take data2()
# test <- data.frame(Year = year2)
# for (name in names(x)){
#   test <- left_join(test,x[name], by=c("Year"="Year"), copy = TRUE)
# }

#   # Create subset of sea.DB
#   if(weight){                     ## weight evolution
#     if(GX.level == 'G1'){
#       df = sea.DB[,c('CARGO_YEAR','AIRWEIGHT','G1NAME')]
#     }
#     else if(GX.level == 'G2'){
#       df = sea.DB[,c('CARGO_YEAR','AIRWEIGHT','G2NAME')]
#     }
#     else if(GX.level == 'G3'){
#       df = sea.DB[,c('CARGO_YEAR','AIRWEIGHT','G3NAME')]
#     }
#     else if(GX.level == 'G4'){
#       df = sea.DB[,c('CARGO_YEAR','AIRWEIGHT','G4NAME')]
#     }
#   }
#   if(weight == FALSE){            ## value evolution
#     if(GX.level == 'G1'){
#       df = sea.DB[,c('CARGO_YEAR','AIRVALUE','G1NAME')]
#     }
#     else if(GX.level == 'G2'){
#       df = sea.DB[,c('CARGO_YEAR','AIRVALUE','G2NAME')]
#     }
#     else if(GX.level == 'G3'){
#       df = sea.DB[,c('CARGO_YEAR','AIRVALUE','G3NAME')]
#     }
#     else if(GX.level == 'G4'){
#       df = sea.DB[,c('CARGO_YEAR','AIRVALUE','G4NAME')]
#     }
#   }
# 
#   df.agg = aggregate(df[,2], by=list(Year = df[,1], Name = df[,3]), FUN=sum)
# }
# x = split(df.agg, df.agg$Name, drop = FALSE)
# names(x)
# for(name in names(x)){

# }