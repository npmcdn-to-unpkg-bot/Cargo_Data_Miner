######### Conncexion MySQL #########

# con <- dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01")
# rs <- dbSendQuery(con, "select * from FR24.FLIGHT_DETAIL;")
# data <- fetch(rs, n = -1)
# huh <- dbHasCompleted(rs)
# dbClearResult(rs)
# dbDisconnect(con)

FR24.query <- function(code, nb.ligne = -1){
  con <- dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01")
  rs <- dbSendQuery(con, code)
  data <- fetch(rs, n = nb.ligne)
  huh <- dbHasCompleted(rs)
  dbClearResult(rs)
  all_cons <- dbListConnections(MySQL())
  return(data)
}

kill_con_fr24 <- function(){
  for(con1 in all_cons){
    dbDisconnect(con1)
  }
}
# FR24.data_query ####
