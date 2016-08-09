######### Seabury.get.year #########

Seabury.get.year <- function(ODBC){
  body = "SELECT data.CARGO_YEAR
          FROM A269_CARGO.A269_CARGO_AVAILABLE_YEAR data
          WHERE data.DATA_ORIGIN = 'Seabury'"
  dataS = dbSendQuery(ODBC, body)
  query.result = fetch(dataS)
  
  year = sort(as.numeric(query.result$CARGO_YEAR))
  return(year)
}

# list = as.list(Seabury.get.year(CargoDB))
# names(list) = c(as.character(Seabury.get.year(CargoDB)))
