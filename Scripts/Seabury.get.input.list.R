# function to get seabury year list
list_sea_year <- function(CargoDB){
  query <- "SELECT CARGO_YEAR
  FROM A269_CARGO.A269_CARGO_AVAILABLE_YEAR
  WHERE DATA_ORIGIN = 'Seabury' "
  data  <-  dbSendQuery(CargoDB, query)
  choice_sea_year <- fetch(data)
  result <- c(sort(choice_sea_year$CARGO_YEAR, decreasing = TRUE))
  return(result)
}


# function to get seabury region list
list_sea_region <- function(CargoDB) {
  query <- "SELECT DISTINCT AREA 
  FROM A269_CARGO.A269_CARGO_SEABURY_COUNTRY
  WHERE AREA IS NOT NULL 
  ORDER BY AREA"
  data  <-  dbSendQuery(CargoDB, query)
  choice_sea_region <- fetch(data)
  return(choice_sea_region$AREA)
}

# function to get seabury COUNTRY list
list_sea_country <- function(CargoDB) {
  query <- "SELECT DISTINCT CONCAT(substr(COUNTRY_CODE,1,2), CONCAT(', ',COUNTRY_DESC)) AS COUNTRY
FROM A269_CARGO.A269_CARGO_SEABURY_COUNTRY
  WHERE COUNTRY_CODE IS NOT NULL
  ORDER BY COUNTRY"
  data  <-  dbSendQuery(CargoDB, query)
  choice_sea_country <- fetch(data)
  return(choice_sea_country$COUNTRY)
}
