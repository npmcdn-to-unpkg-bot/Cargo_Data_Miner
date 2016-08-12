# function to get cargois region list
list_cargois_region <- function(CargoDB) {
  query <- "SELECT DISTINCT REGION_CODE, REGION_NAME FROM A269_CARGOIS.CARGOIS_AIRPORT"
  data  <-  dbSendQuery(CargoDB, query)
  choice_cargois_region <- fetch(data)
  choice_cargois_region <- choice_cargois_region[order(choice_cargois_region$REGION_NAME),]
  # list <- as.list(choice_cargois_region$REGION_CODE)
  # names(list) <- choice_cargois_region$REGION_NAME
  list <- paste(choice_cargois_region$REGION_CODE, ', ', choice_cargois_region$REGION_NAME, sep = '')
  return(list)
}

# function to get cargois year list
list_cargois_year <- function(CargoDB) {
  query <- "SELECT YEAR_NUM
  FROM A269_CARGOIS.CARGOIS_PERIOD_TO_LOAD"
  data  <-  dbSendQuery(CargoDB, query)
  choice_cargois_year <- fetch(data)
  year <- sort(c(2010:max(choice_cargois_year$YEAR_NUM)),decreasing = TRUE) # create vector 2010:now
  
  # list_year <- lapply(year, function(x) x) 
  # names(list_year) <- sapply(list_year,paste,collapse="")      # create a list with the proper name
  
  return(year)
}

# function to get cargois city list
list_cargois_city <- function(CargoDB) {
  query <- "SELECT DISTINCT CITY_CODE, CONCAT(CITY_NAME,CONCAT(', ',COUNTRY_CODE)) AS CITY_NAME
FROM A269_CARGOIS.CARGOIS_AIRPORT
  ORDER BY CITY_CODE"
  data  <-  dbSendQuery(CargoDB, query)
  choice_cargois_city <- fetch(data)
  list <- paste0(choice_cargois_city$CITY_CODE, ', ', choice_cargois_city$CITY_NAME)

  return(list)
}


# function to get cargois country list
list_cargois_country <- function(CargoDB) {
  query <- "SELECT DISTINCT CONCAT(CASS_COUNTRY_CODE,CONCAT(', ',CASS_COUNTRY_NAME)) AS COUNTRY
FROM A269_CARGOIS.CARGOIS_COUNTRY
  ORDER BY COUNTRY"
  data  <-  dbSendQuery(CargoDB, query)
  choice_cargois_ctry <- fetch(data)
  return(choice_cargois_ctry$COUNTRY)
}

# function to get cargois airport list
list_cargois_airport <- function(CargoDB) {
  query <- "SELECT DISTINCT CONCAT(AIRPORT_CODE, CONCAT(', ',AIRPORT_NAME)) AS AIRPORT
  FROM A269_CARGOIS.CARGOIS_AIRPORT
  WHERE AIRPORT_CODE IS NOT NULL
  ORDER BY AIRPORT"

  data  <-  dbSendQuery(CargoDB, query)
  choice_cargois_apt <- fetch(data)
  return(choice_cargois_apt$AIRPORT)
}
