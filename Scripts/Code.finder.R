######### CODE FINDER #########
# general code finer for:

# 'Airport'='airport',
# 'Country'='country',
# 'Region (CargoIS)' = 'region(cargois)',
# 'Airline'='airline',
# 'GXCODE (Seabury trade)' = 'gxcode'

# input: {NAME} *patial match
# output: {dataframe like} CODE, NAME

Code.finder <- function(name, level = 'airport'){
  if(level == 'airport'){
    query <- 'SELECT ap.AIRPORT_CODE, ap.AIRPORT_NAME, ap.CITY_NAME
              FROM A269_CARGOIS.CARGOIS_AIRPORT ap'
    dataS  <-  dbSendQuery(CargoDB, query)
    
    query.result <- fetch(dataS)
    query.result$index <- paste(query.result$AIRPORT_NAME,query.result$CITY_NAME)
    name1 <- query.result[query.result$index %like% name,][,c(1,2,3)]
    names(name1) <- c('Code','Name','City')
    
  } else if (level == 'airline'){
    
    query <- 'SELECT acc.IATA, acc.SHORT_NAME
              FROM BIO_NEO_PROD.bio.ACCOUNT acc'
    query.BIO <- sqlQuery(BIO, query)
    query.BIO <- query.BIO[query.BIO$IATA != '',]
    query.result <- query.BIO[complete.cases(query.BIO),]
    name1 <- query.result[tolower(query.result$SHORT_NAME) %like% tolower(name),]
    names(name1) <- c('IATA airline','Name')
  } else if (level == 'country'){
    
  }
  
  row.names(name1) <- NULL
  return(name1)
}

# TEST
# Code.finder('Charles Paris', level = 'airport')
# 
# query <- paste("SELECT ap.AIRPORT_CODE AS Code, ap.AIRPORT_NAME AS Name
#                     FROM A269_CARGOIS.CARGOIS_AIRPORT ap
#       WHERE lower(ap.AIRPORT_NAME) LIKE lower('%",'charles',"')", sep = '')
