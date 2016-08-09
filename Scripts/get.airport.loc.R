######### get.airport.loc #########

get.airport.loc <- function(){
  # con <- dbConnect(MySQL(), user="user_ext", password="fr0-dmds-p01", dbname='FR24', host="fr0-dmds-p01")
  # rs <- dbSendQuery(con, 
  #                   "select 
  #                   APT.IATA, 
  #                   APT.LONGITUDE,
  #                   APT.LATITUDE
  #                   from FR24.AIRPORT APT;")
  # data <- fetch(rs, n = -1)
  # names(data) <- c("IATA","long", "lat")
  # huh <- dbHasCompleted(rs)
  # dbClearResult(rs)
  # dbDisconnect(con)
  BIO <- odbcDriverConnect('driver={SQL Server};
                              server=fr0-bio-p01,10335;
                           database=BIO_NEO_PROD;
                           trusted_connection=true')
  query.BIO <- sqlQuery(BIO, 
                        'select IATA,
                            LONGITUDE,
                            LATITUDE,
                            NAME
                    from bio.AIRPORT ')
  names(query.BIO) <- c("IATA","long", "lat", "name")
  close(BIO)
  return(query.BIO)
}

# apttest <- get.airport.loc()

get.airport.esad <- function(){
  BIO <- odbcDriverConnect('driver={SQL Server};
                           server=fr0-bio-p01,10335;
                           database=BIO_NEO_PROD;
                           trusted_connection=true')
  query.BIO <- sqlQuery(BIO, 
                        'SELECT from_.IATA AS ORG,
                                to_.IATA AS DST,
                                apt.DISTANCE,
                                apt.ESAD
                        FROM BIO_NEO_PROD.bio.AIRPORT_PAIR apt
                        INNER JOIN BIO_NEO_PROD.bio.AIRPORT from_ ON apt.ORG_AIRPORT_ID = from_.AIRPORT_ID
                        INNER JOIN BIO_NEO_PROD.bio.AIRPORT to_ ON apt.DST_AIRPORT_ID = to_.AIRPORT_ID
                        ')
  # names(query.BIO) <- c("IATA","long", "lat")
  close(BIO)
  return(query.BIO)
}
# apttest <- get.airport.esad()
