########################################
#####  RODBC connect string (BIO)  ##### 
########################################

# connect string
BIO <- odbcDriverConnect('driver={SQL Server};
                              server=fr0-bio-p01,10335;
                              database=BIO_NEO_PROD;
                              trusted_connection=true')

# res <- sqlQuery(dbhandle, 'select * from information_schema.tables')


# send query
query.BIO <- sqlQuery(BIO, 
                    'select IATA,
                            LONGITUDE,
                            LATITUDE
                    from bio.AIRPORT ')

# query a table 
airports <- sqlFetch(BIO, 'bio.AIRPORT')

# close the connection
close(BIO)

# creat a .csv file 
write.csv(airports, 'E:/DATA/bio.airports.csv')


