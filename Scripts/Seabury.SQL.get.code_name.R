##############################################################################
#                  Seabury.SQL.get.code / Seabury.SQL.get.name               #
##############################################################################
Seabury.SQL.get.code = function(ODBC, GXNAME){
  body <- 'SELECT * FROM A269_CARGO.A269_CARGO_SEABURY_GXCODE code'
  dataS  <-  dbSendQuery(ODBC, body)
  query.result <- fetch(dataS)
  name <- query.result[tolower(query.result$GXNAME) %like% tolower(GXNAME),]
  return(name[1,])
}


# give a vector of GXCODE, return a data.frame of GXNAME
Seabury.SQL.get.name = function(ODBC, GXCODE){
  body <- 'SELECT * FROM A269_CARGO.A269_CARGO_SEABURY_GXCODE'
  dataS  <-  dbSendQuery(ODBC, body)
  qr <- fetch(dataS)
  # name <- qr[qr$GXCODE == GXCODE, ]
  name = data.frame()
  for(i in GXCODE){
    x = qr[qr$GXCODE == i, ]
    name = rbind(name, x)
  }
  # name <- apply(GXCODE, 1, function(x) qr[qr$GXCODE == x, ])
  return(name)
}

# Seabury.SQL.get.code(CargoDB, '- - - Other Animals, Live')
# Seabury.SQL.get.name(CargoDB, "DBA")