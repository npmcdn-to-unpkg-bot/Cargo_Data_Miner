#################################################################
#                   Seabury.SQL.OD.query                        #
#################################################################

library(ROracle)
# 
# drv = dbDriver('Oracle')
# host = 'p595dodmp01'
# port = 1521
# sid = 'DBUPA269'
# 
# connect.string <- paste("(DESCRIPTION=",
#                         "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
#                         "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
# CargoDB <- dbConnect(drv, username = 'CARGO', password = 'dbu1_cargo', dbname = connect.string)

Seabury.SQL.OD.query <- function(ODBC, level, ORG, DST, year = NA, country.name = FALSE){
  if(country.name){
    body <- 'SELECT 
                ORIGIN_COUNTRY,
                DESTINATION_COUNTRY,
                CARGO_YEAR,
                AIRWEIGHT,
                AIRVALUE,
                SFCWEIGHT,
                SFCVALUE,
                jn1.GXNAME AS G4NAME,
                jn2.GXNAME AS G3NAME,
                jn3.GXNAME AS G2NAME,
                jn4.GXNAME AS G1NAME,
                jn5.COUNTRY_DESC AS ORG_COUNRTY,
                jn5.AREA AS ORG_AREA,
                jn6.COUNTRY_DESC AS DST_COUNTRY,
                jn6.AREA AS DST_AREA
            FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn1 ON data.G4CODE = jn1.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn2 ON SUBSTR(data.G4CODE,1,3) = jn2.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn3 ON SUBSTR(data.G4CODE,1,2) = jn3.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn4 ON SUBSTR(data.G4CODE,1,1) = jn4.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE '
  }else{
    body <- 'SELECT 
              ORIGIN_COUNTRY,
              DESTINATION_COUNTRY,
              CARGO_YEAR,
              AIRWEIGHT,
              AIRVALUE,
              SFCWEIGHT,
              SFCVALUE,
              jn1.GXNAME AS G4NAME,
              jn2.GXNAME AS G3NAME,
              jn3.GXNAME AS G2NAME,
              jn4.GXNAME AS G1NAME,
              jn5.AREA AS ORG_AREA,
              jn6.AREA AS DST_AREA
            FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn1 ON data.G4CODE = jn1.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn2 ON SUBSTR(data.G4CODE,1,3) = jn2.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn3 ON SUBSTR(data.G4CODE,1,2) = jn3.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn4 ON SUBSTR(data.G4CODE,1,1) = jn4.GXCODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
            INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE '
  }
  
  if (is.na(year) == TRUE){
    if (level == 'C2C'){
      script = paste("
            WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
            AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'", sep = "")
    } else if (level == 'R2R'){
      script = paste("WHERE jn5.AREA = '", ORG,"' AND jn6.AREA = '", DST,"'", sep = "")
    } else if (level == 'C2R'){
      script = paste("WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
                     AND jn6.AREA = '", DST,"'", sep = "")
    } else if (level == 'R2C'){
      script = paste("WHERE jn5.AREA = '", ORG,"'
                     AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'", sep = "")
    } else if (level == 'ALL'){
      script = ''
    } else if (level == 'C2W'){
      script = paste("WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'", sep = "")
    } else if (level == 'W2C'){
      script = paste("WHERE SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'", sep = "")
    } else if (level == 'R2W'){
      script = paste("WHERE jn5.AREA = '", ORG,"'", sep = "")
    } else if (level == 'W2R'){
      script = paste("WHERE jn6.AREA = '", DST,"'", sep = "")
    }
    
  } else {
    if (level == 'C2C'){
      script = paste("
                     WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
                     AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'R2R'){
      script = paste("WHERE jn5.AREA = '", ORG,"'
                     AND jn6.AREA = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'C2R'){
      script = paste("WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
                     AND jn6.AREA = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'R2C'){
      script = paste("WHERE jn5.AREA = '", ORG,"'
                     AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'ALL'){
      script = paste(" WHERE data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'C2W'){
      script = paste("WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"' 
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'W2C'){
      script = paste("WHERE SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'R2W'){
      script = paste("WHERE jn5.AREA = '", ORG,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    } else if (level == 'W2R'){
      script = paste("WHERE jn6.AREA = '", DST,"'
                     AND data.CARGO_YEAR = ", year, sep = "")
    }
  }
  script.toto  <-  paste(body,script, sep = "")
  
  dataS  <-  dbSendQuery(ODBC, script.toto)
  query.result <- fetch(dataS)
  return(query.result)
}






# dt2 = Seabury.SQL.evo.query(CargoDB, 'C2C',ORG = 'EG', DST = 'SA', GX.level = 'G1', GX.name = 'Consumer Fashion Goods')
# dt = Seabury.SQL.OD.query(CargoDB, 'C2C',ORG = 'EG', DST = 'SA')
