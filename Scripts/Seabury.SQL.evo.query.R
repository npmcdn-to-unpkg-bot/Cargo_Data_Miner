#################################################################
#                   Seabury.SQL.evo.query                       #
#################################################################

library(ROracle)

Seabury.SQL.evo.query <- function(ODBC, level, GX.code, ORG, DST, year = NA, country.name = FALSE, SFC = FALSE){
  ## body of the query 
  ## IF country.name = TRUE --> add country name
  if(SFC){
    if(country.name){
      body = "SELECT
      CARGO_YEAR,
      SFCWEIGHT,
      SFCVALUE,
      G4CODE,
      ORIGIN_COUNTRY,
      DESTINATION_COUNTRY,
      jn5.COUNTRY_DESC AS ORG_COUNRTY,
      jn5.AREA AS ORG_AREA,
      jn6.COUNTRY_DESC AS DST_COUNTRY,
      jn6.AREA AS DST_AREA
      FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
      INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
      INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE"
    }
    else {
      body = "SELECT
      CARGO_YEAR,
      SFCWEIGHT,
      SFCVALUE,
      G4CODE
      FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
      INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
      INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE"
    }
  } else {
  if(country.name){
    body = "SELECT
    CARGO_YEAR,
    AIRWEIGHT,
    AIRVALUE,
    G4CODE,
    ORIGIN_COUNTRY,
    DESTINATION_COUNTRY,
    jn5.COUNTRY_DESC AS ORG_COUNRTY,
    jn5.AREA AS ORG_AREA,
    jn6.COUNTRY_DESC AS DST_COUNTRY,
    jn6.AREA AS DST_AREA
    FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
    INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
    INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE"
  }
  else {
    body = "SELECT
    CARGO_YEAR,
    AIRWEIGHT,
    AIRVALUE,
    G4CODE
    FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
    INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
    INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE"
  }
  }
  ## ORG and DST
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
  
  ## GXLEVEL
  if (is.na(GX.code)){
    script.gxl = " "
  }
  
  else if (nchar(GX.code) == 1){
    script.gxl = paste("
                       AND SUBSTR(data.G4CODE,1,1) = '", GX.code, "' ", sep ='')
  }
  else if (nchar(GX.code) == 2){
    script.gxl = paste("
                       AND SUBSTR(data.G4CODE,1,2) = '", GX.code, "' ", sep ='')
  }
  else if (nchar(GX.code) == 3){
    script.gxl = paste("
                       AND SUBSTR(data.G4CODE,1,3) = '", GX.code, "' ", sep ='')
  }
  else if (nchar(GX.code) == 4){
    script.gxl = paste("
                       AND SUBSTR(data.G4CODE,1,4) = '", GX.code, "' ", sep ='')
  }

  
  ## year
  if(is.na(year)==FALSE){
    script.y <- paste(" AND data.CARGO_YEAR = ", year, sep = '')
  }
  else{script.y = " "}
  
  script.gx = paste(body, script, script.gxl, script.y, sep = " ")
  dataS = dbSendQuery(ODBC, script.gx)
  query.result = fetch(dataS)
  return(query.result)
}


# data = Seabury.SQL.evo.query(CargoDB, 'C2C',ORG = 'CN', DST = 'US', GX.code = 'A', SFC = TRUE)

############# too slow #######################
# Seabury.SQL.evo.query <- function(ODBC, level, GX.level = NA, GX.name, ORG, DST, year = NA, country.name = FALSE){
#   if(country.name){
#     body = 'SELECT 
#     ORIGIN_COUNTRY,
#     DESTINATION_COUNTRY,
#     CARGO_YEAR,
#     AIRWEIGHT,
#     AIRVALUE,
#     jn1.GXNAME AS G4NAME,
#     jn2.GXNAME AS G3NAME,
#     jn3.GXNAME AS G2NAME,
#     jn4.GXNAME AS G1NAME,
#     jn5.COUNTRY_DESC AS ORG_COUNRTY,
#     jn5.AREA AS ORG_AREA,
#     jn6.COUNTRY_DESC AS DST_COUNTRY,
#     jn6.AREA AS DST_AREA
#     FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn1 ON data.G4CODE = jn1.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn2 ON SUBSTR(data.G4CODE,1,3) = jn2.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn3 ON SUBSTR(data.G4CODE,1,2) = jn3.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn4 ON SUBSTR(data.G4CODE,1,1) = jn4.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE '
#   }
#   else{
#     body = 'SELECT 
#     CARGO_YEAR,
#     AIRWEIGHT,
#     AIRVALUE,
#     jn1.GXNAME AS G4NAME,
#     jn2.GXNAME AS G3NAME,
#     jn3.GXNAME AS G2NAME,
#     jn4.GXNAME AS G1NAME
#     FROM A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4 data
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn1 ON data.G4CODE = jn1.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn2 ON SUBSTR(data.G4CODE,1,3) = jn2.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn3 ON SUBSTR(data.G4CODE,1,2) = jn3.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_GXCODE jn4 ON SUBSTR(data.G4CODE,1,1) = jn4.GXCODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn5 ON data.ORIGIN_COUNTRY = jn5.COUNTRY_CODE
#     INNER JOIN A269_CARGO.A269_CARGO_SEABURY_COUNTRY jn6 ON data.DESTINATION_COUNTRY = jn6.COUNTRY_CODE '
#   }
#  
#   if (is.na(year) == TRUE){
#     if (level == 'C2C'){
#       script = paste("
#                      WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
#                      AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'", sep = "")
#     }
#     if (level == 'R2R'){
#       script = paste("WHERE jn5.AREA = '", ORG,"' AND jn6.AREA = '", DST,"'", sep = "")
#     }
#     if (level == 'C2R'){
#       script = paste("WHERE SUBSTR(data.ORIGIN_COUNTRY,1,2) = '", ORG,"'
#                      AND jn6.AREA = '", DST,"'", sep = "")
#     }
#     if (level == 'R2C'){
#       script = paste("WHERE jn5.AREA = '", ORG,"'
#                      AND SUBSTR(data.DESTINATION_COUNTRY,1,2) = '", DST,"'", sep = "")
#     }
#     if (level == 'ALL'){
#       script = ''
#     }
#   }
#   script.no.gx = paste(body,script, sep = "")
#   
#   if (GX.level == 'G1'){
#     script.gxl = paste(" 
#                        AND jn4.GXNAME = '", GX.name, "' ", sep ='')
#   }
#   
#   else if (GX.level == 'G2'){
#     script.gxl = paste(" 
#                        AND jn3.GXNAME = '", GX.name, "' ", sep ='')
#   }
#   
#   else if (GX.level == 'G3'){
#     script.gxl = paste(" 
#                        AND jn2.GXNAME = '", GX.name, "' ", sep ='')
#   }
#   
#   else if (GX.level == 'G4'){
#     script.gxl = paste(" 
#                        AND jn4.GXNAME = '", GX.name, "' ", sep ='')
#   }
#   else if (is.na(GX.level)){
#     script.gxl = paste("","", sep = "")
#   }
#   
#   script.gx = paste(script.no.gx, script.gxl, sep = " ") 
#   dataS = dbSendQuery(ODBC, script.gx)
#   query.result = fetch(dataS)
#   return(query.result)
# }
