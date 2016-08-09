#########################################################
#                 CargoIS.SQL.query                     #
#########################################################


CargoIS.SQL.query = function(ODBC,level, ORG, DST, year = NA){
  body = "
    SELECT       
      data.SALES_YEAR,
      data.SALES_MONTH, 
      data.ORIGIN_AIRPORT_CODE, 
      data.ORIGIN_COUNTRY_CODE AS ORI_COUNTRY, 
      org.REGION_CODE AS ORI_REGION,
      data.DESTINATION_AIRPORT_CODE,
      dst.COUNTRY_CODE AS DST_COUNTRY, 
      dst.REGION_CODE AS DSR_REGION,
      data.WEIGHT_CHARGES_CURR_YEAR_USD, 
      data.OTHER_CHARGES_CURR_YEAR_USD,
      data.WEIGHT_CURRENT_YEAR, 
      data.WEIGHT_BREAK,
      data.AWB_COUNT_CURRENT_YEAR,
      data.GREATCIRCLEDISTANCE_KM AS GREATCIRCLEDISTANCE_NM
    From A269_CARGOIS.CARGOIS_MONTHLYDATA data
    INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT org ON data.ORIGIN_AIRPORT_CODE = org.AIRPORT_CODE
    INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT dst ON data.DESTINATION_AIRPORT_CODE = dst.AIRPORT_CODE "
    
  if (is.na(year) == TRUE){
    if (level == 'R2R'){
      scp = paste("WHERE org.REGION_CODE = '",ORG,
                  "' AND dst.REGION_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'C2R'){
      scp =  paste("WHERE data.ORIGIN_COUNTRY_CODE = '",ORG,
                   "' AND dst.REGION_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'R2C'){
      scp =  paste("WHERE org.REGION_CODE = '",ORG,
                   "' AND dst.COUNTRY_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'C2C'){
      scp =  paste("WHERE data.ORIGIN_COUNTRY_CODE = '",ORG,
                   "' AND dst.COUNTRY_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'C2W'){
      scp =  paste("WHERE data.ORIGIN_COUNTRY_CODE = '",ORG, "'",
                   sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'W2C'){
      scp =  paste("WHERE dst.COUNTRY_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'A2A'){
      scp =  paste("WHERE data.ORIGIN_AIRPORT_CODE = '",ORG,
                   "' AND data.DESTINATION_AIRPORT_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'R2A'){
      scp =  paste("WHERE org.REGION_CODE = '",ORG,
                   "' AND data.DESTINATION_AIRPORT_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'A2R'){
      scp =  paste("WHERE data.ORIGIN_AIRPORT_CODE = '",ORG,
                   "' AND dst.REGION_CODE = '",DST,"'",sep = '')
      script = paste(body, scp, sep = '')
    } else if (level == 'ALL'){
      script = paste(body, sep = '')
    }
  } else {
    if ( nchar(year) == 4 ){
      if (level == 'R2A'){
        scp = paste("WHERE org.REGION_CODE = '",ORG,
                    "' AND data.DESTINATION_AIRPORT_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'A2R'){
        scp = paste("WHERE data.ORIGIN_AIRPORT_CODE = '",ORG,
                    "' AND dst.REGION_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'R2R'){
        scp = paste("WHERE org.REGION_CODE = '",ORG,
                    "' AND dst.REGION_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'C2R'){
        scp = paste("WHERE data.ORIGIN_COUNTRY_CODE = '",ORG,
                    "' AND dst.REGION_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'R2C'){
        scp = paste("WHERE org.REGION_CODE = '",ORG,
                    "' AND dst.COUNTRY_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'W2C'){
        scp = paste("WHERE dst.COUNTRY_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'C2C'){
        scp = paste("WHERE data.ORIGIN_COUNTRY_CODE = '",ORG,
                    "' AND dst.COUNTRY_CODE = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'A2A'){
        scp = paste("WHERE data.ORIGIN_AIRPORT_CODE = '",ORG,
                    "' AND data.DESTINATION_AIRPORT_CODE  = '",DST,"' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'C2W'){
        scp = paste("WHERE data.ORIGIN_COUNTRY_CODE = '", ORG, "' AND data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      } else if (level == 'ALL'){
        scp = paste("WHERE data.SALES_YEAR = ",year,sep = '')
        script = paste(body, scp, sep = '')
      }
    } else {
      warning('please type a four digits year parameter')
    }
  }
  #else { warning('Please check the parameters')}
  dataS = dbSendQuery(ODBC, script)
  query.result = fetch(dataS)
  query.result[is.na(query.result)] <- 0
  query.result$GREATCIRCLEDISTANCE_NM <- round(query.result$GREATCIRCLEDISTANCE_NM/1.852,0)
  return(query.result)
}



# CargoIS.SQL.query(ODBC,level, ORG, DST, year = NA)
#
# ODBC: name of the Oracle server connexion
# level: 'ALL', 'R2R', 'C2R', 'R2C', 'C2C', 'C2W', 'W2C', 'A2A'
# ORG: origin. (depends on level parameter. Could be airport code, country code and region code.)
#              (ignored if level = 'ALL')
# DST: destination (same as ORG parameter)
# year: defaut NA --> return data for all the periods
#       if year = xxxx (four digits number) then return data for the specified year.

# test2 = CargoIS.SQL.query(CargoDB, level = 'W2C',DST = 'HK')


# test2 = CargoIS.SQL.query(CargoDB, level = 'R2R',ORG = 'NAM', DST = 'ASPAC', year = 2015)
