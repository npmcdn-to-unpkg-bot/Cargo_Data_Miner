#########################################################
#            CargoIS.SQL.query.weight                   #
#########################################################
CargoIS.SQL.query.weight = function(ODBC, min.Weight, max.Weight ,year) {
  if ( nchar(year) == 4 ){
    script = paste("   
                   SELECT *
                   FROM (
                   SELECT       
                   data2.SALES_YEAR,
                   data2.ORIGIN_AIRPORT_CODE, 
                   data2.ORI_COUNTRY, 
                   data2.ORI_REGION,
                   data2.DESTINATION_AIRPORT_CODE,
                   A269_CARGOIS.CARGOIS_AIRPORT.COUNTRY_CODE AS DST_COUNTRY, 
                   A269_CARGOIS.CARGOIS_AIRPORT.REGION_CODE AS DSR_REGION,
                   SUM(data2.WEIGHT_CHARGES_CURR_YEAR_USD) as WEIGHT_CHARGES_CURR_YEAR_USD, 
                   SUM(data2.OTHER_CHARGES_CURR_YEAR_USD) as OTHER_CHARGES_CURR_YEAR_USD,
                   SUM(data2.WEIGHT_CURRENT_YEAR) as WEIGHT_CURRENT_YEAR
                   FROM ( 
                   Select 
                   data.SALES_YEAR,
                   data.SALES_MONTH, 
                   data.ORIGIN_AIRPORT_CODE, 
                   A269_CARGOIS.CARGOIS_AIRPORT.COUNTRY_CODE AS ORI_COUNTRY, 
                   A269_CARGOIS.CARGOIS_AIRPORT.REGION_CODE AS ORI_REGION,
                   data.DESTINATION_AIRPORT_CODE,
                   data.WEIGHT_CHARGES_CURR_YEAR_USD, 
                   data.OTHER_CHARGES_CURR_YEAR_USD,
                   data.WEIGHT_CURRENT_YEAR
                   From A269_CARGOIS.CARGOIS_MONTHLYDATA data
                   INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT
                   ON data.ORIGIN_AIRPORT_CODE = A269_CARGOIS.CARGOIS_AIRPORT.AIRPORT_CODE
                   ) data2
                   
                   INNER JOIN A269_CARGOIS.CARGOIS_AIRPORT 
                   ON data2.DESTINATION_AIRPORT_CODE = A269_CARGOIS.CARGOIS_AIRPORT.AIRPORT_CODE
                   WHERE data2.SALES_YEAR =",year , 
                   "GROUP BY 
                   data2.SALES_YEAR, 
                   data2.ORIGIN_AIRPORT_CODE, 
                   data2.ORI_COUNTRY, 
                   data2.ORI_REGION,
                   data2.DESTINATION_AIRPORT_CODE,
                   A269_CARGOIS.CARGOIS_AIRPORT.COUNTRY_CODE,  
                   A269_CARGOIS.CARGOIS_AIRPORT.REGION_CODE 
                   ORDER BY  
                   data2.SALES_YEAR
                   ) data3
                   WHERE data3.WEIGHT_CURRENT_YEAR >",min.Weight,
                   "AND data3.WEIGHT_CURRENT_YEAR < ", max.Weight,sep = "")
  }
  else {
    warning('please type a four digits year parameter')
  }
  dataS = dbSendQuery(ODBC, script)
  query.result = fetch(dataS)
  return(query.result)
}
