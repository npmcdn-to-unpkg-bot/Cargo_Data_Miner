#################################################################
#                     Load Oracle Database                      #
#################################################################
#       Xuzhou QIN              | last modif. 18/04/2016        #
#################################################################
library(ROracle)

drv = dbDriver('Oracle')
host = 'p595dodmp01'
port = 1521
sid = 'DBUPA269'

connect.string <- paste("(DESCRIPTION=",
                         "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
                         "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

CargoDB = dbConnect(drv, username = 'CARGO', password = 'dbu1_cargo', dbname = connect.string)
# dbDisconnect(CargoDB)
####################### Query ########################


# check = cargo[ cargo$COMMISSIONS_CURRENT_YEAR_USD != 0,]
# summary(cargo)

####################### Old Cargo IS DB ########################
drv2 = dbDriver('Oracle')
host2 = 'p595dodmv01'
port2 = 1521
sid2 = 'DBUVF260 '

connect.string2 <- paste("(DESCRIPTION=",
                         "(ADDRESS=(PROTOCOL=tcp)(HOST=", host2, ")(PORT=", port2, "))",
                         "(CONNECT_DATA=(SID=", sid2, ")))", sep = "")
CargoDB.old = dbConnect(drv2, username = 'CARGOIS', password = 'dbu1_cargois', dbname = connect.string2)
