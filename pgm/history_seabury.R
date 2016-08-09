source('C:/Users/QIN_XUZ/Documents/CARGOIS/Scripts/Load_Oracle_DB.R')
sbry_histo2 = dbSendQuery(CargoDB, 'Select * From A269_CARGO.A269_CARGO_SEABURY_BY_COUNTRY')
history2 = fetch(sbry_histo2)

# table(history$CARGO_YEAR)
table(history2$CARGO_YEAR)
names(history2)

x = data.frame(Year=factor(history2$CARGO_YEAR), Weight = history2$AIRWEIGHT/1000000000)
evo = aggregate(x$Weight, by=list(Year=x$Year), FUN=sum)

qplot(x =Year, y = x, data = evo, geom = 'line',group = 1, ylab = "billion kg")


################################
sbry_dtl_G4 = dbSendQuery(CargoDB, 'Select * From A269_CARGO.A269_CARGO_SEABURY_DETAIL_G4')

saveRDS(fetch(sbry_dtl_G4),file.path(DATA,'Seabury_detail_G4.rds'))
Seabury = readRDS(file.path(DATA,'Seabury_detail_G4.rds'))

head(Seabury)

x = data.frame(Year = factor(Seabury$CARGO_YEAR), weight = Seabury$AIRWEIGHT/1000000000)
evo_new = aggregate(x$weight, by=list(Year=x$Year), FUN=sum)
qplot(x =Year, y = x, data = evo_new, geom = 'line',group = 1, ylab = "billion kg")
