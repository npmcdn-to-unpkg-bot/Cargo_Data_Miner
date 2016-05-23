# define environment
mainDir = 'C:/Users/QIN_XUZ/Documents/CARGOIS'
dir.create(mainDir, showWarnings = FALSE)
## create folders if not exist
TEMPS = paste(mainDir,'Temporaire',sep = '/');dir.create(TEMPS, showWarnings = FALSE)
OUTPUT = paste(mainDir,'output',sep = '/');dir.create(OUTPUT, showWarnings = FALSE)
FLATFILE = paste(mainDir,'CARGOIS_raw',sep = '/');dir.create(FLATFILE, showWarnings = FALSE)
SCRIPTS = paste(mainDir,'Scripts',sep = '/');dir.create(SCRIPTS, showWarnings = FALSE)
ANALYSIS = paste(mainDir,'Analysis',sep = '/');dir.create(ANALYSIS, showWarnings = FALSE)
DATA = paste(mainDir,'DATA',sep = '/');dir.create(DATA, showWarnings = FALSE)

# load functions
source(file.path(SCRIPTS,'Data_input.R'))



choice.level = list("Region-Region"= 'R2R',
                    'Country-Region' ='C2R',
                    'Region-Country'='R2C',
                    'Country-Country'= 'C2C',
                    'Country-World'='C2W',
                    'World-Country'='W2C',
                    'Airport-Airport' = 'A2A',
                    'ALL' = 'ALL')
choice.year = list("2013" = 2013, '2014' = 2014, '2015'=2015, '2016'=2016, 'ALL'=NA)
