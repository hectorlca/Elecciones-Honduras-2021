# Cargar Paquetes

library (jsonlite)
library (plyr)
library (dplyr)
library (httr)
library (RSelenium)
library (rvest)
library (xml2)




### Importar Datos ###

sps <- read.csv("data/cortesdips.csv")
spspres <- read.csv("data/cortespres.csv")
sps <- na.omit(sps)
spspres <- na.omit(spspres)
sps$url <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/DIP/", sps$JRV)
sps$presurl <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/PRE/", sps$PRESJRV, "_PRE/")  
spspres$presurl <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/PRE/", spspres$PRESJRV, "_PRE") 

#####################
##### SELENIUM ######
#####################

driver <- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]


#### LOOP CSV DIPS ####

for(i in 1:nrow(sps))  {
  remDr$navigate(paste0(sps$url[[i]]))
  Sys.sleep(2)
  buttonDL <- remDr$findElement(using = "link text", "CSV")
  buttonDL$clickElement()
  Sys.sleep(1)
}

#### LOOP CSV PRES ####




for(i in 1:nrow(spspres))  {
  remDr$navigate(paste0(spspres$presurl[[i]]))
  Sys.sleep(1)
  buttonDL <- remDr$findElement(using = "link text", "CSV")
  buttonDL$clickElement()
  Sys.sleep(1)
}

### Concatenate JSON ###

pre.json <- fromJSON("C:/Users/hecto/Desktop/JRV/HN.8.1.1.318.9910_PRE_PRE.json")
dip.csv <- read_csv("C:/Users/hecto/Desktop/JRV/HN.8.1.1.318.9910_DIP_DIP.csv")
dip.csv$votospres <- pre.json$cant_votantes
 




