#RAW #
sps <- read.csv("data/cortesdips.csv")
spspres <- read.csv("data/cortespres.csv")

sps <- na.omit(sps)
spspres <- na.omit(spspres)

sps$url <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/DIP/", sps$JRV)
sps$presurl <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/PRE/", sps$PRESJRV, "_PRE/")  
spspres$presurl <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/PRE/", spspres$PRESJRV, "_PRE") 

# CONSOLIDADA #

consolidadoDips <- read_csv("data/consolidadoDips.csv")
consolidadoPres <- read_csv("data/consolidadoPres.csv")

consolidadoDips$id_candidatura <- 
  ifelse(consolidadoDips$id_candidatura == "BLC", yes = "15", no = consolidadoDips$id_candidatura)

consolidadoDips$id_candidatura <-
  ifelse(consolidadoDips$id_candidatura == "NUL", yes = "16", no = consolidadoDips$id_candidatura)

consolidadoDips$id_candidatura <- floor(as.numeric(consolidadoDips$id_candidatura))

# Quitar sufijos #

consolidadoPres$id_ubicacion <- str_remove(consolidadoPres$id_ubicacion, "_PRE")
consolidadoDips$id_ubicacion <- str_remove(consolidadoDips$id_ubicacion, "_DIP")

#Separar id_ubicacion para accesar solo numero de JRV

consolidadoDips <- consolidadoDips %>%
  separate(id_ubicacion, c("pais", "departamento", "municipio", "urbano", "centro", "jrv"))

consolidadoPres <- consolidadoPres %>%
  separate(id_ubicacion, c("pais", "departamento", "municipio", "urbano", "centro", "jrv"))

#Estandarizar para merge.

consolidadoDips$descripcion_candidatura <- str_replace(consolidadoDips$descripcion_candidatura, "Voto en BLANCO", "Blancos")
consolidadoDips$descripcion_candidatura <- str_replace(consolidadoDips$descripcion_candidatura, "Votos NULOS", "Nulos")

consolidadoPres$descripcion_candidatura <- str_replace(consolidadoPres$descripcion_candidatura, "Voto en BLANCO", "Blancos")
consolidadoPres$descripcion_candidatura <- str_replace(consolidadoPres$descripcion_candidatura, "Votos NULOS", "Nulos")


codDepts <- read.csv("casillaspordept.csv")

rm (spspres, sps)

#############

# Agregarle Descripcion Partido a ConsolidadoDips #

codPartidos <- read.csv("codigos partidos.csv")
consolidadoDips <- left_join(consolidadoDips, codPartidos, by = c("id_candidatura" = "id_partidoDip"))

rm(codPartidos)

#################################################
### Aislar Votos Nulos y Blancos de Diputados ###
#################################################

blancos_nulos <-
  consolidadoDips %>%  
  filter(descripcion_candidatura == "Blancos" | descripcion_candidatura =="Nulos") %>%
  select(jrv, descripcion_candidatura, cant_votos)

blancosnulosmelt <- dcast(melt(as.data.table(blancos_nulos), id.vars = c("descripcion_candidatura", "jrv")), 
                          jrv + variable ~ descripcion_candidatura, value.var = "value")

rm(blancos_nulos)

######################################
### Calcular Planchas por Partido ###
#####################################

planchas <- 
  consolidadoDips %>%
  group_by(jrv, partidoDip) %>%
  summarise (planchas = min(cant_votos)) %>%
  filter (partidoDip != "Blancos") %>%
  filter (partidoDip != "Nulos")


####################################################
### Encontrar el Diputado mas Votado en una JRV ###
###################################################
soloVotantes_pres <- unique(select(consolidadoPres, jrv, cant_votantes))
soloVotantes_dips <- unique(select(consolidadoDips, jrv, cant_votantes))

mas_votado <- 
  consolidadoDips %>%
  filter (partidoDip != "Blancos") %>%
  filter (partidoDip != "Nulos") %>%
  group_by(jrv, partidoDip) %>%
  summarise(masVotado = max (cant_votos), total_marcas_partido = sum(cant_votos)) 

mas_votado <- left_join(mas_votado, planchas, keep = FALSE)
mas_votado <- left_join(mas_votado, blancosnulosmelt, keep = FALSE)
mas_votado$variable <- NULL

mas_votado <- left_join(mas_votado, soloVotantes_pres, keep = FALSE)

## Calcula Numero de Papeletas Validas usando Data de ConsolidadoDips y ConsolidadoPres ##

mas_votado$papeletas_validas <- (mas_votado$cant_votantes - mas_votado$Nulos - mas_votado$Blancos)

#### Transpose MAs_Votados para explorar ###

mas_votado_melt <- dcast(melt(as.data.table(mas_votado), id.vars = c("partidoDip", "jrv")), 
                         jrv + variable ~ partidoDip, value.var = "value")

planchas_transposed <- 
  mas_votado_melt %>%
  filter(variable == "planchas")

planchas_transposed <- left_join(planchas_transposed, mas_votado, keep = FALSE) 

planchas_transposed$partidoDip <- NULL
planchas_transposed$total_marcas_partido <- NULL
planchas_transposed$masVotado <- NULL
planchas_transposed$planchas <- NULL

#######################################################################
### Encontrar Votos Maximos Permitidos Por 4 Partidos Principales ####
######################################################################

violacion <- planchas_transposed

violacion$max_pn <- 
  violacion$papeletas_validas - violacion$`PARTIDO ALIANZA PATRIOTICA HONDURENA` -
  violacion$`PARTIDO ANTICORRUPCION` - violacion$`PARTIDO DE CENTRO SOCIAL CRISTIANO VA; MOVIMIENTO SOLIDARIO` - 
  violacion$`PARTIDO DEMOCRATA CRISTIANO DE HONDURAS` - violacion$`PARTIDO FRENTE AMPLIO (EL FRENTE)` - 
  violacion$`PARTIDO LIBERACION DEMOCRATICO DE HONDURAS` - violacion$`PARTIDO LIBERAL DE HONDURAS` - 
  violacion$`PARTIDO LIBERTAD Y REFUNDACION` - violacion$`PARTIDO NUEVA RUTA DE HONDURAS` - violacion$`PARTIDO TODOS SOMOS HONDURAS` - 
  violacion$`PARTIDO UNIFICACION DEMOCRATICA` - violacion$PINU - violacion$PSH

violacion$max_plh <- 
  violacion$papeletas_validas - violacion$`PARTIDO ALIANZA PATRIOTICA HONDURENA` -
  violacion$`PARTIDO ANTICORRUPCION` - violacion$`PARTIDO DE CENTRO SOCIAL CRISTIANO VA; MOVIMIENTO SOLIDARIO` - 
  violacion$`PARTIDO DEMOCRATA CRISTIANO DE HONDURAS` - violacion$`PARTIDO FRENTE AMPLIO (EL FRENTE)` - 
  violacion$`PARTIDO LIBERACION DEMOCRATICO DE HONDURAS` - violacion$`PARTIDO NACIONAL DE HONDURAS` - 
  violacion$`PARTIDO LIBERTAD Y REFUNDACION` - violacion$`PARTIDO NUEVA RUTA DE HONDURAS` - violacion$`PARTIDO TODOS SOMOS HONDURAS` - 
  violacion$`PARTIDO UNIFICACION DEMOCRATICA` - violacion$PINU - violacion$PSH

violacion$max_libre <- 
  violacion$papeletas_validas - violacion$`PARTIDO ALIANZA PATRIOTICA HONDURENA` -
  violacion$`PARTIDO ANTICORRUPCION` - violacion$`PARTIDO DE CENTRO SOCIAL CRISTIANO VA; MOVIMIENTO SOLIDARIO` - 
  violacion$`PARTIDO DEMOCRATA CRISTIANO DE HONDURAS` - violacion$`PARTIDO FRENTE AMPLIO (EL FRENTE)` - 
  violacion$`PARTIDO LIBERACION DEMOCRATICO DE HONDURAS` - violacion$`PARTIDO NACIONAL DE HONDURAS` - 
  violacion$`PARTIDO LIBERAL DE HONDURAS` - violacion$`PARTIDO NUEVA RUTA DE HONDURAS` - violacion$`PARTIDO TODOS SOMOS HONDURAS` - 
  violacion$`PARTIDO UNIFICACION DEMOCRATICA` - violacion$PINU - violacion$PSH

violacion$max_psh <- 
  violacion$papeletas_validas - violacion$`PARTIDO ALIANZA PATRIOTICA HONDURENA` -
  violacion$`PARTIDO ANTICORRUPCION` - violacion$`PARTIDO DE CENTRO SOCIAL CRISTIANO VA; MOVIMIENTO SOLIDARIO` - 
  violacion$`PARTIDO DEMOCRATA CRISTIANO DE HONDURAS` - violacion$`PARTIDO FRENTE AMPLIO (EL FRENTE)` - 
  violacion$`PARTIDO LIBERACION DEMOCRATICO DE HONDURAS` - violacion$`PARTIDO NACIONAL DE HONDURAS` - 
  violacion$`PARTIDO LIBERAL DE HONDURAS` - violacion$`PARTIDO NUEVA RUTA DE HONDURAS` - violacion$`PARTIDO TODOS SOMOS HONDURAS` - 
  violacion$`PARTIDO UNIFICACION DEMOCRATICA` - violacion$PINU - violacion$`PARTIDO LIBERTAD Y REFUNDACION`


### Unir Mas Votados con Maximos Permitidos ###

irregulares <- 
  select(violacion,
         jrv, cant_votantes, Blancos, Nulos, papeletas_validas,
         max_pn, max_libre, max_plh, max_psh)

irregulares <- unique(irregulares)


### Aislar mas Votados por Partido para Agregar a Irregulares

mas_votado_principales <-
  filter(mas_votado_melt, variable == "masVotado")

mas_votado_principales <-
  select(mas_votado_principales, jrv,
         `PARTIDO NACIONAL DE HONDURAS`,
         `PARTIDO LIBERTAD Y REFUNDACION`,
         `PARTIDO LIBERAL DE HONDURAS`,
         PSH
  )

### Unir a Irregulares ###

irregulares <- left_join(irregulares, mas_votado_principales, keep = FALSE)

irregulares$violacion <- "Ninguno"
irregulares$violacion <- ifelse(irregulares$`PARTIDO NACIONAL DE HONDURAS` > irregulares$max_pn,
                                yes = "PN", no = irregulares$violacion)

irregulares$violacion <- ifelse(irregulares$`PARTIDO LIBERTAD Y REFUNDACION` > irregulares$max_libre,
                                yes = "LIBRE", no = irregulares$violacion)


irregulares$violacion <- ifelse(irregulares$`PARTIDO LIBERAL DE HONDURAS` > irregulares$max_plh,
                                yes = "PLH", no = irregulares$violacion)


irregulares$violacion <- ifelse(irregulares$PSH > irregulares$max_psh,
                                yes = "PSH", no = irregulares$violacion)

irregulares <- filter(irregulares,
                      violacion != "Ninguno")


### Agregar Links ###

jrvs <- unique(select(consolidadoDips,
                      departamento, municipio, urbano, centro, jrv))

jrvs$address <- paste0("HN.", jrvs$departamento, ".", jrvs$municipio, ".", jrvs$urbano, ".", jrvs$centro, ".", jrvs$jrv)
jrvs$URL <- paste0("https://resultadosgenerales2021.cne.hn/#resultados/DIP/", jrvs$address, "_DIP")
jrvs$foto <- paste0("https://provisorio-honduras-2021.datosoficiales.com/opt/recuentos/mesa-", jrvs$jrv, "_DIP.jpg")

##############################
### Finalizar Irregulares ###
#############################

violaciones_plancha <- select (irregulares,
                               JRV = jrv,
                               papeletas_validas,
                               
                               max_posible_pn = max_pn,
                               mas_votado_pn = `PARTIDO NACIONAL DE HONDURAS`,
                               
                               max_posible_libre = max_libre,
                               mas_votado_libre = `PARTIDO LIBERTAD Y REFUNDACION`,
                               
                               max_posible_plh = max_plh,
                               mas_votado_plh = `PARTIDO LIBERAL DE HONDURAS`,
                               
                               max_posible_psh = max_psh,
                               mas_votado_psh = PSH)

violaciones_plancha <- left_join(violaciones_plancha, jrvs, by = c("JRV" = "jrv"), keep=FALSE)

######################
### Para Finalizar ###
#####################

violaciones_final <- select(violaciones_plancha,
                            JRV,
                            Direccion = URL,
                            Imagen = foto)

Sys.sleep(2)

view(violaciones_plancha)


