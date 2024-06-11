library(readr)
library(readxl)
library(dplyr)

# Datasets.
basal <- read_excel("COVIDPersistent-basal.xlsx")
CP_E <- read_csv("COVIDPersistente-Seguimiento.csv")
CP_C <- read_csv("COVIDPersistent-Seguiment.csv")
descartes <- read_csv2("COVIDPersistente_descartes.csv")

# Descartes.
descartes$cip <- tolower(descartes$cip)
descartes$codi <- tolower(descartes$codi)
descartes$email <- tolower(descartes$email)
descartes$cip <- gsub("\\s+", "", descartes$cip)
descartes$codi <- gsub("\\s+", "", descartes$codi)

# Basal.
names(basal) <- gsub("actuals", "actual", names(basal))
basal$cip <- tolower(basal$cip)
basal$codi <- tolower(basal$codi)
basal$email <- tolower(basal$email)
basal$cip <- gsub("\\s+", "", basal$cip)
basal$codi <- gsub("\\s+", "", basal$codi)
cip_repetido <- na.omit(basal$cip)
cip_repetido <- unique(cip_repetido[duplicated(cip_repetido)])
codigo_repetido <- na.omit(basal$codi)
codigo_repetido <- unique(codigo_repetido[duplicated(codigo_repetido)])

# Cambios CP_E.
CP_E <- CP_E[CP_E$consentiment___1 == 1, ]
CP_E <- CP_E[CP_E$instrucciones_timestamp != "[not completed]", ]
CP_E <- CP_E[CP_E$covid19_persistente_encuesta_seguimiento_timestamp != "[not completed]", ]
CP_E <- CP_E[complete.cases(CP_E$covid19_persistente_encuesta_seguimiento_timestamp), ]
CP_E$instrucciones_timestamp <- as.POSIXct(CP_E$instrucciones_timestamp, 
                                           format = "%Y-%m-%d %H:%M:%S", tz = "")
CP_E$data <- as.Date(CP_E$instrucciones_timestamp)
CP_E <- CP_E[, !names(CP_E) %in% c("instrucciones_timestamp", "redcap_survey_identifier", 
                                   "consentiment___1", "instrucciones_complete",
                                   "covid19_persistente_encuesta_seguimiento_timestamp",
                                   "covid19_persistente_encuesta_seguimiento_complete")]

CP_E$codi_seguiment <- tolower(CP_E$codi_seguiment)
CP_E$codi_seguiment <- gsub("\\s+", "", CP_E$codi_seguiment)
valor_repetido <- CP_E[CP_E$codi_seguiment %in% codigo_repetido, ]
CP_E[CP_E$codi_seguiment %in% codigo_repetido, "codi_seguiment"] <- "sorayacoca@hotmail.com"

# Unificar los pacientes con el correo.
for (i in 1:nrow(CP_E)) {
  codigo_seguimiento <- CP_E[i, "codi_seguiment"]
  
  if (codigo_seguimiento %in% basal$cip) {
    icip <- match(codigo_seguimiento, basal$cip)
    correo <- basal$email[icip]
    CP_E[i, "codi_seguiment"] <- correo
  }
  
  else if (codigo_seguimiento %in% basal$codi) {
    icodi <- match(codigo_seguimiento, basal$codi)
    correo <- basal$email[icodi]
    CP_E[i, "codi_seguiment"] <- correo
  }
}

CP_E_filtrado <- CP_E[!CP_E$codi_seguiment %in% descartes$codi, ]
CP_E_filtrado <- CP_E_filtrado[!CP_E_filtrado$codi_seguiment %in% descartes$cip, ]
CP_E_filtrado <- CP_E_filtrado[!CP_E_filtrado$codi_seguiment %in% descartes$email, ]

# Buscamos los codigos sin correo.
codigos_sin_arroba <- CP_E_filtrado$codi_seguiment[!grepl("@", CP_E_filtrado$codi_seguiment)]
codigos_sin_arroba <- unique(codigos_sin_arroba)
codigos_sin_arroba

# Modificamos manualmente estos codigos.
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "dofu1979"] <- "veronicadoblas.cj@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "sigrid1963"] <- "siegrid.de@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "mónica13"] <- "momoxapi@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "davidfigueroamartinezgmail.com"] <- "davidfigueroamartinez@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "jgvs11"] <- "juditgilvernet@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "roca64"] <- "rosicg64@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "copa1965"] <- "corbacho416@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "fuvi5302"] <- "delafuentevil@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "mgr2210"] <- "mariagr2210@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "mul1963"] <- "marisa.urena.lopez@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "wenpal82"] <- "wen3100pza@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "wen82"] <- "wen3100pza@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "lepe17979"] <- "laura.leiro@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "vica1974"] <- "melecardiello@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "blgo1959"] <- "jesus_e_blanco@yahoo.es"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "loam1965"] <- "dollopa65@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "alpl1978"] <- "sap2_@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "crna1967"] <- "violeta_craviotto@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "goma1973"] <- "maiteusc@hotmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "mife1984"] <- "sara.emaleth@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "rori1958"] <- "riosjairo@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "jale1966"] <- "carlosjaimejuan@telefonica.net"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "gaar1955"] <- "maluigar@yahoo.es"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "loga1966"] <- "mpilarlop@hotmail.com"

CP_E_filtrado <- CP_E_filtrado[grep("@", CP_E_filtrado$codi_seguiment), ]

# Miramos si todos los correos son de la lista.
todos_presentes <- all(CP_E_filtrado$codi_seguiment %in% basal$email)
indices_no_presentes <- which(!(CP_E_filtrado$codi_seguiment %in% basal$email))
codigos_no_presentes <- CP_E_filtrado$codi_seguiment[indices_no_presentes]
print(codigos_no_presentes)

# Modificamos o elimanamos los correos.
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "ríosjairo@gmail.com"] <- "riosjairo@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "inesae1965@gmail.come"] <- "inesae1965@gmail.com"
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "dolorsgilh@gmail.com", ]
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "sgarciamo.bcn.ics@gencat,cat"] <- "sgarciamo.bcn.ics@gencat.cat"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "sergiomunozv@gencat.cat"] <- "sergiomunogv@hotmail.com"
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "erik_master650@hotmai.com", ]
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "eerikano4@hormail.com"] <- "erikano4@hotmail.com"
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "paulafelguera@gmail", ]
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "mtbelloso@yahoo.es"] <- "mtbelloso@gmail.com"
CP_E_filtrado$codi_seguiment[CP_E_filtrado$codi_seguiment == "carotamar@hotmail,com"] <- "carotamar@hotmail.com"
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "ecuadro@gencat.cat", ]
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "itrailip60@hotmail.com", ]
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "mplazas@adaptaliacorporacion.com", ]
CP_E_filtrado <- CP_E_filtrado[CP_E_filtrado$codi_seguiment != "rosamayorisaac@telefonica.ney", ]

# Verificamos que tenemos los codigos.
todos_presentes <- all(CP_E_filtrado$codi_seguiment %in% basal$email)

CP_E_filtrado <- CP_E_filtrado[, 2:175]

# Corregir meses de seguimento.
basal$data_inici_simptomes <- as.Date(basal$data_inici_simptomes)
CP_E_filtrado$data <- as.Date(CP_E_filtrado$data)
CP_E_filtrado$meses_transcurridos <- NA

for (i in 1:nrow(CP_E_filtrado)) {
  codigo <- CP_E_filtrado[i, "codi_seguiment"]
  indice <- match(codigo, basal$email)
  if (length(indice) > 0) {
    diferencia <- as.numeric(difftime(CP_E_filtrado$data[i], basal$data_inici_simptomes[indice]))
    diferencia_meses <- diferencia / 30    
    CP_E_filtrado[i, "meses_transcurridos"] <- diferencia_meses
  }
}

for (i in 1:nrow(CP_E_filtrado)) {
  if (CP_E_filtrado$meses_transcurridos[i] < 5.5) {
    CP_E_filtrado$mesos_seguiment[i] <- 1
  } else if (CP_E_filtrado$meses_transcurridos[i] >= 5.5 & CP_E_filtrado$meses_transcurridos[i] < 11.5) {
    CP_E_filtrado$mesos_seguiment[i] <- 2
  } else if (CP_E_filtrado$meses_transcurridos[i] >= 11.5 & CP_E_filtrado$meses_transcurridos[i] < 23) {
    CP_E_filtrado$mesos_seguiment[i] <- 3
  }else if (CP_E_filtrado$meses_transcurridos[i] >= 23 & CP_E_filtrado$meses_transcurridos[i] < 35) {
    CP_E_filtrado$mesos_seguiment[i] <- 4
  }else {
    CP_E_filtrado$mesos_seguiment[i] <- 5
  }
}

# Filtramos por  los años que queremos.
CP_E_final <- CP_E_filtrado %>%
  filter(mesos_seguiment %in% c(3, 4))

# Cambios CP_C.
CP_C <- CP_C[CP_C$consentiment___1 == 1, ]
CP_C <- CP_C[CP_C$instruccions_timestamp != "[not completed]", ]
CP_C <- CP_C[CP_C$covid19_persistent_enquesta_seguiment_timestamp != "[not completed]", ]
CP_C <- CP_C[complete.cases(CP_C$covid19_persistent_enquesta_seguiment_timestamp), ]
CP_C$instruccions_timestamp <- as.POSIXct(CP_C$instruccions_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "")
CP_C$data <- as.Date(CP_C$instruccions_timestamp)
CP_C <- CP_C[, !names(CP_C) %in% c("instruccions_timestamp", "redcap_survey_identifier", 
                                   "consentiment___1", "instruccions_complete",
                                   "covid19_persistent_enquesta_seguiment_timestamp",
                                   "covid19_persistent_enquesta_seguiment_complete")]

CP_C$codi_seguiment <- tolower(CP_C$codi_seguiment)
CP_C$codi_seguiment <- gsub("\\s+", "", CP_C$codi_seguiment)
valor_repetido2 <- CP_C[CP_C$codi_seguiment %in% codigo_repetido, ]
CP_C <- subset(CP_C, !(codi_seguiment %in% codigo_repetido))

# Unificamos el codigo por correo.
for (i in 1:nrow(CP_C)) {
  codigo_seguimiento <- CP_C[i, "codi_seguiment"]
  
  if (codigo_seguimiento %in% basal$cip) {
    icip <- match(codigo_seguimiento, basal$cip)
    correo <- basal$email[icip]
    CP_C[i, "codi_seguiment"] <- correo
  }
  
  else if (codigo_seguimiento %in% basal$codi) {
    icodi <- match(codigo_seguimiento, basal$codi)
    correo <- basal$email[icodi]
    CP_C[i, "codi_seguiment"] <- correo
  }
}

CP_C_filtrado <- CP_C[!CP_C$codi_seguiment %in% descartes$codi, ]
CP_C_filtrado <- CP_C_filtrado[!CP_C_filtrado$codi_seguiment %in% descartes$cip, ]
CP_C_filtrado <- CP_C_filtrado[!CP_C_filtrado$codi_seguiment %in% descartes$email, ]

# Buscamos los codigos sin correo.
CP_C_noemail <- CP_C_filtrado$codi_seguiment[!grepl("@", CP_C_filtrado$codi_seguiment)]
CP_C_noemail <- unique(CP_C_noemail)
CP_C_noemail

# Modificamos manualmente estos codigos.
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "flpr1956"] <- "merce.flix@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "roco1978"] <- "ester_rossell@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rado194122100"] <- "marttarams15@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rono1978"] <- "lidiarn@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "merca1965"] <- "neusmercader65@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "beve1975"] <- "soniabenitov@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "pema1976"] <- "susanyperalta@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "maco1975"] <- "espemartin7@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "ubca1975"] <- "eubasart@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rues750923007"] <- "vanescribano09@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cafa1910"] <- "laiacaminsf@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "nitzia1984"] <- "carme.egea.arranz@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "anor1581224002"] <- "carmenandresortiz@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "hema79"] <- "mont_hm@yahoo.es"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cero1700130003"] <- "scerro.cp.ics@gencat.cat"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "chca1976"] <- "carme.76@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "pemi1779"] <- "mirtins0@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "flpr0506"] <- "merce.flix@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "roba1959"] <- "grosquelles@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "maca1969"] <- "sec2008@hotmail.es"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "viji1965"] <- "neus_viso@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "coto1973"] <- "cosali13@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cega1963"] <- "reyescerdagaos@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "casi1965"] <- "ecsierra@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "mutr1961"] <- "mon22pilar@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cuto1948"] <- "montse.cuso@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "mdc1950"] <- "mcasade1@xtec.cat"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "sopa1980"] <- "montseacordionista@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "ribo1960"] <- "anaripollborras@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "hehe1984"] <- "greencomaband@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "pemi1960"] <- "fperales.lleida.ics@gencat.cat"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "feca1960"] <- "olfeca.60@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rino1962"] <- "ribas.rosa2@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "meca1965"] <- "neusmercader65@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rise131076"] <- "free_libertad@yahoo.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "siesco1974"] <- "silviaescoda@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "sufobo1969"] <- "sufobo@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "rogi1982"] <- "blanca.rgc@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "puso1974"] <- "sypuigdemasa@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "coli1964"] <- "coronadoliebana@yahoo.es"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "flex1711209008"] <- "aktualanna@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "jobr1976"] <- "malalt76@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "dope1971"] <- "ricard.domingo@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "oren1973"] <- "gloglortega@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "toor1972"] <- "rarosjabi@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "simo1968"] <- "carles.sis289@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cave1987"] <- "acarnes@gss.cat"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "gama1976"] <- "sgarcia@ambitcp.catsalut.net"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "cape1978"] <- "sandrusky78@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "glor1961"] <- "gloriamartido@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "loam1965"] <- "dollopa65@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "caca1965"] <- "mcanalc@uoc.edu"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "vica1974"] <- "melecardiello@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "gall1985"] <- "angelagarciallado@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "gama1979"] <- "gaspona@hotmail.com"

CP_C_filtrado <- CP_C_filtrado[grep("@", CP_C_filtrado$codi_seguiment), ]

# Miramos si todos los correos son de la lista.
todos_presentes2 <- all(CP_C_filtrado$codi_seguiment %in% basal$email)
indices_no_presentes2 <- which(!(CP_C_filtrado$codi_seguiment %in% basal$email))
codigos_no_presentes2 <- CP_C_filtrado$codi_seguiment[indices_no_presentes2]
codigos_no_presentes2 <- unique(codigos_no_presentes2)
print(codigos_no_presentes2)

# Modificamos o elimanamos los correos.
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "ghonrado@hotmail.com"] <- "ghonrado,@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "meri_85_13@hotmailcom"] <- "meri_85_13@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "gisela-144@hotmail.con"] <- "gisela-144@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "vircorreo@hotmail.cim"] <- "vircorreo@hotmail.com"
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "amaréisy@hotmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "casi0711alcasi93@hotmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "xciaurriz@icloud.com", ]
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "eva@foto1979"] <- "evafotollamas@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "pauferrejust@gmail.com"] <- "pferre6@xtec.cat"
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "onavarro.cc.ics@gencat.cat", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "mirage48isj@gmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "helennn14@gmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "pau.marc@hotmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "relo1985monica.recasens@gmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "giselagallgar@gmail.com", ]
CP_C_filtrado <- CP_C_filtrado[CP_C_filtrado$codi_seguiment != "teregoco80@gmail.com", ]
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "mserrabassa.cc.ics@gencat.cat"] <- "mserrabassa@hotmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "maco1975espemartin7@gmail.com"] <- "espemartin7@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "gibomo48@g.mail.com"] <- "gibomo48@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "visa.mcs@gmailcom"] <- "visa.mcs@gmail.com"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "fado1997-lfarreras@salutms.cat"] <- "lfarreras@salutms.cat"
CP_C_filtrado$codi_seguiment[CP_C_filtrado$codi_seguiment == "mari_x11@hormail.com"] <- "mari_x11@hotmail.com"

# Verificamos que tenemos los codigos.
todos_presentes2 <- all(CP_C_filtrado$codi_seguiment %in% basal$email)

CP_C_filtrado <- CP_C_filtrado[, 2:175]

# Corregir meses de seguimento.
basal$data_inici_simptomes <- as.Date(basal$data_inici_simptomes)
CP_C_filtrado$data <- as.Date(CP_C_filtrado$data)
CP_C_filtrado$meses_transcurridos <- NA
for (i in 1:nrow(CP_C_filtrado)) {
  codigo <- CP_C_filtrado[i, "codi_seguiment"]
  indice <- match(codigo, basal$email)
  
  if (length(indice) > 0) {
    diferencia <- as.numeric(difftime(CP_C_filtrado$data[i], basal$data_inici_simptomes[indice]))
    diferencia_meses <- diferencia / 30    
    CP_C_filtrado[i, "meses_transcurridos"] <- diferencia_meses
  }
}

for (i in 1:nrow(CP_C_filtrado)) {
  if (CP_C_filtrado$meses_transcurridos[i] < 5.5) {
    CP_C_filtrado$mesos_seguiment[i] <- 1
  } else if (CP_C_filtrado$meses_transcurridos[i] >= 5.5 & CP_C_filtrado$meses_transcurridos[i] < 11.5) {
    CP_C_filtrado$mesos_seguiment[i] <- 2
  } else if (CP_C_filtrado$meses_transcurridos[i] >= 11.5 & CP_C_filtrado$meses_transcurridos[i] < 23) {
    CP_C_filtrado$mesos_seguiment[i] <- 3
  }else if (CP_C_filtrado$meses_transcurridos[i] >= 23 & CP_C_filtrado$meses_transcurridos[i] < 35) {
    CP_C_filtrado$mesos_seguiment[i] <- 4
  }else {
    CP_C_filtrado$mesos_seguiment[i] <- 5
  }
}

# Filtramos por los años que queremos.
CP_C_final <- CP_C_filtrado %>%
  filter(mesos_seguiment %in% c(3, 4))

# Unificamos los dos idiomas.
df_combined <- bind_rows(CP_E_final, CP_C_final)

# Valores duplicados de paciente con los mismos meses de seguimiento.
df_combined <- df_combined %>% 
  arrange(desc(data)) 

# Conservamos el más reciente.
df_combined <- df_combined[!duplicated(df_combined[c("codi_seguiment", "mesos_seguiment")], fromLast = TRUE), ]

# Obtenemos una base de datos por cada seguimiento.
df_combined3 <- df_combined %>%
  filter(mesos_seguiment %in% c(3)) 
df_combined4 <- df_combined %>%
  filter(mesos_seguiment %in% c(4)) 

# Guardar el data frame en un archivo CSV.
write.csv(df_combined3, "COVIDPersistente-Seguimiento-1año.csv", row.names = FALSE)
write.csv(df_combined4, "COVIDPersistente-Seguimiento-2año.csv", row.names = FALSE)
write.csv(basal, "COVIDPersistent-basal.csv", row.names = FALSE)