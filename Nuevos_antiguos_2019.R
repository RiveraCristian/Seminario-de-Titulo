#################################################
#           Limpieza de "Nuevos_antiguos_2019"
#
#   Author : Cristian Rivera
#   Date   : 12-08-2023
################################################

#https://atm.amegroups.org/article/view/8839/9467

library(ggplot2)
library(readxl)
library(ggthemes)
library(dplyr)
library(stats)
library(writexl)


Nuevos_antiguos_2019 <- read_excel("C:/Users/pc/Desktop/Seminario de Investigacion/Bases EESS 2019 y 2020 - 2021.07.15 (1).xlsx",
                                   sheet = "Nuevos y antiguos encuesta 2019")
Nuevos_antiguos_2019 <- tail(Nuevos_antiguos_2019, nrow(Nuevos_antiguos_2019) - 2) #Filas Na

# A continuacion se seleccionan todas las Columnas necesarias.

Variables_De_Estudio <- c("ID",
                          "TIEMPO",
                          "REGION",
                          "DESEA_RESP",
                          "COLEG_CAT",
                          "RAZONUCM_PADRES",
                          "RAZONUCM_PROFESOR",
                          "RAZONUCM_REPUTACION",
                          "RAZONUCM_UBICACION",
                          "RAZONUCM_GASTOS",
                          "RAZONUCM_REC",
                          "RAZONUCM_RELIGION",
                          "RAZONUCM_RANKING",
                          "RAZONUCM_WEB",
                          "RAZONUCM_VISITA",
                          "RAZONUCM_CARRERA",
                          "RAZONUCM_AMIGOS",
                          "EESS_Q1",
                          "EESS_Q2",
                          "EESS_Q3",
                          "EESS_Q4",
                          "EESS_Q5",
                          "EESS_Q6",
                          "EESS_Q7",
                          "EESS_Q8",
                          "EESS_Q9",
                          "EESS_Q10",
                          "INTERES_Q1",
                          "INTERES_Q2",
                          "INTERES_Q3",
                          "INTERES_Q4",
                          "INTERES_Q5",
                          "INTERES_Q6",
                          "INTERES_Q7",
                          "INTERES_Q8",
                          "INTERES_Q9",
                          "EXPECT_Q1",
                          "EXPECT_Q2",
                          "EXPECT_Q3",
                          "EXPECT_Q4",
                          "EXPECT_Q5",
                          "INTERES_PART_Q1",
                          "INTERES_PART_Q2",
                          "INTERES_PART_Q3",
                          "PARTICIPACION_Q1",
                          "PARTICIPACION_Q2",
                          "PARTICIPACION_Q3",
                          "CREENCIA_DIOS",
                          "CONOCIMIENTO_Q1",
                          "CONOCIMIENTO_Q2",
                          "CONOCIMIENTO_Q3",
                          "CONOCIMIENTO_Q4",
                          "CONOCIMIENTO_Q5",
                          "RELIGION_HOGAR",
                          "FILIACION",
                          "ESTADO_EESS",
                          "NOMBRE_CARRERA",
                          "FACULTAD",
                          "SEDE ALUMNO",
                          "VIA DE INGRESO",
                          "POSTULANTE BEA",
                          "TIPO_SELECCIÓN",
                          "PUNTAJE_PPS",
                          "RAMA_EDUCA",
                          "GRUPO_DEPEN",
                          "PROM_NUMNOTAS",
                          "PTJE_NEM")

df_1 <- Nuevos_antiguos_2019[, Variables_De_Estudio]

#Ahora realizamos el primer filtro, para esto la variable DESEA_RESP nos indicara si la persona decicio o no responder la encuesta

df_1 <- df_1[df_1$DESEA_RESP == 1,]
df_1 <- df_1[!is.na(df_1$ESTADO_EESS),]
df_1 <- df_1[!is.na(df_1$EESS_Q10),]
df_1 <- df_1[!is.na(df_1$CONOCIMIENTO_Q5),]

#Ahora vamos con las variables academicas,

#PUNTAJE_PPS
plot(df_1$PUNTAJE_PPS)
df_1 <- df_1[df_1$PUNTAJE_PPS != 0, ]

summary(df_1$PUNTAJE_PPS) #En esta caso de permutara por la mediana, ya que no es sensible a los datos atipicos en este caso el "0"
Mediana_Puntaje_PPS <- median(df_1$PUNTAJE_PPS, na.rm = TRUE)
df_1$PUNTAJE_PPS[is.na(df_1$PUNTAJE_PPS)] <- Mediana_Puntaje_PPS
summary(df_1$PUNTAJE_PPS)
sd(df_1$PUNTAJE_PPS)

#Prom_numnotas
plot(df_1$PROM_NUMNOTAS)
summary(df_1$PROM_NUMNOTAS) #Promedio
media_prom_numnotas <- mean(df_1$PROM_NUMNOTAS, na.rm = TRUE)
df_1$PROM_NUMNOTAS[is.na(df_1$PROM_NUMNOTAS)] <- media_prom_numnotas
summary(df_1$PROM_NUMNOTAS)
sd(df_1$PROM_NUMNOTAS)

#PTJE NEM
plot(df_1$PTJE_NEM)
summary(df_1$PTJE_NEM)
Mediana_ptje_nem <- median(df_1$PTJE_NEM, na.rm =TRUE)
df_1$PTJE_NEM[is.na(df_1$PTJE_NEM)] <- Mediana_ptje_nem
summary(df_1$PTJE_NEM)
sd(df_1$PTJE_NEM)


#############################

#Grupo_Depen
table(df_1$GRUPO_DEPEN) # Cambiar el nombre por alguna categoria (Municipal = 1 , Particular pagado = 2, Particular suvencionado = 3) los 0 asignarle por la moda (en este caso 3)
df_1$GRUPO_DEPEN[df_1$GRUPO_DEPEN == "0"] <- "PARTICULAR SUBVENCIONADO"
df_1$GRUPO_DEPEN[df_1$GRUPO_DEPEN == "MUNICIPAL"] <- 0
df_1$GRUPO_DEPEN[df_1$GRUPO_DEPEN == "PARTICULAR PAGADO"] <- 1
df_1$GRUPO_DEPEN[df_1$GRUPO_DEPEN == "PARTICULAR SUBVENCIONADO"] <- 2
table(df_1$GRUPO_DEPEN)

df_1$GRUPO_DEPEN <- as.numeric(df_1$GRUPO_DEPEN)
summary(df_1$GRUPO_DEPEN)
Mediana_grupo_dep <- median(df_1$GRUPO_DEPEN, na.rm =TRUE)
df_1$GRUPO_DEPEN[is.na(df_1$GRUPO_DEPEN)] <- Mediana_grupo_dep
summary(df_1$GRUPO_DEPEN)
df_1$GRUPO_DEPEN <- as.character(df_1$GRUPO_DEPEN)
table(df_1$GRUPO_DEPEN)


#Rama_educacional
table(df_1$RAMA_EDUCA)

df_1$RAMA_EDUCA <- factor(
  ifelse(grepl("HUMANISTA", df_1$RAMA_EDUCA, ignore.case = TRUE), 1,
  ifelse(grepl("TÉCNICO", df_1$RAMA_EDUCA, ignore.case = TRUE), 2, 0)))

df_1$RAMA_EDUCA[df_1$RAMA_EDUCA == "Humanistas"] <- 1
df_1$RAMA_EDUCA[df_1$RAMA_EDUCA == "Otros"] <- 0
df_1$RAMA_EDUCA[df_1$RAMA_EDUCA == "Tecnicos"] <- 2
table(df_1$RAMA_EDUCA)
plot(df_1$RAMA_EDUCA)


#Tipo_seleccion
table(df_1$TIPO_SELECCIÓN)
df_1$TIPO_SELECCIÓN[df_1$TIPO_SELECCIÓN == 0] <- 'SELECCIONADO'
df_1$TIPO_SELECCIÓN[df_1$TIPO_SELECCIÓN == 'LISTA ESPERA'] <- 0
df_1$TIPO_SELECCIÓN[df_1$TIPO_SELECCIÓN == 'SELECCIONADO'] <- 1
table(df_1$TIPO_SELECCIÓN)

df_1$TIPO_SELECCIÓN <- as.numeric(df_1$TIPO_SELECCIÓN)
summary(df_1$TIPO_SELECCIÓN)
Mediana_TIPO_SELECCIÓN <- median(df_1$TIPO_SELECCIÓN, na.rm =TRUE)
df_1$TIPO_SELECCIÓN[is.na(df_1$TIPO_SELECCIÓN)] <- Mediana_TIPO_SELECCIÓN
summary(df_1$TIPO_SELECCIÓN)
df_1$TIPO_SELECCIÓN <- as.character(df_1$TIPO_SELECCIÓN)
table(df_1$TIPO_SELECCIÓN)


#Postulante BEA
table(df_1$`POSTULANTE BEA`) # 0 = no ; 1 = si
df_1$`POSTULANTE BEA`[df_1$`POSTULANTE BEA` == 'BEA'] <- 1
table(df_1$`POSTULANTE BEA`)

df_1$`POSTULANTE BEA` <- as.numeric(df_1$`POSTULANTE BEA`)
summary(df_1$`POSTULANTE BEA`)
Mediana_TIPO_POSTULANTE_BEA <- median(df_1$`POSTULANTE BEA`, na.rm =TRUE)
df_1$`POSTULANTE BEA`[is.na(df_1$`POSTULANTE BEA`)] <- Mediana_TIPO_POSTULANTE_BEA
summary(df_1$`POSTULANTE BEA`)
df_1$`POSTULANTE BEA` <- as.character(df_1$`POSTULANTE BEA`)
table(df_1$`POSTULANTE BEA`)


df_1$TIPO_SELECCIÓN <- as.numeric(df_1$TIPO_SELECCIÓN)
summary(df_1$TIPO_SELECCIÓN)
Mediana_TIPO_SELECCIÓN <- median(df_1$TIPO_SELECCIÓN, na.rm =TRUE)
df_1$TIPO_SELECCIÓN[is.na(df_1$TIPO_SELECCIÓN)] <- Mediana_TIPO_SELECCIÓN
summary(df_1$TIPO_SELECCIÓN)
df_1$TIPO_SELECCIÓN <- as.character(df_1$TIPO_SELECCIÓN)
table(df_1$TIPO_SELECCIÓN)

#Via de Ingreso
table(df_1$`VIA DE INGRESO`) #REVISAR
#Agregar codigo para que sea psu y otros
df_1$Ingreso <- ifelse(df_1$`VIA DE INGRESO` == "PSU", 1, 0)
table(df_1$Ingreso)

df_1$Ingreso <- as.numeric(df_1$Ingreso)
summary(df_1$Ingreso)
Mediana_TIPO_Ingreso <- median(df_1$Ingreso, na.rm =TRUE)
df_1$Ingreso[is.na(df_1$Ingreso)] <- Mediana_TIPO_Ingreso
summary(df_1$Ingreso)
df_1$Ingreso <- as.character(df_1$Ingreso)
table(df_1$Ingreso)

#Sede alumno
table(df_1$`SEDE ALUMNO`)
df_1$`SEDE ALUMNO`[df_1$`SEDE ALUMNO` == 'CURICO'] <- 0
df_1$`SEDE ALUMNO`[df_1$`SEDE ALUMNO` == 'TALCA'] <- 1
table(df_1$`SEDE ALUMNO`)

df_1$`SEDE ALUMNO` <- as.numeric(df_1$`SEDE ALUMNO`)
summary(df_1$`SEDE ALUMNO`)
Mediana_TIPO_sede <- median(df_1$`SEDE ALUMNO`, na.rm =TRUE)
df_1$`SEDE ALUMNO`[is.na(df_1$`SEDE ALUMNO`)] <- Mediana_TIPO_sede
summary(df_1$`SEDE ALUMNO`)
df_1$`SEDE ALUMNO` <- as.character(df_1$`SEDE ALUMNO`)
table(df_1$`SEDE ALUMNO`)


#Facultad
table(df_1$FACULTAD)
facultades <- c("CIENCIAS AGRARIAS Y FORESTALES" = 1,
                            "CIENCIAS BASICAS" = 2,
                            "CIENCIAS DE LA EDUCACIÓN" = 3,
                            "CIENCIAS DE LA INGENIERIA" = 4,
                            "CIENCIAS DE LA SALUD" = 5,
                            "CIENCIAS RELIGIOSAS Y FILOSOFICAS" = 6,
                            "CIENCIAS SOCIALES Y ECONOMICAS" = 7,
                            "MEDICINA" = 8)
df_1$FACULTAD <- as.integer(factor(df_1$FACULTAD, levels = names(facultades), labels = facultades))
table(df_1$FACULTAD)


df_1$FACULTAD <- as.numeric(df_1$FACULTAD)
summary(df_1$FACULTAD)
Mediana_TIPO_FACULTAD <- median(df_1$FACULTAD, na.rm =TRUE)
df_1$FACULTAD[is.na(df_1$FACULTAD)] <- Mediana_TIPO_FACULTAD
summary(df_1$FACULTAD)
df_1$FACULTAD <- as.character(df_1$FACULTAD)
table(df_1$FACULTAD)



  #Filiacion
table(df_1$FILIACION) #En esta ocacion las religiones empiezan por 1 hasta 6

# La 6 hace alusion a otros por lo tanto los 0 se asignaran a esta opcion.
df_1$FILIACION[df_1$FILIACION == 0] <- 6
table(df_1$FILIACION)

df_1$FILIACION <- as.numeric(df_1$FILIACION)
summary(df_1$FILIACION)
Mediana_TIPO_FILIACION <- median(df_1$FILIACION, na.rm =TRUE)
df_1$FILIACION[is.na(df_1$FILIACION)] <- Mediana_TIPO_FILIACION
summary(df_1$FILIACION)
df_1$FILIACION <- as.character(df_1$FILIACION)
table(df_1$FILIACION)


#RELIGION_HOGAR
table(df_1$RELIGION_HOGAR) #Se remplazara por la moda
df_1$RELIGION_HOGAR[df_1$RELIGION_HOGAR == 0] <- 2
table(df_1$RELIGION_HOGAR)


df_1$RELIGION_HOGAR <- as.numeric(df_1$RELIGION_HOGAR)
summary(df_1$RELIGION_HOGAR)
Mediana_TIPO_RELIGION_HOGAR <- median(df_1$RELIGION_HOGAR, na.rm =TRUE)
df_1$RELIGION_HOGAR[is.na(df_1$RELIGION_HOGAR)] <- Mediana_TIPO_RELIGION_HOGAR
summary(df_1$RELIGION_HOGAR)
df_1$RELIGION_HOGAR <- as.character(df_1$RELIGION_HOGAR)
table(df_1$RELIGION_HOGAR)


#Tiempo
df_1$TIEMPO <- as.numeric(df_1$TIEMPO)
mean(df_1$TIEMPO) #tiempo #Esta variable para informe, para modelo de ML NO
plot(df_1$TIEMPO)


#REGION
table(df_1$REGION) #
df_1 <- df_1 %>%
  mutate(REGION = case_when(
    REGION %in% c("01", "03", "04") ~ "Zona Norte",
    REGION %in% c("05", "CA", "NY", "VA", "39") ~ "Zona Metropolitana",
    REGION %in% c("07", "08", "10", "11", "12", "14", NA) ~ "Zona Sur",
    TRUE ~ "Zona Metropolitana"
  ))
table(df_1$REGION)




#COLEG_CAT
df_1$COLEG_CAT <- as.numeric(df_1$COLEG_CAT)
summary(df_1$COLEG_CAT) #Permuto por la media
hist(df_1$COLEG_CAT)
Mediana_COLEG_CAT <- median(df_1$COLEG_CAT, na.rm = TRUE)
df_1$COLEG_CAT[is.na(df_1$COLEG_CAT)] <- Mediana_COLEG_CAT
table(df_1$COLEG_CAT)

#CREENCIA_DIOS
df_1$CREENCIA_DIOS <- as.numeric(df_1$CREENCIA_DIOS) 
summary(df_1$CREENCIA_DIOS)
mediana_crencia_dios <- median(df_1$CREENCIA_DIOS, na.rm = TRUE)
df_1$CREENCIA_DIOS[is.na((df_1$CREENCIA_DIOS))] <- mediana_crencia_dios
summary(df_1$CREENCIA_DIOS)
table(df_1$CREENCIA_DIOS)

#Todas aquellas personas que no completaron la encuesta, sus respuestan seran remplazadas por un 0.

variables_encst <- c("RAZONUCM_PADRES",
                     "RAZONUCM_PROFESOR",
                     "RAZONUCM_REPUTACION",
                     "RAZONUCM_UBICACION",
                     "RAZONUCM_GASTOS",
                     "RAZONUCM_REC",
                     "RAZONUCM_RELIGION",
                     "RAZONUCM_RANKING",
                     "RAZONUCM_WEB",
                     "RAZONUCM_VISITA",
                     "RAZONUCM_CARRERA",
                     "RAZONUCM_AMIGOS",
                     "EESS_Q1",
                     "EESS_Q2",
                     "EESS_Q3",
                     "EESS_Q4",
                     "EESS_Q5",
                     "EESS_Q6",
                     "EESS_Q7",
                     "EESS_Q8",
                     "EESS_Q9",
                     "EESS_Q10",
                     "INTERES_Q1",
                     "INTERES_Q2",
                     "INTERES_Q3",
                     "INTERES_Q4",
                     "INTERES_Q5",
                     "INTERES_Q6",
                     "INTERES_Q7",
                     "INTERES_Q8",
                     "INTERES_Q9",
                     "EXPECT_Q1",
                     "EXPECT_Q2",
                     "EXPECT_Q3",
                     "EXPECT_Q4",
                     "EXPECT_Q5",
                     "INTERES_PART_Q1",
                     "INTERES_PART_Q2",
                     "INTERES_PART_Q3",
                     "PARTICIPACION_Q1",
                     "PARTICIPACION_Q2",
                     "PARTICIPACION_Q3",
                     "CONOCIMIENTO_Q1",
                     "CONOCIMIENTO_Q2",
                     "CONOCIMIENTO_Q3",
                     "CONOCIMIENTO_Q4",
                     "CONOCIMIENTO_Q5")

#Ahora con las variables de la encuesta iguales a 0, vamos a transformas todas estas varaibles a numericas para analisarlas y lograr reducir la cantidad de variables

df_2 <- na.omit(df_1[variables_encst, ])
df_3 <- df_1[rowSums(is.na(df_1[variables_encst, ])) == 0, ]
df_3 <- df_1[complete.cases(df_1[variables_encst, ]), ]

df_1 <- df_1[!is.na(df_1$CONOCIMIENTO_Q5),]
#df_1[variables_encst] <- mutate_all(df_1[variables_encst], list(~ ifelse(is.na(.), 0, .)))
summary(df_1[variables_encst])


df_1[variables_encst] <- lapply(df_1[variables_encst], as.numeric)
summary(df_1[variables_encst])

# Función para calcular la moda de una fila
calcular_moda_fila <- function(fila) {
  frecuencias <- table(fila)
  moda <- as.numeric(names(frecuencias)[which.max(frecuencias)])
  return(moda)
}




######
Razon_Ucm <- c("ID",
  "RAZONUCM_PADRES",
               "RAZONUCM_PROFESOR",
               "RAZONUCM_REPUTACION",
               "RAZONUCM_UBICACION",
               "RAZONUCM_GASTOS",
               "RAZONUCM_REC",
               "RAZONUCM_RELIGION",
               "RAZONUCM_RANKING",
               "RAZONUCM_WEB",
               "RAZONUCM_VISITA",
               "RAZONUCM_CARRERA",
               "RAZONUCM_AMIGOS")

Razon <- df_1[, Razon_Ucm]


# Crear una nueva variable con la moda de cada observación
df_1$Razon_moda <- apply(Razon, 1, calcular_moda_fila)




######
#Interes

Interes <- c("INTERES_Q1",
             "INTERES_Q2",
             "INTERES_Q3",
             "INTERES_Q4",
             "INTERES_Q5",
             "INTERES_Q6",
             "INTERES_Q7",
             "INTERES_Q8",
             "INTERES_Q9")

interes <- df_1[, Interes]

df_1$interes_moda <- apply(interes, 1, calcular_moda_fila)


#####
# EXPECT

expect <- c("EXPECT_Q1",
"EXPECT_Q2",
"EXPECT_Q3",
"EXPECT_Q4",
"EXPECT_Q5")

Expect <- df_1[, expect]

df_1$Expect_moda <- apply(Expect, 1, calcular_moda_fila)

 # EXPECT
#####
# INTERES_PART

interes_part <- c("INTERES_PART_Q1",
"INTERES_PART_Q2",
"INTERES_PART_Q3")

INTERES_PART <- df_1[,interes_part]

df_1$INTERES_PART_moda <- apply(INTERES_PART, 1, calcular_moda_fila)

 # INTERES_PART
#####
# Participacion

PARTICIPACION <- c("PARTICIPACION_Q1",
"PARTICIPACION_Q2",
"PARTICIPACION_Q3")

Participacion <- df_1[,PARTICIPACION]

df_1$PARTICIPACION_moda <- apply(Participacion, 1, calcular_moda_fila)

 # Participacion
#####
# CONOCIMIENTO

conocimiento <- c("CONOCIMIENTO_Q1",
"CONOCIMIENTO_Q2",
"CONOCIMIENTO_Q3",
"CONOCIMIENTO_Q4",
"CONOCIMIENTO_Q5")

CONOCIMIENTO <- df_1[,conocimiento]

df_1$CONOCIMIENTO_moda <- apply(CONOCIMIENTO, 1, calcular_moda_fila)

 # CONOCIMIENTO
######
#EESS

eess <- c("EESS_Q1",
"EESS_Q2",
"EESS_Q3",
"EESS_Q4",
"EESS_Q5",
"EESS_Q6",
"EESS_Q7",
"EESS_Q8",
"EESS_Q9",
"EESS_Q10")

EESS <- df_1[,eess]

df_1$EESS_moda <- apply(EESS, 1, calcular_moda_fila)

#EESS
#####

#ESPIRITU DE SERVICIO
#POR MOTODOLOGIA DEL AÑO 2019, el espiritu de servicio de cada estudiante es el promedio de cada individuo
#Por lo tanto la nueva variable es:

df_1$PROMEDIO_EESS <- rowMeans(EESS)

ggplot(df_1, aes(x = PROMEDIO_EESS)) +
  geom_bar() +
  labs(title = "Gráfico de Barras - Frecuencia de PROMEDIO_EESS",
       x = "PROMEDIO_EESS",
       y = "Frecuencia")
sd(df_1$PROMEDIO_EESS)
df_1$Cumplimiento_EESS <- ifelse(df_1$PROMEDIO_EESS < 4, 0, 1)
table(df_1$Cumplimiento_EESS)

Variables_De_Estudio_v2 <- c( "COLEG_CAT",
                          "RAZONUCM_PADRES",
                          "RAZONUCM_PROFESOR",
                          "RAZONUCM_REPUTACION",
                          "RAZONUCM_UBICACION",
                          "RAZONUCM_GASTOS",
                          "RAZONUCM_REC",
                          "RAZONUCM_RELIGION",
                          "RAZONUCM_RANKING",
                          "RAZONUCM_WEB",
                          "RAZONUCM_VISITA",
                          "RAZONUCM_CARRERA",
                          "RAZONUCM_AMIGOS",
                          "EESS_Q1",
                          "EESS_Q2",
                          "EESS_Q3",
                          "EESS_Q4",
                          "EESS_Q5",
                          "EESS_Q6",
                          "EESS_Q7",
                          "EESS_Q8",
                          "EESS_Q9",
                          "EESS_Q10",
                          "INTERES_Q1",
                          "INTERES_Q2",
                          "INTERES_Q3",
                          "INTERES_Q4",
                          "INTERES_Q5",
                          "INTERES_Q6",
                          "INTERES_Q7",
                          "INTERES_Q8",
                          "INTERES_Q9",
                          "EXPECT_Q1",
                          "EXPECT_Q2",
                          "EXPECT_Q3",
                          "EXPECT_Q4",
                          "EXPECT_Q5",
                          "INTERES_PART_Q1",
                          "INTERES_PART_Q2",
                          "INTERES_PART_Q3",
                          "PARTICIPACION_Q1",
                          "PARTICIPACION_Q2",
                          "PARTICIPACION_Q3",
                          "CREENCIA_DIOS",
                          "CONOCIMIENTO_Q1",
                          "CONOCIMIENTO_Q2",
                          "CONOCIMIENTO_Q3",
                          "CONOCIMIENTO_Q4",
                          "CONOCIMIENTO_Q5",
                          "RELIGION_HOGAR",
                          "FILIACION",
                          "FACULTAD",
                          "SEDE ALUMNO",
                          "Ingreso",
                          "POSTULANTE BEA",
                          "TIPO_SELECCIÓN",
                          "PUNTAJE_PPS",
                          "RAMA_EDUCA",
                          "GRUPO_DEPEN",
                          "PROM_NUMNOTAS",
                          "PTJE_NEM",
                          "Cumplimiento_EESS")

Data_v2 <- df_1[,Variables_De_Estudio_v2]
head(Data_v2)
write_xlsx(Data_v2, path = "BASE_MODELO_v2.xlsx")




Variables_De_Estudio_v4 <-c("EESS_Q1",
                            "EESS_Q2",
                            "EESS_Q3",
                            "EESS_Q4",
                            "EESS_Q5",
                            "EESS_Q6",
                            "EESS_Q7",
                            "EESS_Q8",
                            "EESS_Q9",
                            "EESS_Q10",
                            "CONOCIMIENTO_Q1",
                            "COLEG_CAT",
                            "RAZONUCM_RELIGION",
                            "PTJE_NEM",
                            "INTERES_PART_Q3",
                            "FILIACION",
                            "EXPECT_Q4",
                            "CONOCIMIENTO_Q4",
                            "PUNTAJE_PPS",
                            "RAZONUCM_WEB",
                            "CONOCIMIENTO_Q3",
                            "INTERES_Q7",
                            "RAZONUCM_GASTOS",
                            "INTERES_Q4",
                            "FACULTAD",
                            "RAZONUCM_RANKING",
                            "SEDE ALUMNO",
                            "Cumplimiento_EESS")

Data_v4 <- df_1[,Variables_De_Estudio_v4]
library(writexl)
write_xlsx(Data_v4, path = "BASE_MODELO_v3.xlsx")
  
  
