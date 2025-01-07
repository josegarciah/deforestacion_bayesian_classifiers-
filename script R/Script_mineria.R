###########################################
#---Script para calculo de epsilon y score
###########################################

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(spatialEco)
#library(rgl)
library(maptools)
library(ggplot2)
library(sf)

#setwd("C:/Users/José/Documents/datos_madera/2020") #directorio
setwd("C:/Users/José/Documents/datos_madera/30forest/clases_corr")
### leer archivos
rejilla<-st_read("Municipios_ID.shp")
#rejilla<-st_read("mun23cw.shp")

#biomasa<-st_read("loss_propor.shp")
#rejilla <- readOGR(dsn= ".", layer = "Municipios_ID") #rejilla regular o una division politica
#plot(rejilla)
#biomasa<-readOGR(dsn= ".", layer = "biomas") #mapa de biomasa categorizado
#variables<-read.csv("covariable2020_30sva.csv", header = T) # covariables. son las de de inegi que estan en epipuma
#variables<-read.csv("covariables2020_30.csv", header = T)
variables<-read.csv("covariables_c.csv", header = T)
#covariable2020_11
##############
##------para este ejempo hice estas categorias:
##############
#nameID	                val_p
#AGB_DAAC_Mexico.p01	[0,90)
#AGB_DAAC_Mexico.p02	[90,133)
#AGB_DAAC_Mexico.p03	[133,182)
#AGB_DAAC_Mexico.p04	[182,262)
#AGB_DAAC_Mexico.p05	[262,564)
#AGB_DAAC_Mexico.p06	[564,1445)
#AGB_DAAC_Mexico.p07	[1445,3082)
#AGB_DAAC_Mexico.p08	[3082,5105)
#AGB_DAAC_Mexico.p09	[5105,7708)
#AGB_DAAC_Mexico.p10	[7708,25896]

#Para un modelo se puede sumar el total de biomasa por municipio y seleccionar el decil mas alto de muncipios
#El modelo seria: que variables se correlacionan significativamente con los municipios donde hay mayor biomasa
#Otro modelo, categorizar la variable (como el ejemplo) y asignar en que municipios se presentan estas categorias
# aqui pueden haber mas de una categoria por municipio. 
#Este segundo modelo sera el ejemplo y se busca que variables se correlacionan con las categorias

##############
##---asignar el ID de celdas (en este caso municipios) a los valores de biomasa
#a<-point.in.poly(biomasa, rejilla, duplicate = F) #intersecci?n
#a2<-as.data.frame(a)                              #pasarlo a tabla
#head(a2)
#selecciono la clase (val_p y pid1)y el id para contar las coocurrencias.
#Nota: Las columnas de coordenadas solo las deje por la estructura del script de calculos, pero si estan vacias no pasa nada
#a3<-dplyr::select(biomasa,val_p, coords.x1, coords.x2, pid1)  
#write.csv(a3, "biomasa_ID.csv", row.names = F) # puede guardarse la asignacion de ID a biomasa para despues solo llamar la tabla

a4<-read.csv("clase_mun.csv") # leo el archivo de biomasa con us ID
a4<-as.data.frame(a4)
#bosque
a4<-read.csv("bosque2000_mun.csv")
a4<-as.data.frame(a4)
###############################################################
##--- ESte es el script para calculos

# aqui indexo las tablas para el analisis, la tabla vector es la Clase, la tabla mamiferos son las covariables
# dejo esos nombres porque asi estan en el script y no lo he cambiado a algo mas generico

vectores<-a4  
mamiferos<-variables

Ncells<-length(rejilla$CVEGEO)  # es la N total de celdas para el calculo de epsilon y score


names(vectores) <- c("especie","long","lat","Id")
# Nombre de las espcies de vector
species_vec <- sort(unique(vectores$especie))

#-------------------------------------------------------------------------------------------
# Función para contar el número de ocurrencias no repetidas de una especie en 
# una base de datos. Regresa un vector del número de occurrencias no repetidas 
# en la base de datos dat.
# Los argumentos de la función son:
#   x: Nombre de la especie 
#   dat: Base de datos con las ocurrencias y un Id del pixel.
#   ret: Argumento donde se especifica si se regresa la 
#        base de datos limpia ("data") o solamente el conteo de 
#        número de ocurrencias únicas de la especie ("count").
#-------------------------------------------------------------------------------------------

count_data <- function(x,dat,ret=c("data","count")){
  unicos <- which(dat$especie == x)
  bd_sp <- dat[unicos,]
  sp_uni <- bd_sp[which(duplicated(bd_sp$Id) == FALSE),]
  if(ret== "data"){
    # Data.frame vac????o 
    dat2 <- data.frame(especie=c(),lon=c(),lat=c(),Id=c())
    dat2<- rbind(dat2,sp_uni)
    return(list(dat2))
  }
  else if(ret== "count"){  	
    cuent <- dim(sp_uni)[1]
    return(cuent)
  }
}
# Conteo de ocurrencias únicas de la especie de vector
cuent_vec <- mapply(FUN=count_data,species_vec,
                    MoreArgs=list(dat=vectores,ret="count"))
names(cuent_vec) <- species_vec

# Datos limpios con ocurrencias únicas en forma de lista
datos_vec <- mapply(FUN=count_data,species_vec,
                    MoreArgs=list(dat=vectores,ret="data"))
# Datos limpios con ocurrencias únicas en forma de data.frame
# datos_vec <- ldply (datos_vec, data.frame)
datos_vec <- do.call(rbind.data.frame, datos_vec)

#-------------------------------------------------------------------------------------------
# Datos mamiferos
#-------------------------------------------------------------------------------------------
names(mamiferos) <- c("especie","long","lat","Id")
species_mami <- sort(unique(mamiferos$especie))

#-----------------------------------------------------

# Conteo de ocurrencias únicas de la especie de mam????feros
cuent_mami <- mapply(FUN=count_data,species_mami,
                     MoreArgs=list(dat=mamiferos,ret="count"))
names(cuent_mami) <- species_mami
# Datos limpios con ocurrencias únicas en forma de lista
datos_mami <- mapply(FUN=count_data,species_mami,
                     MoreArgs=list(dat=mamiferos,ret="data"))
# Datos limpios con ocurrencias únicas en forma de data.frame
# datos_mami <- ldply (datos_mami, data.frame)
datos_mami <- do.call(rbind.data.frame, datos_mami)

#str(datos_mami)
# Combinaciones entre la base de datos de vectores y mam????feros
# utilizando como criterio la Id (número de pixel)
datos_mam_vec <- merge(datos_vec,datos_mami,c("Id"), all=T)


tabla <- table(datos_mam_vec[,c(5,2)])# ,1)])
freqs <- as.data.frame(tabla)
freqs2 <- freqs[,c(2,1,3)]
#freqs3 <- freqs[which(freqs$Freq != 0),]
dim(freqs2)

freqs2$count_vec <- NA
freqs2$count_mam <- NA

c_vec <- function(x) {
  da_vec <- which(freqs2$especie.x == species_vec[x])
  return(da_vec)
}
f_vec <- lapply(1:length(species_vec),c_vec)

c_mam <- function(x) {
  da_mam <- which(freqs2$especie.y == species_mami[x])
  return(da_mam)
}
f_mam <- lapply(1:length(species_mami),c_mam)

for(i in 1:length(f_vec)){
  freqs2$count_vec[f_vec[[i]]] <- cuent_vec[i]
}

for(i in 1:length(f_mam)){
  freqs2$count_mam[f_mam[[i]]] <- cuent_mami[i]
}

epsilon <- function(intersection,count_vec,count_mam,N){
  num <- (count_mam*((intersection/count_mam)-(count_vec/N)))
  dem <-  sqrt((count_mam*(count_vec/N)*(1-(count_vec/N))))
  return(num/dem)
}

score <- function(intersection,count_vec,count_mam,N){
  px_c <- (intersection+0.005)/(count_vec+0.01)
  px_nc <- (count_mam-intersection+0.01)/(N-count_vec+0.005)
  score <- log((px_c/px_nc))
  return(score)
}

freqs2$epsilon <- epsilon(freqs2$Freq,count_vec = freqs2$count_vec,count_mam = freqs2$count_mam,N = Ncells)
freqs2$score <- score(freqs2$Freq,count_vec = freqs2$count_vec,count_mam = freqs2$count_mam,N = Ncells)

names(freqs2)<-c("clase", "factor", "Nij", "Ni", "Nj", "epsilon", "score")

# Escribo el archivo del output final en csv
write.csv(freqs2,"eps_score_loss30_ha_c.csv",row.names=F)
#write.csv(freqs2,"eps_score_loss30_ha_area.csv",row.names=F)
write.csv(freqs2,"eps_score_loss30_mun_c.csv",row.names=F)
write.csv(freqs2,"eps_score_loss30_bosque_c.csv",row.names=F)

########################################################################
##----------- mapear
###################################################################

##---- seleccionamos la clase a mapear



##---- seleccionamos la clase a mapear

freqs2 %>% 
  filter(clase =="top10")->C1


##----- asignar el score a cada variable           

#score<-merge(variables, C1, by.x="especievalida", by.y="factor", all= T)
####IVAN BORRAR SI NO FUNCIONA####
variables<-variables %>% 
  rename(factor=especievalida)

score<-left_join(variables, C1, by="factor")



#head(score)
##----- suma de score por municipio CVEGEO
score_sum<-aggregate(score$score,list(score$ET_ID),sum)

###---- unir score al mapa

map_score<-merge(rejilla, score_sum, by.x= "CVEGEO", by.y="Group.1")

###----- graficar el mapa del modelo

b<-ggplot(map_score) +
  geom_sf(aes(fill = x), color = NA)+
  labs(title = "TOP 10% FOREST LOSS ", fill="Score") +
  scale_fill_distiller(palette = "Spectral")

ggsave("map_mun.png", b, width = 10, height = 6, dpi = 300 )

##--- salva el modelo como shapefile
st_write(map_score,"modelo_score_clima.shp", delete_layer = TRUE)

#plot(rejilla)


freqs2 %>% 
  filter(clase =="top10")->C

C %>% 
filter( epsilon > 2)->C1

C %>% 
  filter( epsilon < -2)->C2

C3 <- bind_rows(C1, C2)
##----- asignar el score a cada variable   
variables<-variables %>% dplyr::rename(factor=especievalida)

##----- suma de score por municipio
score1<-left_join(variables, C, by ="factor")

names(top)<-c("clase", "x", "y", "ET_ID")

score_sum_total<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
write.csv(score_sum_total, "score_total.csv")


CS <- score1[grepl("^Social", score1$factor), ]
CE <- score1[grepl("^Economic", score1$factor), ]
CP <- score1[grepl("^Politic", score1$factor), ]
CF <- score1[grepl("^Forest", score1$factor), ]
CB <- score1[grepl("^Bio", score1$factor), ]

##----- suma de score por municipio y filtro
score1<-left_join(variables, CS, by ="factor")
#sumatorias 
score_sum_dos<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_social<-aggregate(score1$score,list(score1$ET_ID),sum)
score_sum_economic<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_politic<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_forestal<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_clima<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_total<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)

###todo el modelo 

loss<-cbind(c$CVEGEO, c$c1, score_sum2$x, score_sum_2$x, 
            score_sum_social$x, score_sum_economic$x, score_sum_politic$x, score_sum_forest$x, score_sum_clima$x, score_sum_total$x)
colnames(loss)<-c("CVEGEO", "c1", "score2", "score_2", 
                  "score_social", "score_economic", "score_politic", "score_forest", "score_clima", "score_total")
loss










freqs2  %>% 
#filter(epsilon > 2)->C1
filter(epsilon < 2)->C2

CS <- freqs2[grepl("^Social", freqs2$factor), ]
CE <- freqs2[grepl("^Economic", freqs2$factor), ]
CP <- freqs2[grepl("^Politic", freqs2$factor), ]
CF <- freqs2[grepl("^Forest", freqs2$factor), ]
CB <- freqs2[grepl("^Bio", freqs2$factor), ]

freqs2 %>% 
  filter(clase =="top10")->CT
##----- asignar el score a cada variable           
variables<-variables %>% dplyr::rename(factor=especievalida)

score1<-left_join(variables, C1, by ="factor")
score2<-left_join(variables, C2, by ="factor")
scoreCS<-left_join(variables, CS, by ="factor")
scoreCP<-left_join(variables, CE, by ="factor")
scoreCF<-left_join(variables, CF, by ="factor")
scoreCB<-left_join(variables, CB, by ="factor")
scoreCT<-left_join(variables, CT, by ="factor")
head(score)

##----- suma de score por municipio
score_sum2<-aggregate(score1$score,list(score1$ET_ID),sum, na.rm=TRUE)
score_sum_2<-aggregate(score2$score,list(score2$ET_ID),sum, na.rm=TRUE)
score_sum_social<-aggregate(scoreCS$score,list(scoreCS$ET_ID),sum, na.rm=TRUE)
score_sum_economic<-aggregate(scoreCE$score,list(scoreCE$ET_ID),sum, na.rm=TRUE)
score_sum_politic<-aggregate(scoreCP$score,list(scoreCP$ET_ID),sum, na.rm=TRUE)
score_sum_forest<-aggregate(scoreCF$score,list(scoreCF$ET_ID),sum, na.rm=TRUE)
score_sum_clima<-aggregate(scoreCB$score,list(scoreCB$ET_ID),sum, na.rm=TRUE)
score_sum_total<-aggregate(scoreCT$score,list(scoreCT$ET_ID),sum, na.rm=TRUE)
#score_sum3<-aggregate(score$score,list(score$ET_ID),sum)
###---- unir score al mapa

map_score<-merge(rejilla, score_sum_total, by.x= "CVEGEO", by.y="Group.1")

###----- graficar el mapa del modelo

ggplot(map_score) +
geom_sf(aes(fill = x), color = NA)+
  labs(title = "C3 menos 10% Mun mayor pérdida de bosque 2020 %bosque 2000", fill="Score") +
  scale_fill_distiller(palette = "Spectral")


##--- salva el modelo como shapefile
st_write(map_score,"modelo_score_climatop.shp", delete_layer = TRUE)


###grafica de puntos####
library(dplyr)

c<-read.csv("C:/Users/José/Documents/datos_madera/30forest/clases_corr/clase_ha.csv")
#top<- filter(c, c1=="top10")

loss<-cbind(c$CVEGEO, c$c1, score_sum2$x, score_sum_2$x, 
            score_sum_social$x, score_sum_economic$x, score_sum_politic$x, score_sum_forest$x, score_sum_clima$x, score_sum_total$x)
colnames(loss)<-c("CVEGEO", "c1", "score2", "score_2", 
                  "score_social", "score_economic", "score_politic", "score_forest", "score_clima", "score_total")
loss

##BIEN 
lf<-cbind(score_sum2$Group.1, score_sum2$x, score_sum_2$x, 
                score_sum_social$x, score_sum_economic$x, score_sum_politic$x, score_sum_forest$x, score_sum_clima$x, score_sum_total$x)
colnames(lf)<-c("CVEGEO", "score2", "score_2", 
                  "score_social", "score_economic", "score_politic", "score_forest", "score_clima", "score_total")


write.csv(lf,"sum_score_bosque_roc.csv")
write.csv(score_sum_economic,"economic.csv")

clase_score<-read.csv("sum_score.csv")

library(ggplot2)

ggplot(clase_score, aes(x = score_c1, y = loss_ha)) +
  geom_point(colour = 4)

library(ggrepel)
library(ggpmisc)
my.formula <- y  ~ x 
ggplot (clase_score, aes (score_c3, bosque)) +
  geom_point (colour= "red") +
  geom_text_repel ( aes (label =  CVEGEO))+
  ggtitle("C3 pérdida de bosque %bosque 2000 con score")+
  geom_smooth ()+
  ggpubr::stat_regline_equation(size = 8)

 
 ggplot (top, aes (score_c1, loss_ha)) +
  geom_point () +
  geom_smooth (method = "lm")

