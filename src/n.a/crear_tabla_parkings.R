library(data.table)

#Preparación del entorno
ruta <-'D:/01 Parclick/01 Solicitudes Analisis de datos/20170615 Modelizacion de Parkings/src'
setwd(ruta)
source('./R_funciones.R')

#Carga de ficheros
f_historico <- as.data.table(read.csv("../dat/historico_canjes.csv", sep =";" , dec =","))
f_promoter <- as.data.table(read.csv("../dat/NPS-Abril 2017_RED.csv", sep =";" , dec =","))

tabla <- merge(f_historico[Marca=='Parclick',list(Id_parking, Parking, Cod_Reserva, Year_canje, Mes_canje, Total_Ingreso, PRODUCTO2)],
               f_promoter[,list(Ciudad.usuario, Score)],
               by.x = 'Cod_Reserva', by.y = 'Ciudad.usuario',
               all.x = TRUE, all.y = FALSE)
tabla[,Id_parking:=as.integer(as.character(Id_parking))]
#Calculamos el número de reservas y los ingresos totales al año para posteriormente calcular la media mensual
tabla1 <- tabla[, list(N_reservas=.N, Ingresos =sum(Total_Ingreso)),
                by = .(Id_parking, Parking, Year_canje, Mes_canje)]
tabla1 <- tabla1[, list(monthly.reservas=mean(N_reservas), monthly.ingresos =mean(Ingresos)),
                by = .(Id_parking, Parking, Year_canje)]


tabla1 <- tabla1[Year_canje==2016, monthly.ingresos.2016:= monthly.ingresos]
tabla1 <- tabla1[Year_canje==2016, monthly.reservas.2016:= monthly.reservas]
tabla1 <- tabla1[Year_canje==2017, monthly.ingresos.2017:= monthly.ingresos]
tabla1 <- tabla1[Year_canje==2017, monthly.reservas.2017:= monthly.reservas]
tabla1[is.na(tabla1)]<- 0
tmp <- tabla1[ , list(monthly.ingresos.2016 = max(monthly.ingresos.2016),     
                      monthly.reservas.2016 = max(monthly.reservas.2016),     
                      monthly.ingresos.2017 = max(monthly.ingresos.2017),     
                      monthly.reservas.2017 = max(monthly.reservas.2017)),
                  by = .(Id_parking, Parking)]


tabla1 <- tabla[, list(Mean.Promoter = mean(Score, na.rm = TRUE)),
                  by = .(Id_parking, Parking)]

tmp <- merge(tmp, tabla1,
             by.x = c('Id_parking', 'Parking'), by.y = c('Id_parking', 'Parking'),
             all.x = TRUE, all.y= FALSE)

#análisis para Marcos
tabla1 <- tabla[,list(N_reservas=.N),
                by = .(Id_parking, Parking, Year_canje, PRODUCTO2)]

tabla1 <- reshape(tabla1[, list(Id_parking, Parking, Year_canje, PRODUCTO2, N_reservas)],
               v.names='N_reservas',idvar=c('Id_parking', 'Parking', 'Year_canje'),
               timevar="PRODUCTO2", 
               direction="wide")
tabla1[is.na(tabla1)]<-0
write.table(tabla1, '../dat/parking_by_canjes_productos.csv')

#Calculamos el tipo de productos que ha vendido el parking ¿asumimos que son los que tiene?
tabla1 <- tabla[,list(N_reservas=.N),
                by = .(Id_parking, Parking, Year_canje, Mes_canje,PRODUCTO2)]

#tabla1 <- tabla1[,list(reservas.medias.mes=mean(N_reservas)),
tabla1 <- tabla1[,list(reservas.medias.mes=1),
                                  by = .(Id_parking, Parking, PRODUCTO2)]

tabla1 <- reshape(tabla1[, list(Id_parking, Parking, PRODUCTO2, reservas.medias.mes)],
                  v.names='reservas.medias.mes',idvar=c('Id_parking', 'Parking'),
                  timevar="PRODUCTO2", 
                  direction="wide")
tabla1[is.na(tabla1)]<-0

tmp <- merge(tmp, tabla1,
             by.x = c('Id_parking', 'Parking'), by.y = c('Id_parking', 'Parking'),
             all.x = TRUE, all.y= TRUE)
tmp[is.na(tmp)]<-0

#calculamos la antigüedad del parking
parkings<-as.data.table(read.csv2('../dat/listado_parkings.csv',
                                  sep = ',', header = FALSE))
colnames(parkings)<-c('Id', 'Location_latitude', 'Location_longitude',
                      'Parking_name', 'Direccion', 'Provider_name', 'Creacion',
                      'Ultima_Act', 'Ciudad', 'Pais', 'Freemium', 'Status')
parkings <- parkings[, list(Id, Status, Creacion, Ultima_Act,
                f.creacion = as.Date(paste0(Creacion,'-01'),  "%Y-%m-%d"),
                f.ult.cambio= as.Date(paste0(Ultima_Act,'-01'), "%Y-%m-%d"))]

parkings[, meses_activo := difftime(Sys.Date(), f.creacion, units = "days")/30]
parkings[Status!='ACTIVE', meses_activo := difftime(f.ult.cambio, f.creacion, units = "days")/30]
parkings[, meses_activo := as.integer(meses_activo)]

tmp2<-merge(tmp, parkings,
           by.x = 'Id_parking', by.y = 'Id',
           all.x = TRUE, all.y = TRUE)
