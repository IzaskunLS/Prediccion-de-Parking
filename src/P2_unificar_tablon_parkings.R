library(data.table)

#Preparación del entorno
ruta <-'C:/Users/izask/Modelo Parking/Modelo Parkings/src'
setwd(ruta)
source('./R_funciones.R')

#Carga de ficheros
f_historico    <- as.data.table(read.csv("../dat/historico_canjes.csv", sep =";" , dec =","))
f_promoter     <- as.data.table(read.csv("../dat/NPS-Abril 2017_RED.csv", sep =";" , dec =","))
area.comercial <-as.data.table(read.csv2('../dat/parkings_zona.csv',
                                         sep = ';', header = TRUE))
parkings       <-as.data.table(read.csv2('../dat/listado_parkings.csv',
                                         sep = ';', header = TRUE))
parking.stock  <-as.data.table(read.csv2('../dat/stock_parkings.csv',
                                         sep = ',', header = TRUE))
parkings<-parkings[,c(1,12:13, 42, 15:17, 20:22, 24:26, 28:31, 62, 69, 41)]
parkings <- merge(parkings, parking.stock[, list(parking_id, Stock=sum.stock.)],
                  by.x = 'Id', by.y = 'parking_id',
                  all.x = TRUE, all.y = FALSE)

tabla <- merge(f_historico[Marca=='Parclick',list(Id_parking, Parking, Cod_Reserva, Year_canje, Mes_canje, Total_Ingreso, PRODUCTO2, aeropuerto = SUBTIPO)],
               f_promoter[,list(Ciudad.usuario, Score)],
               by.x = 'Cod_Reserva', by.y = 'Ciudad.usuario',
               all.x = TRUE, all.y = FALSE)
tabla[,Id_parking:=as.integer(as.character(Id_parking))]
#Calculamos el número de reservas y los ingresos totales al año para posteriormente calcular la media mensual
tabla1 <- tabla[, list(N_reservas=.N, Ingresos =sum(Total_Ingreso)),
                by = .(Id_parking, Parking, Year_canje, Mes_canje)]
tmp2    <- tabla1[, list(monthly.reservas.mean=mean(N_reservas, na.rm = TRUE), 
                         monthly.reservas.media=median(N_reservas, na.rm = TRUE),
                         monthly.ingresos.mean =mean(Ingresos, na.rm = TRUE), 
                         monthly.ingresos.media=median(Ingresos, na.rm = TRUE),
                         num.meses =.N),
                 by = .(Id_parking, Parking)]
tmp    <- tabla1[, list(monthly.reservas=mean(N_reservas, na.rm = TRUE), 
                        monthly.ingresos =mean(Ingresos, na.rm = TRUE),
                        num.meses =.N),
                by = .(Id_parking, Parking)]

#Calculamos el valor medio que le dan los usuarios al parking

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
                by = .(Id_parking, Parking, Year_canje, Mes_canje, PRODUCTO2)]

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

#calculamos la antigüedad del parking
parkings[ ,Date_created := as.Date(Date_created,  "%m/%d/%Y")]
parkings[ ,Last_updated := as.Date(Last_updated,  "%m/%d/%Y")]

parkings[, meses_activo := difftime(Sys.Date(), Date_created, units = "days")/30]
parkings[Status!='ACTIVE', meses_activo := difftime(Last_updated, Date_created, units = "days")/30]
parkings[, meses_activo := as.integer(meses_activo)]

parkings2<-parkings[, lapply(.SD, function(x) as.character(x)), .SDcols = c(7:17, 19)]
parkings2<-parkings2[, lapply(.SD, function(x) replace(x, which(x=='false'),0)), .SDcols = 1:ncol(parkings2)]
parkings2<-parkings2[, lapply(.SD, function(x) replace(x, which(x=='true') ,1)), .SDcols = 1:ncol(parkings2)]
parkings<-cbind(parkings[,list(Id, meses_activo, Max_height, Stock, Spots), parkings2])

tmp<-merge(tmp, parkings,
           by.x = 'Id_parking', by.y = 'Id',
           all.x = TRUE, all.y = TRUE)

tmp <-merge(tmp, area.comercial,
            by.x = 'Id_parking', by.y = 'Id',
            all.x = TRUE, all.y = TRUE)
tmp<-tmp[!is.na(Parking)]

#Incluimos los tipos de aeropuerto
tabla1 <- tabla[,list(tipo.aeropuerto= 1), by=.(Id_parking, aeropuerto)]
tabla1 <- reshape(tabla1,
                  v.names='tipo.aeropuerto',idvar=c('Id_parking'),
                  timevar="aeropuerto", 
                  direction="wide")
tabla1[is.na(tabla1)]<-0

tmp <-merge(tmp, tabla1,
            by.x = 'Id_parking', by.y = 'Id_parking',
            all.x = TRUE, all.y = TRUE)

tmp <- tmp[tipo.aeropuerto.NA == 0, n_sitios.aeropuerto:=1]
tmp <- tmp[,-c('tipo.aeropuerto.NA','Parking_name', 'n_sitios.OTROS')]

write.table(tmp,'../dat/tabla_parkings.csv',
            sep =';', dec = ',', row.names = FALSE)

