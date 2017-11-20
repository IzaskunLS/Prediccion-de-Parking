library(data.table)
ruta <-'D:/01 Parclick/01 Solicitudes Analisis de datos/20170615 Modelizacion de Parkings/src'
set(ruta)
f_historico <- as.data.table(read.csv("../dat/historico_canjes.csv", sep =";" , dec =","))
f_promoter <- as.data.table(read.csv("../dat/NPS-Abril 2017_RED.csv", sep =";" , dec =","))

tabla <- merge(f_historico[Marca=='Parclick',list(Id_parking, Parking, Cod_Reserva, Year_canje, Mes_canje, Total_Ingreso, PRODUCTO2)],
               f_promoter[,list(Ciudad.usuario, Score)],
               by.x = 'Cod_Reserva', by.y = 'Ciudad.usuario',
               all.x = TRUE, all.y = FALSE)

tabla1 <- tabla[, N_reservas:=.N,
               by = .(Id_parking, Parking, Year_canje)]
tabla1 <- tabla1[, T_Ingresos :=sum(Total_Ingreso),
               by = .(Id_parking, Parking, Year_canje)]
tabla1 <- tabla1[, Mean.Promoter := mean(Score, na.rm = TRUE),
               by = .(Id_parking, Parking)]

tabla1 <- tabla1[Year_canje==2016, year.2016:= T_Ingresos]
tabla1 <- tabla1[Year_canje==2017, year.2017:= T_Ingresos]
tabla1[is.na(year.2016), year.2016:=0]
tabla1[is.na(year.2017), year.2017:=0]

tabla1 <- tabla1[, list(year.2017=max(year.2017),
                      year.2016=max(year.2016), 
                      Mean.Promoter = max(Mean.Promoter)),
               by= .(Id_parking, Parking)]



tabla2 <- tabla[,list(N_reservas=.N, T_Ingresos =sum(Total_Ingreso), Mean_Promoter = mean(Score)),
                by = .(Id_parking, Parking, PRODUCTO2)]

tabla2 <- reshape(tabla2[, list(Id_parking, PRODUCTO2)],
               v.names='PRODUCTO2',idvar='Id_parking',
               timevar="PRODUCTO2",direction="wide")
tmp <- tabla2[,2:7]
tmp[!is.na(tmp),]<-1
tmp[is.na(tmp)] <-0
