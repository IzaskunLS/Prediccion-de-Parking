library(jsonlite)
library(data.table)  

#Inicialización y parametrización
#ruta <-'E:/u_tad/Proyecto Fin de Grado'
ruta <-'D:/01 Parclick/01 Solicitudes Analisis de datos/20170615 Modelizacion de Parkings/src'
distancia <- '1000'


setwd(ruta)
source('./R_funciones.R')

parkings<-as.data.table(read.csv2('../dat/listado_parkings.csv',
                        sep = ',', header = FALSE))

colnames(parkings)<-c('Id', 'Location_latitude', 'Location_longitude',
                     'Parking_name', 'Direccion', 'Provider_name', 'Creacion',
                     'Ultima_ Act', 'Ciudad', 'Pais', 'Freemium', 'Status')
parkings <- parkings[Status %in% c('ACTIVE', 'INACTIVE') & Freemium == 'false',]


for (i in 1:nrow(parkings)){
      #parkings[i,]
      latitud  <- as.character(parkings[i,Location_latitude])
      longitud <- as.character(parkings[i,Location_longitude])
    
      #Buscamos el tipo de zona
      json.zona <- tratar_json(latitud, longitud, distancia, NA)
      json.zona <- json.zona[, lapply(.SD, sum), .SDcols=4:(ncol(json.zona))]
      
      #Buscamos el nº de parkings cercanos y los parking de caravanas
      dist.parking <-500
      json.parking <- tratar_json(latitud, longitud, dist.parking, 'parking')
      json.parking[, dist.parking := dist.parking]
      json.prv <- tratar_json(latitud, longitud, dist.parking, 'prv')
      json.prv[, dist.parking := dist.parking]
      json.dist.parking <- rbind(json.parking, json.prv)
      #json.dist.parking <- json.parking
      dist.parking <-1000
      json.parking <- tratar_json(latitud, longitud, dist.parking, 'parking')
      json.parking[, dist.parking := dist.parking]
      json.prv <- tratar_json(latitud, longitud, dist.parking, 'prv')
      json.prv[, dist.parking := dist.parking]
      json.dist.parking <- rbind(json.dist.parking,json.parking, json.prv)
      #json.dist.parking <- rbind(json.dist.parking,json.parking)
      json.dist.parking[, min.dist := min(dist.parking), by = name]
      #json.dist.parking[name != 'N.D', zona1.parking := 1]
      json.dist.parking <- json.dist.parking[min.dist == dist.parking]
      json.dist.parking[min.dist == 500, parking.500m := zona1.parking]
      json.dist.parking[min.dist == 1000, parking.1000m := zona1.parking]
      json.dist.parking[is.na(json.dist.parking)]<-0
      json.dist.parking<-json.dist.parking[, lapply(.SD, sum), .SDcols=32:33]
      
      temp <- cbind(parkings[i,  c('Id', 'Parking_name')], json.dist.parking, json.zona)
      if (i==1){
        parkings.zona <- temp
      }else{
        parkings.zona <- rbind(parkings.zona,temp)
      }
      print(temp[,  c('Id', 'Parking_name', 'parking.500m', 'parking.1000m')])
  }  
write.table(parkings.zona,'../dat/parkings_zona.csv',
            sep =';', dec = ',', row.names = FALSE)

