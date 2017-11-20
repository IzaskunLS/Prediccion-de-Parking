#######################################################################################################
# Funcion: clasificar_negocios                                                                        #
#         Descripción: Función que a partir de una lista que obtenemos del JSON clasificamos los      #
#                      negocios en el objeto type                                                     #
#         Entradas:                                                                                   #
#                 l.JSON: lista obtenida del JSON para cada uno de los negocios                       #
#                 salida: data.table con la clasificación.                                            #
#######################################################################################################
clasificar.negocios <- function(l.JSON, datos = TRUE){
  #Tablas parametrizacion
  clasificacion.areas <- as.data.table(read.csv2('../dat/clasificacion_areas.csv'), 
                                       sep=';', header=TRUE)
  if (datos){
      num.el <- sapply(l.JSON, length)
      t_salida <- as.data.table(cbind(unlist(l.JSON), rep(1:length(l.JSON), num.el)))
      colnames(t_salida)<-c("types", "fila")
      t_salida <- merge(t_salida, clasificacion.areas[,list(types, zona1)],
                        all.x = TRUE, all.y = TRUE)
      t_salida <- t_salida[is.na(zona1), zona1:='OTROS']
      t_salida <- t_salida[, list(n_sitios = .N), by = .(fila, zona1)]
      t_salida <- t_salida[is.na(fila), n_sitios := 0]  
  }else{
    t_salida <- clasificacion.areas[,list(types, zona1, n_sitios=0, fila=0)]
    t_salida <- t_salida[, list(n_sitios = .N), by = .(fila, zona1)]
    
  }      
  setorder(t_salida, zona1)
  t_salida <- reshape(t_salida[,list(fila, zona1, n_sitios)],
                      v.names='n_sitios',idvar='fila',timevar="zona1",direction="wide")
  t_salida <-t_salida[!is.na(fila),]
  
  if (datos){    
      #t_salida2[!is.na(t_salida2),]<-1
      t_salida[is.na(t_salida),]<- 0
  #    t_salida <- t_salida[,1:(ncol(t_salida)-1)]
  }else{
      t_salida[,]<- 0
    }
      #t_salida2<- t_salida2[,lapply(.SD, as.integer), .SDcols=1:ncol(t_salida2)]
      #t_salida <- cbind(t_salida[, list(fila=as.integer(fila))], t_salida2)
      t_salida <- t_salida[!is.na(fila), fila1:=as.integer(fila)]
      setorder(t_salida, fila1)
      return(t_salida[,1:(ncol(t_salida)-1)])

      
}

#######################################################################################################
# Funcion: tratar_json                                                                                #
#         Descripción: Función que carga y trata el resultado de recibir el fichero JSON              #
#         Entradas:                                                                                   #
#                 arch: URL con la solicitud de información                                           #
#                 salida: data.table con las características de la zona solicitada                    #
#######################################################################################################

tratar_json <- function(lat, long, dist, tipo){
  if (is.na(tipo)){
            arch <- paste0('https://maps.googleapis.com/maps/api/place/nearbysearch/json?',
                           'location=', lat,',',long,
                           '&radius=', dist,
                           '&key=AIzaSyDuHuha-w0mO7HmE5cqJHje2ARLJYama90')
  }else{
    arch <- paste0('https://maps.googleapis.com/maps/api/place/nearbysearch/json?',
                   'location=', lat,',',long,
                   '&radius=', dist,
                   '&type=', tipo, '&keyword=cruise',
                   '&key=AIzaSyDuHuha-w0mO7HmE5cqJHje2ARLJYama90')
      } 
  #descargamos el JSON y recogemos los campos necesarios en una tabla
  file.JSON <-fromJSON(arch)
  t_JSON <- lapply(file.JSON, function(x) { as.data.frame(x) })
  status <- t_JSON$status$x
  if (status=='OK'){
    t_JSON <- t_JSON$results
    t_JSON <- as.data.table(t_JSON[,c('name','scope','types')])
    n <- ncol(t_JSON)+1  
    # Clasificamos el resultado por tipo de comercio
    dt.clasificacion <- clasificar.negocios(t_JSON$types, TRUE)
     #t_salida <- t_JSON[, lapply(.SD, sum), .SDcols=n:(ncol(t_JSON))]
  }
  else{
    t_JSON <- data.table(name      = 'N.D',
                         scope     = 'N.D',
                         types     = 'N.D'
                         )
    dt.clasificacion <- clasificar.negocios(t_JSON$types, FALSE)
  }
  t_salida<- cbind(t_JSON[], dt.clasificacion[,2:ncol(dt.clasificacion)])
  print(paste0("Llamada API: ", status))
  return(t_salida)
}
