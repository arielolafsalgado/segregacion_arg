require(stringr)
dataset = readLines('reporte.csv') # Leo como lineas los datos
## Notar que al archivo original le saqué el preambulo y el epilogo. 
## Sólo están los tramos que empiezan con "AREA #"

areaLine = grep('AREA',dataset) # Encuentro lineas que dicen "AREA"
areas = str_split(dataset[areaLine],' |,',simplify = TRUE) # Parto cada linea separando por espacio y ,
areas = areas[,3] # En la tercer columna quedan los códigos de radio
df = data.frame('id_radio'=areas) # Armo un dataframe con los ids

# El for recorre todas las lineas de area
for(i in 1:length(areaLine)){
  # Capture el bloque asociado
  bloque = dataset[areaLine[i]:min(areaLine[i+1],length(dataset),na.rm=TRUE)]
  # Lo guardo en un archivo temporal
  tmp = tempfile()
  # Las lineas 1:2 y -1:-5 (ó -2 si es la última) las tiro
  writeLines(bloque[3:(length(bloque)-ifelse(i==length(areaLine),3,5))],tmp)
  # Leo el archivo y lo borro
  data = read.csv(tmp)
  file.remove(tmp)
  # Veo cuales países de los anotados ya agregue antes
  paises = intersect(colnames(df),data$Pais.de.nacimiento)
  if(length(paises)>0){
    # En esa linea agrego los casos de estos países
    df[i,paises] = data$Casos[match(paises,data$Pais.de.nacimiento)]
  }
  nuevosPaises = setdiff(data$Pais.de.nacimiento,colnames(df))
  if(length(nuevosPaises)>0){
    # Para los nuevos países agregamos 0s en los previos que no los tenían y
    # los casos nuevos en esta fila.
    df[,nuevosPaises] = 0
    df[i,nuevosPaises] = data$Casos[match(nuevosPaises,data$Pais.de.nacimiento)]
  }
}
# Lo guardo
write.csv(df,'matriz_nacionalidades.csv',row.names=FALSE)
# Y guardo el resumen o agregado total.
resumen = colSums(df[,2:ncol(df)])
write.csv(sort(resumen,decreasing=TRUE),'resumen_nacionalidades.csv',row.names=names(sort(resumen,decreasing=TRUE)))
