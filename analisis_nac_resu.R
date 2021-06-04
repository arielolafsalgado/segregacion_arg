## Analisis de las nacionalidad para determinar categorias o grupos con los que trabajar
require(sf)
radios = read_sf('radios.gpkg')
radios = radios[match(unique(radios$link),radios$link),]
nac = read.csv('matriz_nacionalidades.csv',check.names =  FALSE)
nac_resu = read.csv('resumen_nacionalidades.csv',check.names =  FALSE)
colnames(nac_resu) = c('Nacionalidad','Casos')

p_radio = 1/nrow(radios)
nac_resu$casos_med = nac_resu$Casos*p_radio
nac_resu$casos_sd = sqrt(nac_resu$Casos*p_radio*(1-p_radio))

require(ggplot2)
# Defino corte para los tamaños de las nacionalidades que miro
corte=0.01*sum(nac_resu$Casos)
p <- ggplot()+
  geom_point(data=nac_resu[nac_resu$Casos>=corte,],aes(x=as.factor(Nacionalidad), y=Casos), color="red")+
  geom_point(aes(x=as.factor("Otros"), y=sum(nac_resu[nac_resu$Casos<corte,]$Casos)), color="blue")+
  theme(axis.text.x = element_text(angle = 90))
plot(p)

#Analizo los "otros" por continente o region
# PROBLEMA CON EL IDIOMA, HAY MUCHOS QUE NO MATCHEAN..HACER UN DIC O A MANO? 
require(countrycode)
countrycode(countries, "spanish", "english", custom_dict = custom_dict)

nac_resu$continente <- countrycode(sourcevar = nac_resu[, "Nacionalidad"],
                                   origin = "country.name",
                                   destination = "continent")
                                   #destination = "region")

p <- ggplot()+
  geom_point(data=nac_resu[nac_resu$Casos>=corte,],aes(x=as.factor(Nacionalidad), y=Casos), color="red")+
  stat_summary(data=nac_resu[nac_resu$Casos<corte,],
               aes(x=continente,y=Casos,group=continente),
               fun = sum,
               na.rm = TRUE,
               color = 'blue',
               geom ='point')+
theme(axis.text.x = element_text(angle = 90))
plot(p)


