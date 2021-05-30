require(sf)
radios = read_sf('radios.gpkg')
radios = radios[match(unique(radios$link),radios$link),]

nac = read.csv('matriz_nacionalidades.csv',check.names =  FALSE)
nac$nac_sd<-apply(nac[,-1],1,sd)

nac_resu = read.csv('resumen_nacionalidades.csv')
colnames(nac_resu) = c('Nacionalidad','Casos')

p_radio = 1/nrow(radios)
nac_resu$casos_med = nac_resu$Casos*p_radio
nac_resu$casos_sd = sqrt(nac_resu$Casos*p_radio*(1-p_radio))

radios$poblacion_extrajera = rowSums(nac[,2:ncol(nac)])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_bolivia = (nac[,'BOLIVIA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_peru = (nac[,'PERU'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_paraguay = (nac[,'PARAGUAY'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_chile = (nac[,'CHILE'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_venezuela = (nac[,'VENEZUELA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_colombia = (nac[,'COLOMBIA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_brasil = (nac[,'BRASIL'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_nigeria = (nac[,'NIGERIA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_senegal = (nac[,'SENEGAL'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_uruguay = (nac[,'URUGUAY'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_italia = (nac[,'ITALIA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_eeuu = (nac[,'ESTADOS UNIDOS'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_china = (nac[,'CHINA'])[match(as.numeric(radios$link),nac$id_radio)]
radios$poblacion_coreaD = (nac[,'COREA DEMOCRATICA'])[match(as.numeric(radios$link),nac$id_radio)]
nac_resu$Nacionalidad[1:10]
plot(radios['poblacion_extrajera'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_bolivia'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_peru'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_paraguay'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_chile'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_venezuela'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_colombia'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_brasil'],lwd=0.01,logz=TRUE)
#plot(radios['poblacion_nigeria'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_senegal'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_uruguay'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_italia'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_eeuu'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_china'],lwd=0.01,logz=TRUE)
plot(radios['poblacion_coreaD'],lwd=0.01,logz=TRUE)


radios_over_expected = sapply(1:nrow(nac),function(row){
  nac_medido = nac[row,2:ncol(nac)]
  nac_medido = nac_medido[nac_resu$Nacionalidad]
  r = sum((nac_medido-nac_resu$casos_med)/nac_resu$casos_sd>2)
  return(r)
})
radios$nac_over_expected = NA
radios$hay_nac_oe = NA
radios$nac_over_expected[match(nac$id_radio,as.numeric(radios$link))] = radios_over_expected
radios$hay_nac_oe[match(nac$id_radio,as.numeric(radios$link))] = radios_over_expected>1
plot(radios['hay_nac_oe'],lwd=0.01)
radios[is.na(radios$nac_over_expected),'link']

radios[which(is.element(radios$link,radios$link[1])),]

M = as.matrix(nac[,2:ncol(nac)])
M = t(M)%*%M
require(igraph)
g = graph_from_adjacency_matrix(M,mode='undirected',weighted=TRUE)
g_ = delete.edges(g,E(g)[E(g)$weight<160])
g_ = delete.vertices(g_,which(degree(g_)==0))
plot(g_,vertex.label=NA)
clusters(g_)
