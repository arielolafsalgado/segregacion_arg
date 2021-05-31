require(sf)
radios = read_sf('radios.gpkg')
radios = radios[match(unique(radios$link),radios$link),]
nac = read.csv('matriz_nacionalidades.csv',check.names =  FALSE)
oNac = names(sort(colSums(nac[2:ncol(nac)]),decreasing=TRUE))
nac_resu = read.csv('resumen_nacionalidades.csv')
colnames(nac_resu) = c('Nacionalidad','Casos')

p_radio = 1/nrow(radios)
nac_resu$casos_med = nac_resu$Casos*p_radio
nac_resu$casos_sd = sqrt(nac_resu$Casos*p_radio*(1-p_radio))

pNac = function(nacio){
  if(nacio!='all'){
    r = (nac[,nacio])[match(as.numeric(radios$link),nac$id_radio)]
  }else{
    r = (rowSums(nac[,2:ncol(nac)]))[match(as.numeric(radios$link),nac$id_radio)]
  }
  return(r)
}

for(nacio in colnames(nac)[2:ncol(nac)]){
  radios[[paste('p',nacio,sep='')]] = pNac(nacio)
}
radios[[paste('p','ALL',sep='')]] = pNac('all')
pdf('nacios.pdf')
for(nacio in oNac){
  if(min(radios[[paste('p',nacio,sep='')]])>0){
    plot(radios[paste('p',nacio,sep='')],logz=TRUE)
  }else{
    plot(radios[paste('p',nacio,sep='')],logz=FALSE)
  }
}
plot(radios[paste('pALL')],logz=TRUE)
dev.off()
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
