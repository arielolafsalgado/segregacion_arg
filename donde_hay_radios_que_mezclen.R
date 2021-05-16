require(sf)
radios = read_sf('radios.gpkg')
nac = read.csv('matriz_nacionalidades.csv',check.names =  FALSE)
nac_resu = read.csv('resumen_nacionalidades.csv')
colnames(nac_resu) = c('Nacionalidad','Casos')

p_radio = 1/nrow(radios)
nac_resu$casos_med = nac_resu$Casos*p_radio
nac_resu$casos_sd = sqrt(nac_resu$Casos*p_radio*(1-p_radio))

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
