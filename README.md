---
title: "Guiame"
output: html_document
---


## IDEA 
Este archivito es para tener una constancia de como usar lo que hay en el repo. Por ahora, tenemos las nacionalidades de CABA en el reporte original (xls) y una matriz indicando qué nacionalidades hay para cada radio censal. 

Tenemos un archivo reporte.csv que tiene el archivo original sin el preambulo y el epilogo (info de REDATAM y un cuadro agregado para todo CABA).

El script rearma_dataset.R toma ese archivo y arma una matriz nueva con radios censales en las filas y nacionalidades en las columnas. Y un archivo resumen.csv que indica los valores agregados sobre todos los radios (checkeado que coincida con el original)


## Faltaría

Archivo con los poligonos de radios censales de CABA (lo tengo que buscar). Y con ese podrían venir otras variables.