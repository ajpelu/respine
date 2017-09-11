<!-- $theme: default -->
<!-- page_number: true -->

# Crear Landscape
* raster vacío (`value = 0`) *100 x 200* celdas (n=20000)
* `createLandscape()` 	 
--- 
# `createLandscape()` (I) 
* **Pinares de repoblación**
	* Añade patch de pinar en posición fija
	* tamaño variable (**<font color="blue">`$size_pp$`</font>**) (300 - 6000)
	* densidad de población (**<font color="blue">`$size_pp$`</font>**)
	* `landUse = 1`
* **Bosques naturales**
	* Añade *n* patches (**<font color="blue">`$n_nf$`</font>**) (1 - 5)
	* Posición aleatoria 
	* tamaño variable (**<font color="blue">`$size_nf$`</font>**) (10 - 500)
	* `landUse = 2`
--- 

# `createLandscape()` (II)
* **Cultivos**
	* Añade entre 3 - 8 patches (*random*) 
	* tamaño 10 - max [7.5 % celdas disponibles]
	* Posición aleatoria
	* `landUse = 3`
* **Otros**
	* Resto celdas
	* `landUse = 0` 
--- 
# Crear Landscape (output)
* Mapa con diferentes patches de pinares de repoblación, cultivos y bosques naturales de acuerdo a lo que el usuario ha elegido. 
* Color de los pinares de repoblación varía en función de la densidad de la repoblación (**<font color="blue">`$den_pp$`</font>**) (baja, media, alta). See issue [#5](https://github.com/ajpelu/respine/issues/5) 
--- 

# Valores iniciales de Riqueza
* `initRichness()` 
* raster con paisaje: output `createLandscape()`
* dataframe con riqueza por uso del Suelo 
* raster de distancia
* densidad del pinar de repoblación
* Uso del pasado del pinar de repoblación

--- 

# `initRichness()`  (I) 
* **Bosques naturales** (`landUse = 2`)
	* Cada pixel un valor aleatorio del rango de riqueza potencial (14.67 - 17.88) 
		* low: 14.67 [ mean(13.72, 15.62) ] 
		* upp: 17.88 [ mean(16.11, 19.66) ]
	* Datos obtenidos de SINFONEVADA (todo add) y de Gómez-Aparicio et al. (2004) 


# issues 
[#5](https://github.com/ajpelu/respine/issues/5) Pine density 

