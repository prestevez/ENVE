# Instrucciones de scripts para el análisis de ENVE 2014 en R
*Patricio R. Estévez Soto*  
Email: [patricio.estevez.14@ucl.ac.uk](mailto:patricio.estevez.14@ucl.ac.uk)  

## Introducción

Estas instrucciones detallan brevemente el procedimiento necesario para correr exitosamente los scripts diseñados para el análisis de la ENVE 2014 en R.

De antemano muchas gracias por su apoyo en la realización de este proyecto.

Cualquier duda, comentario, o error que se genere al correr este proyecto, agradezco me contacte a la brevedad posible.

La ejecución consta principalmente de dos partes, la **Instalación de paquetes requeridos**, y la **Ejecución del script**.

La instalación de paquetes solo se requiere realizar una vez. La ejecución del script puede realizarse las veces que sean necesarias si se encuentran errores.

## Instalación de paquetes requeridos

Los paquetes requeridos por este proyecto son los siguientes:
- **foreign**: Importa archivos .dbf a R
- **ggplot2**: Genera gráficos más atractivos que R básico
- **Cairo**: Permite guardar los gráficos como archivos png
- **xtable**: Genera tablas sencillas en formato LaTeX
- **texreg**: Genera tablas complejas en formato LaTeX
- **lmtest**: Realiza pruebas de hipótesis al comparar modelos
- **MASS**: Permite generar modelos estadísticos utilizando la distribución binomial negativa.
- **lme4 Version 1.1.7**: Permite generar modelos estadísticos de efectos mixtos.

Algunos de estos paquetes pueden ya estar instalados en nuestra computadora. Aquellos que no estén instalados pueden instalarse mediante los siguientes comandos:

```
install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("xtable")
install.packages("texreg")
install.packages("lmtest")
install.packages("MASS")
```

El paquete **lme4** es esencial para este proyecto. Sin embargo, la versión más actual (1.1.8) disponible en CRAN genera resultados inconsistentes. Por tanto, se requiere instalar la versión anterior **1.1.7** para asegurar que los resultados generados sean correctos. Para instalar dicha versión, primero se requiere instalar la versión 1.1.8 para asegurarnos de que se cuenta con todas las dependencias necesarias. Segundo, se instala la versión 1.1.7 desde el archivo *source* incluido en la carpeta del proyecto (es requerido especificar el *path* al archivo).

```
install.packages("lme4")

# Se debe de especificar la dirección al archivo correcta
install.packages("C:/R/ENVE_EstevezSoto/lme4_1.1-7.tar.gz", repos=NULL, type="source") 
```

**La siguientes instrucciones están pensadas para un sistema Windows**.

Si la instalación reporta **tERROR: compilation failed for package "lme4"**, o similar, será necesario instalar Rtools en la computadora.

Rtools permite la instalación de paquetes en R desde el código fuente (*source*). Para instalar Rtools navegamos a la página de [Rtools](http://cran.r-project.org/bin/windows/Rtools/), y descargamos el archivo ejecutable acorde a nuestra versión de R. Corremos el instalador seleccionando la opción default: "Package authoring installation".

Tras la instalación reinicie R y vuelva a correr el comando de instalación desde código fuente.

```
install.packages("C:/R/ENVE_EstevezSoto/lme4_1.1-7.tar.gz", repos=NULL, type="source")
```

Para asegurarse de que se ha instalado la versión correcta se puede correr el siguiente comando:

```
library(lme4) # Para cargar el paquete
packageVersion("lme4")
```

El último comando debe de regresar la versión **1.1.7**.

Finalmente, el script de análisis carga los paquetes, por lo que no es estrictamente necesario cargarlos de antemano.

## Ejecución del script

El proyecto está diseñado para requerir la menos intervención posible para su ejecución. Una vez puesto en marcha, el script generará los reportes y outputs relevantes en una carpeta para su posterior revisión y envío.

Además de la instalación de los paquetes necesarios, descrita en la sección anterior, la única intervención necesaria es colocar el archivo .dbf que contiene los datos de la ENVE en el folder del proyecto y asignar el *working directory* en R al folder del proyecto.

### Archivo .dbf ENVE 2014

Copie el archivo **"TR_ENVE_CUES_2014.dbf"** al folder de este proyecto.

### Establecer working directory en R

Establezca el *working directory* en R para que apunte al folder del proyecto.

Por ejemplo:
```
getwd() # Devuelve cuál es el working directory actual.

setwd("C:/R/ENVE_EstevezSoto") # Establece el working directory en el folder del proyecto
```

Sólo se requiere que el *path* al folder del proyecto sea especificado.

### Ejecutar el script

El proyecto está contenido en dos scripts: **ENVE_script_00.R** y **ENVE_script.R**.

**Sólo es necesario correr ENVE_script_00.R**, pues este script contiene las instrucciones para correr el segundo script.

Para ejecutarlo, escriba el siguiente comando en la consola de R y ejecute:

```
source(file="ENVE_script_00.R", echo=TRUE)
```
La ejecución tarda cerca de media hora, es normal que parezca que no hace nada.

Cuando termine de ejecutarse, la consola probablemente avisará que se generaron un número (cerca de 30) *warnings*. Pueden ignorarse.

## Resultados 

El script automáticamente realizará los análisis requeridos y guardará los resultados en una subcarpeta dentro del directorio **Output/** del folder del proyecto. El nombre de la subcarpeta se compone de un número grande (que representa la versión en *integer* de la fecha y hora en la que se ejecutó el script) y **_results**. 

Dentro de esta subcarpeta se encontrarán los siguientes archivos:
- Archivo de texto **[número]_log.txt**: Contiene los comandos y resultados del script en texto.
- Archivos **.pdf** y **.png**: Contienen imágenes con gráficas de los análisis realizados.
- Archivos **.Rdata**: contienen los objetos de análisis (modelos), reporte (tablas y xtables) y gráficas (ggplots) en formato R.

## Errores

Los scripts han sido probados con un archivo .dbf del mismo nombre y estructura (pero datos falsos) que el archivo .dbf de la ENVE 2014. Ha sido probado en plataformas Mac y Windows y ha corrido sin errores.

Si se produce algún error, agradecería me enviaran el texto que arroja la consola, así como el contenido (por lo menos las últimas líneas) del archivo **[número]_log.txt**, pues ello me ayudará a identificar precisamente dónde ocurrió el error.

Puedo ser contactado vía email, o vía Whatsapp para cuestiones urgentes (mi número de celular lo tiene la Dirección de Acceso a Microdatos).
