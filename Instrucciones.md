# Instrucciones de scripts para el análisis de ENVE 2014 en R
*Patricio R. Estévez Soto*  
Email: [patricio.estevez.14@ucl.ac.uk](mailto:patricio.estevez.14@ucl.ac.uk)

## Versión 0.3

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
- **evaluate**: Permite correr el script entero ignorando errores.
- **coda**: Requerido por `glmmADMB`
- **R2admb**: Requerido por `glmmADMB`
- **glmmADMB**: Algoritmo utilizado para los modelos estadísticos multinivel (fuera de CRAN, requiere instalación desde código fuente).
- **classInt**: Genera intervalos para quantiles
- **dgof**: Requerido para prueba KS-Test de distribuciones discretas (Poisson y NB)


Algunos de estos paquetes pueden ya estar instalados en nuestra computadora. Aquellos que no estén instalados pueden instalarse mediante los siguientes comandos:

```
install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("xtable")
install.packages("texreg")
install.packages("lmtest")
install.packages("MASS")
install.packages("evaluate")
install.packages("coda")
install.packages("R2admb")
install.packages("classInt")
install.packages("dgof")
```

El paquete **glmmADMB** no se encuentra en los repositorios CRAN y debe de instalarse desde código fuente (para ello se requiere Rtools en un ambiente Windows; vea las instrucciones abajo). El paquete **glmmADMB** posiblemente requiera de la [**versión más nueva de R disponible en CRAN**](https://cran.r-project.org). De ser posible, asegúrese de contar con la versión más actualizada de R. De lo contrario, la instalación puede fallar.

Para instalar **glmmADMB**, pruebe con el siguiente código:

```
install.packages("glmmADMB",
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")
```

Es posible que deba instalar manualmente los paquetes **coda** y **R2admb**, pues son dependencias requeridas. Si la instalación de **glmmADMB** falla, intente instalar las dependencias primero.

### Instalación de Rtools
**La siguientes instrucciones están pensadas para un sistema Windows**.

Si la instalación reporta **tERROR: compilation failed for package "glmmADMB"**, o similar, será necesario instalar Rtools en la computadora.

Rtools permite la instalación de paquetes en R desde el código fuente (*source*). Para instalar Rtools navegamos a la página de [Rtools](http://cran.r-project.org/bin/windows/Rtools/), y descargamos el archivo ejecutable acorde a nuestra versión de R. Corremos el instalador seleccionando la opción default: "Package authoring installation".

Tras la instalación reinicie R y vuelva a correr el comando de instalación desde código fuente.

```
install.packages("glmmADMB",
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")
```

Finalmente, el script de análisis carga los paquetes, por lo que no es estrictamente necesario cargarlos de antemano.

## Ejecución del script

El proyecto está diseñado para requerir la menor intervención posible para su ejecución. Una vez puesto en marcha, el script generará los reportes y outputs relevantes en una carpeta para su posterior revisión y envío.

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

El proyecto está contenido en tres scripts: **ENVE_script_00.R**, **ENVE_script.R** y **functions.R**.

**Sólo es necesario correr ENVE_script_00.R**, pues este script contiene las instrucciones para correr los otros dos.

Para ejecutarlo, escriba el siguiente comando en la consola de R y ejecute:

```
source(file="ENVE_script_00.R", echo=TRUE)
```

La ejecución tarda cerca de dos horas, es normal que parezca que no hace nada.

Cuando termine de ejecutarse, la consola probablemente avisará que se generaron un número de *warnings*.

## Resultados

El script automáticamente realizará los análisis requeridos y guardará los resultados en una subcarpeta dentro del directorio **Output/** del folder del proyecto. El nombre de la subcarpeta se compone de un número grande (que representa la versión en *integer* de la fecha y hora en la que se ejecutó el script) y **_results**.

Dentro de esta subcarpeta se encontrarán los siguientes archivos:
- Archivo de texto **[número]_log.txt**: Contiene los comandos y resultados del script en texto.
- Archivos **.pdf** y **.png**: Contienen imágenes con gráficas de los análisis realizados.

## Errores

Los scripts han sido probados con un archivo .dbf del mismo nombre y estructura (pero datos falsos) que el archivo .dbf de la ENVE 2014. Ha sido probado en plataformas Mac, Windows y Ubuntu Linux y ha corrido sin errores.

El script está diseñado para correr hasta el final aún y cuando se generen erroes. Agradecería me enviaran el texto que arroja la consola, además del contenido del archivo **[número]_log.txt**, para identificar precisamente el tipo y consecuencia de posibles errores.

Puedo ser contactado vía email, o vía Whatsapp para cuestiones urgentes (mi número de celular lo tiene la Dirección de Acceso a Microdatos).
