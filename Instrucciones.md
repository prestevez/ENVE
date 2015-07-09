




## Instalación de paquetes requeridos

```
install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("xtable")
install.packages("texreg")
install.packages("lmtest")
install.packages("MASS")
```

El paquete **lme4** es esencial para este proyecto. Sin embargo, la versión más actual (1.1.8) disponible en CRAN genera resultados inconsistentes. Por tanto, se requiere instalar la versión anterior **1.1.7** para asegurar que los resultados generados sean correctos. Para instalar dicha versión, primero se requiere instalar la versión 1.1.8 para asegurarnos de que se cuenta con todas las dependencias necesarias. Segundo, se instala la versión 1.1.7 desde el archivo *source* incluido en la carpeta del proyecto (Es requerido especificar el *path* al archivo).

```
install.packages("lme4")
install.packages("C:/R/ENVE_EstevezSoto/lme4_1.1-7.tar.gz", repos=NULL, type="source")
```

**La siguientes instrucciones están pensadas para un sistema Windows, y probablemente pueden omitirse en un sistema Mac o Linux**

Si la instalación reporta **tERROR: compilation failed for package "lme4"**, será necesario instalar Rtools en la computadora.

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
