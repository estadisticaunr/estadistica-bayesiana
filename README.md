# Estadística Bayesiana <img src="utils/imgs/logo.png" width="150px" align="right" />

Este repositorio contiene un proyecto de Quarto. 

El mismo se utiliza para generar la página web del curso y los PDF de la práctica y los trabajos prácticos.

**TODO** presentaciones

Para renderizar el proyecto hay que ejecutar el siguiente comando:

```shell
quarto render
```

Por defecto, este genera la web y los archivos en PDF. Si se desea generar solamente la web se puede
utilizar

```shell
quarto render --to html
```

y se desea generar solo los archivos en PDF, hay que reemplazar `html` por `pdf`.

## Dependencias


* Quarto. La versión utilizada se puede encontrar en el archivo de configuración del despliegue
automático [quarto-publish.yml](.github/workflows/quarto-publish.yml)
* R. Se recomienda utilizar una versión de R mayor a 4.1
* tinytex. Es utilizado para generar los archivos PDF. Se puede instalar desde la terminal con `quarto install tinytex`.
* Librerías de R. Estas se pueden encontrar en [quarto-publish.yml](.github/workflows/quarto-publish.yml).


<!-- ## Despliegue continuo

El directorio [.github/workflows](.github/workflows) contiene siguientes archivos de configuración:

* `quarto-publish.yml`
* `quarto-render-pdf.yml` -->