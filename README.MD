# README

## Cuotas Auto

Este proyecto es una herramienta interna de nuestro equipo que se utiliza para gestionar y actualizar automáticamente las encuestas y hojas de cálculo de Google Sheets. El proyecto está escrito en R y se ejecuta en GitHub Actions.

### Descripción

El proyecto consta de varios scripts R y flujos de trabajo de GitHub Actions. Los scripts R se utilizan para interactuar con la API de LimeSurvey y Google Sheets. Los flujos de trabajo de GitHub Actions se utilizan para programar y ejecutar estos scripts automáticamente.

Los scripts R principales son:

update_sheets.R
localts-uploadQS.R
Los flujos de trabajo de GitHub Actions principales son:

pruebas_6h.yml
campo_30'\_lun-mie.yml

### Cómo funciona

El script localts-uploadQS.R se utiliza para interactuar con la API de LimeSurvey y Google Sheets. Este script se encarga de autenticarse con la API de LimeSurvey, obtener los participantes y respuestas de las encuestas, y escribir estos datos en Google Sheets.

Los flujos de trabajo de GitHub Actions se utilizan para programar y ejecutar el script update_sheets.R automáticamente. Este script se encarga de actualizar las hojas de cálculo de Google Sheets con los datos más recientes de las encuestas.

Licencia
Este proyecto está licenciado bajo la licencia MIT. Para más detalles, por favor vea el archivo LICENSE.

Nota
Este proyecto es para uso interno del equipo y no está abierto a contribuciones externas. No hay tareas pendientes en este momento.
