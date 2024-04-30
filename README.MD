# README

## Cuotas Auto

Este proyecto es una herramienta interna de nuestro equipo diseñada para gestionar y actualizar automáticamente las encuestas y hojas de cálculo de Google Sheets. Está desarrollado en R y se ejecuta a través de GitHub Actions.

### Descripción

El proyecto incluye una serie de scripts en R y flujos de trabajo de GitHub Actions que interactúan con la API de LimeSurvey y Google Sheets para automatizar procesos.

#### Scripts R principales

-   `update_sheets.R`

#### Flujos de trabajo de GitHub Actions principales

-   `pruebas_6h.yml`
-   `campo_30'_lun-mie.yml`

### Funcionamiento

#### Interacción con APIs

El script `update_sheets.R` gestiona la autenticación con la API de LimeSurvey, recupera los datos de los participantes y las respuestas de las encuestas y los transfiere a Google Sheets.

#### Automatización de actualizaciones

Los flujos de trabajo de GitHub Actions programan y ejecutan el script `update_sheets.R` para actualizar las hojas de cálculo de Google Sheets con los datos más recientes obtenidos de las encuestas.

### Licencia

Este proyecto está licenciado bajo la Licencia MIT. Para más detalles, consulta el archivo `LICENSE`.

### Nota

Este proyecto es de uso exclusivo interno del equipo y no está abierto a contribuciones externas. Actualmente, no hay tareas pendientes.
