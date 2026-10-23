# Dashboard de cuotas (SYRCI)

Workflow: `.github/workflows/dashboard_cuotas.yml` → https://cuotas-syrci.pages.dev (Cloudflare Access, login por PIN al email).

| Archivo | Qué es |
|---|---|
| `datos_cuotas.R` | Lee de LimeSurvey (389243 Teléfono, 689133 Online) modo, fecha, materia y submateria de las completas con `PART[SI]`. Sin datos personales. Guarda `_datos/respuestas.rds`. |
| `index.qmd` | Quarto dashboard: resumen, tabla tributaria y tabla general. |
| `config/materias.csv` | Materia de LimeSurvey → grupo de cuota. |
| `config/objetivos.csv` | Objetivos por grupo (fila con `submateria` vacía) y por submateria. El «Resto» se calcula solo. |
| `estilos.css` | Estilos de las tablas. |

**Cambiar cuotas**: edita los dos CSV y haz push; la siguiente ejecución ya los usa.
Las materias que no estén en `materias.csv` salen en «Avisos».

**Materia y submateria** salen de los atributos de participante; se detectan por la descripción
del atributo («materia», «submateria»). Si no, se usan `attribute_3` y `attribute_7`
(cambiables con las variables `ATTR_MATERIA` / `ATTR_SUBMATERIA`).

**Probar en local** (desde la raíz del repo, con `.env` con LIME_USERNAME / LIME_PASSWORD / LIME_API):

```sh
Rscript dashboard/datos_cuotas.R
quarto preview dashboard/index.qmd
```

**Producción y desarrollo**: rama `main` → https://cuotas-syrci.pages.dev (cliente, cron cada hora);
rama `dev` → https://dev.cuotas-syrci.pages.dev (se publica al hacer push a `dev` o a mano).
Se trabaja en `dev` y, cuando está bien, merge a `main`.

**Filtros (OJS)**: Resumen y Participación tienen filtros en la barra lateral (modo, tipo de materia,
tipo de solicitud, canal de entrada) que recalculan en el navegador. La tabla de Cuotas no se filtra.
Los datos que van dentro del HTML no son personales: modo, día, grupo, tipo, canal y PART.
