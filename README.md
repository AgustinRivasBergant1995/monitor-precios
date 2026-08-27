# Monitor de precios — Supermercados de Bariloche

Índice de precios semanal de supermercados de Bariloche (La Anónima y El Todo),
en **R + Quarto**, publicado en **GitHub Pages** como página editorial de datos.

## Estructura

```
monitor-precios/
├─ _quarto.yml
├─ custom.scss          # tema: fondo petróleo, serif, recuadros, tarjetas
├─ index.qmd            # la página
├─ R/indices.R          # ingesta + índice ponderado + cortes
└─ datos/               # laanonima_historico.xlsx · eltodo_historico.xlsx
```

## Metodología

- **Elemental**: Jevons (media geométrica de relativos) por sub-subcategoría,
  encadenado (base 100).
- **Faltantes**: se rellenan con el precio de la semana anterior o posterior.
- **Exclusiones**: sub-subcategorías de El Todo sin correspondiente en La Anónima
  (05.1.1, 05.2.1, 05.3.2, 05.4.1, 05.5.2, 07.2.1).
- **Ponderación**: el índice general y el de Alimentos agregan los elementales
  como promedio ponderado por su canasta (tablas `PESOS_GENERAL` y `PESOS_ALIM`
  en `indices.R`). Bebidas alcohólicas y las otras categorías seguidas se
  muestran con variaciones crudas.

## Correr y publicar

```r
install.packages(c("readxl","readr","dplyr","tidyr","lubridate","stringr",
                   "tibble","ggplot2","scales","plotly","knitr"))
```

```bash
quarto preview
quarto publish gh-pages
```
