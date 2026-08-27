# ---------------------------------------------------------------------------
# indices.R — ingesta y cálculo del monitor de precios (Bariloche)
# Índice Jevons elemental por sub-subcategoría, encadenado (base 100), y
# agregación superior como promedio ponderado de los índices elementales.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl); library(readr); library(dplyr)
  library(tidyr);  library(lubridate); library(stringr); library(tibble)
})

COLS_ID <- c("nombre_articulo", "categorias", "subcategoria", "sub_subcategoria")

# Sub-subcategorías de El Todo sin correspondiente en La Anónima: se excluyen
EXCLUIR_CODIGOS <- c("05.1.1","05.2.1","05.3.2","05.4.1","05.5.2","07.2.1")

# Renombres para las vistas de sub-subcategoría (fuera de 01/02)
REN_SUBSUB <- c(
  "12.1.3" = "Bienes para el cuidado personal",
  "05.6.1" = "Bienes para el hogar",
  "09.3.4" = "Alimento para mascotas",
  "06.1.2" = "Productos de farmacia")

# --- Ponderadores -------------------------------------------------------------
# Índice general (sobre canasta total de supermercados). "02" = rubro completo.
PESOS_GENERAL <- tribble(
  ~grupo,   ~peso,
  "01.1.1", 13.55, "01.1.2", 36.24, "01.1.4", 9.02, "01.1.5", 1.84, "01.1.6", 3.06,
  "01.1.7", 7.07,  "01.1.8", 3.30,  "01.1.9", 2.03, "01.2.1", 2.49, "01.2.2", 3.61,
  "02",     3.79,  "05.6.1", 4.71,  "06.1.2", 0.18, "09.3.4", 2.76, "12.1.3", 6.34)
# Índice de Alimentos y bebidas no alcohólicas (sobre su propia canasta)
PESOS_ALIM <- tribble(
  ~grupo,   ~peso,
  "01.1.1", 16.4874552, "01.1.2", 44.08602151, "01.1.4", 10.97670251,
  "01.1.5", 2.240143369, "01.1.6", 3.718637993, "01.1.7", 8.602150538,
  "01.1.8", 4.009856631, "01.1.9", 2.464157706, "01.2.1", 3.024193548,
  "01.2.2", 4.390681004)

grupo_general <- function(code) case_when(
  str_starts(code, "02.")                             ~ "02",
  code %in% c("05.6.1","06.1.2","09.3.4","12.1.3")     ~ code,
  code %in% PESOS_ALIM$grupo | code == "01.1.3"        ~ code,   # 01.* (pescados no pesa)
  TRUE                                                 ~ NA_character_)
grupo_alim <- function(code) if_else(code %in% PESOS_ALIM$grupo, code, NA_character_)

# --- Utilidades ---------------------------------------------------------------
parse_fecha_header <- function(x) {
  x <- as.character(x); n <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(n) & n > 40000, as.character(as.Date(n, origin = "1899-12-30")), x) |> as.Date()
}

cargar_cadena <- function(path, comercio) {
  wide <- read_excel(path)
  cols_fecha <- setdiff(names(wide), COLS_ID)
  wide |>
    pivot_longer(all_of(cols_fecha), names_to = "fecha_raw", values_to = "precio") |>
    transmute(
      fecha = parse_fecha_header(fecha_raw), supermercado = comercio,
      descripcion = nombre_articulo, rubro = categorias, subsub = sub_subcategoria,
      sku_id = paste(comercio, nombre_articulo, sep = " | "),
      precio = suppressWarnings(as.numeric(precio)))
}

cargar_bases <- function(
    path_laanonima = "datos/laanonima_historico.xlsx",
    path_eltodo    = "datos/eltodo_historico.xlsx") {
  crudo <- bind_rows(cargar_cadena(path_laanonima, "La Anonima"),
                     cargar_cadena(path_eltodo,    "El Todo")) |>
    mutate(code = str_extract(subsub, "^\\d+\\.\\d+\\.\\d+"))

  # El Todo: descartar los artículos que tengan algún precio 0 (artículo completo)
  sku_cero_eltodo <- crudo |>
    filter(supermercado == "El Todo", !is.na(precio), precio == 0) |>
    distinct(sku_id) |> pull(sku_id)

  crudo |>
    filter(!code %in% EXCLUIR_CODIGOS, !sku_id %in% sku_cero_eltodo) |>  # exclusiones
    group_by(sku_id) |> arrange(fecha, .by_group = TRUE) |>
    fill(precio, .direction = "downup") |> ungroup() |>   # relleno de faltantes
    filter(!is.na(precio), precio > 0) |>
    mutate(
      subsub_lbl = case_when(
        code %in% names(REN_SUBSUB)                       ~ unname(REN_SUBSUB[code]),
        str_starts(code, "01.") | str_starts(code, "02.") ~ str_remove(subsub, "^\\d+\\.\\d+\\.\\d+\\s+"),
        TRUE                                              ~ NA_character_),
      subsub_grupo = case_when(
        str_starts(code, "01.")     ~ "alimentos",
        str_starts(code, "02.")     ~ "alcoholicas",
        code %in% names(REN_SUBSUB) ~ "otras",
        TRUE                        ~ NA_character_)) |>
    group_by(sku_id, fecha, supermercado, rubro, subsub, code,
             subsub_lbl, subsub_grupo, descripcion) |>
    summarise(precio = mean(precio), .groups = "drop") |> arrange(sku_id, fecha)
}
cargar_precios <- function(...) cargar_bases(...)

relativos <- function(df) df |> group_by(sku_id) |> arrange(fecha, .by_group = TRUE) |>
  mutate(rel = precio / lag(precio)) |> ungroup()

# Índice Jevons encadenado (base 100), opcionalmente por grupos
indice_encadenado <- function(df, grupo_vars = character(0)) {
  rel <- relativos(df) |> filter(is.finite(rel), rel > 0)
  f0  <- min(df$fecha)
  fac <- rel |> group_by(across(all_of(grupo_vars)), fecha) |>
    summarise(factor = exp(mean(log(rel))), n_sku = n(), .groups = "drop")
  base <- fac |> distinct(across(all_of(grupo_vars))) |>
    mutate(fecha = f0, factor = 1, n_sku = NA_integer_)
  bind_rows(base, fac) |> group_by(across(all_of(grupo_vars))) |>
    arrange(fecha, .by_group = TRUE) |>
    mutate(indice = 100 * cumprod(factor), var_pct = (factor - 1) * 100) |> ungroup()
}

sub_rubro <- function(df, pref) filter(df, str_starts(rubro, pref))

# Índice ponderado: elementales por grupo -> promedio ponderado de los índices
indice_ponderado <- function(df, pesos, grupo_fun) {
  d <- df |> mutate(grupo = grupo_fun(code)) |> filter(!is.na(grupo))
  indice_encadenado(d, "grupo") |>
    inner_join(pesos, by = "grupo") |>
    group_by(fecha) |>
    summarise(indice = weighted.mean(indice, peso), .groups = "drop") |>
    arrange(fecha) |> mutate(var_pct = (indice / lag(indice) - 1) * 100)
}

# Índice de alimentos ponderado, para ambas cadenas y cada una
indice_alim_cadenas <- function(df) {
  f <- function(d) indice_ponderado(sub_rubro(d, "01"), PESOS_ALIM, grupo_alim)
  bind_rows(
    f(df) |> mutate(serie = "Ambos"),
    f(filter(df, supermercado == "La Anonima")) |> mutate(serie = "La Anonima"),
    f(filter(df, supermercado == "El Todo"))    |> mutate(serie = "El Todo"))
}

var_mensual <- function(idx_total) idx_total |>
  mutate(mes = floor_date(fecha, "month")) |> group_by(mes) |>
  slice_max(fecha, n = 1, with_ties = FALSE) |> ungroup() |> arrange(mes) |>
  mutate(var_mensual = (indice / lag(indice) - 1) * 100)

UMBRAL_CAMBIO <- 0.1
cambios_ultima_semana <- function(df) {
  rel <- relativos(df) |> filter(is.finite(rel), rel > 0)
  ult <- max(rel$fecha)
  rel |> filter(fecha == ult) |>
    mutate(delta = (rel - 1) * 100,
           tipo = case_when(abs(delta) < UMBRAL_CAMBIO ~ "Sin cambio",
                            delta >= UMBRAL_CAMBIO ~ "Suba", TRUE ~ "Baja"))
}

# Variación semanal cruda por sub-subcategoría (para los recuadros)
var_semanal_subsub <- function(dfg, ult) {
  indice_encadenado(dfg, "subsub_lbl") |> filter(fecha == ult) |>
    transmute(subsub_lbl, var_pct) |> arrange(desc(var_pct))
}

indicadores <- function(df) {
  idx  <- indice_ponderado(df, PESOS_GENERAL, grupo_general)   # general PONDERADO
  vm   <- var_mensual(idx)
  camb <- cambios_ultima_semana(df)
  list(var_semanal = tail(idx$var_pct, 1), var_mensual = tail(vm$var_mensual, 1),
       acumulado = tail(idx$indice, 1) - 100,
       pct_con_cambio = mean(camb$tipo != "Sin cambio") * 100, ultima_fecha = max(df$fecha))
}
