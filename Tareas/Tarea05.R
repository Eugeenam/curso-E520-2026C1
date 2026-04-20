# ======================================================================
# TRABAJO PRÁCTICO FINAL - ANÁLISIS DE MOVIMIENTO AÉREO
# ESTUDIANTE: Tomás Benítez
# LEGAJO: TB-4521
# ======================================================================

# 1) CARGAR PAQUETES ----------------------------------------------------

library(tidyverse)   # limpieza y gráficos
library(readr)       # importar CSVs
library(lubridate)   # fechas

# 2) LEER BASES DE DATOS ------------------------------------------------

# Datos anuales provistos por el ministerio
base_2025 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202512-informe-ministerio-actualizado-dic-final.csv")
base_2024 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202412-informe-ministerio-actualizado-dic-final.csv")
base_2023 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202312-informe-ministerio-actualizado-dic.csv")
base_2022 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202212-informe-ministerio-actualizado-dic-final.csv")
base_2021 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202112-informe-ministerio-actualizado-dic-final.csv")
base_2020 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/202012-informe-ministerio-actualizado-dic-final.csv")
base_2019 <- read_csv2("CienciaDatos/curso-E520-2026C1/Clase/201912-informe-ministerio-actualizado-dic-final.csv")

# Tabla de códigos de aeropuertos
aeropuertos_codes <- read_csv("CienciaDatos/curso-E520-2026C1/Clase/iata-icao.csv")

# Datos meteorológicos (formato fijo)
temperaturas <- read_fwf("CienciaDatos/curso-E520-2026C1/Clase/registro_temperatura365d_smn.txt", 
                         col_positions = fwf_widths(
                           c(8, 1, 5, 1, 5, 200), 
                           c('fecha_obs', 'sep1', 'temp_max', 'sep2', 'temp_min', 'estacion')
                         ),
                         skip = 3) |> 
  select(-sep1, -sep2)

# 3) PREPARACIÓN Y LIMPIEZA ---------------------------------------------

# Unifico tipo de dato de Aeronave para que no haya conflictos
lista_anual <- list(base_2019, base_2020, base_2021, base_2022, 
                    base_2023, base_2024, base_2025)
lista_anual <- map(lista_anual, ~ mutate(.x, Aeronave = as.character(Aeronave)))

# Uno todo en un solo dataframe
vuelos_historicos <- bind_rows(lista_anual)

# Convierto fechas
temperaturas <- temperaturas |> mutate(fecha_dmy = dmy(fecha_obs))
vuelos_historicos <- vuelos_historicos |> mutate(fecha_dmy = dmy('Fecha UTC'))

# Paso variables categóricas a factor
vuelos_historicos <- vuelos_historicos |> 
  mutate(
    tipo_vuelo = factor('Clasificación Vuelo'),
    movimiento = factor('Tipo de Movimiento'),
    linea = factor('Aerolinea Nombre')
  )

# 4) AGREGACIÓN MENSUAL -------------------------------------------------

vuelos_por_mes <- vuelos_historicos |> 
  mutate(año_mes = floor_date(fecha_dmy, "month")) |> 
  group_by(año_mes, tipo_vuelo) |> 
  summarise(cantidad = n(), .groups = "drop")

# 5) GRÁFICOS Y ANÁLISIS ------------------------------------------------

# A) ¿Qué muestra la serie durante la crisis sanitaria?
ggplot(vuelos_por_mes, aes(x = año_mes, y = cantidad, color = tipo_vuelo)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-20")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Evolución del tráfico aéreo mensual (2019-2025)",
       subtitle = "El colapso se produce en marzo de 2020 (marca roja)",
       x = "Período", y = "Total de operaciones") +
  theme_bw() +
  theme(legend.position = "bottom")

# B) ¿Cuándo se recuperaron los niveles previos a la pandemia?
promedio_2019_pre <- vuelos_por_mes |> 
  filter(year(año_mes) == 2019) |> 
  summarise(media_anual = mean(cantidad)) |> pull(media_anual)

ggplot(vuelos_por_mes, aes(x = año_mes, y = cantidad)) +
  geom_line(color = "darkblue") +
  geom_hline(yintercept = promedio_2019_pre, color = "forestgreen", 
             linetype = "longdash", linewidth = 0.9) +
  annotate("text", x = as.Date("2021-06-01"), y = promedio_2019_pre + 800, 
           label = "Referencia 2019", color = "forestgreen", fontface = "bold") +
  labs(title = "Trayectoria post-pandemia vs. nivel histórico",
       x = "Fecha", y = "Vuelos mensuales") +
  theme_light()

# C) Cambios en los destinos favoritos antes y después
# Creo variable de período
vuelos_con_periodo <- vuelos_historicos |> 
  rename(cod_destino = `Origen / Destino`) |> 
  mutate(periodo = if_else(fecha_dmy < "2020-03-01", "Antes", "Después"))

# Calculo top 5 por período
ranking_destinos <- vuelos_con_periodo |> 
  count(periodo, cod_destino) |> 
  group_by(periodo) |> 
  slice_max(n, n = 5) |> 
  ungroup() |> 
  left_join(aeropuertos_codes, by = c("cod_destino" = "iata")) |> 
  select(periodo, cod_destino, nombre_aeropuerto = airport, cantidad_vuelos = n)

print(ranking_destinos)

# Visualización
ggplot(ranking_destinos, aes(x = reorder(cod_destino, cantidad_vuelos), 
                             y = cantidad_vuelos, fill = periodo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  labs(title = "Comparativa de destinos: antes y después de la pandemia",
       x = "Aeropuerto (código)", y = "Volumen de operaciones") +
  scale_fill_manual(values = c("Antes" = "steelblue", "Después" = "tomato")) +
  theme_minimal()

# 6) EFECTO DE LA TEMPERATURA EN 2025 -----------------------------------

# Resumen diario para 2025
vuelos_diario_2025 <- vuelos_historicos |> 
  filter(year(fecha_dmy) == 2025) |> 
  group_by(fecha_dmy) |> 
  summarise(total_diario = n(), .groups = "drop")

# Unión con datos climáticos
base_clima_vuelos <- vuelos_diario_2025 |> 
  left_join(temperaturas, by = "fecha_dmy")

# Gráfico de relación
ggplot(base_clima_vuelos, aes(x = temp_max, y = total_diario)) +
  geom_point(alpha = 0.5, color = "purple4") +
  geom_smooth(method = "lm", se = TRUE, color = "orange", fill = "gray90") +
  labs(title = "Influencia de la temperatura máxima en la cantidad de vuelos",
       subtitle = "Datos diarios del año 2025",
       x = "Temperatura máxima registrada (°C)", 
       y = "Operaciones diarias") +
  theme_minimal()

# ======================================================================
# RESPUESTAS A LAS PREGUNTAS
# ======================================================================

  #1. PANDEMIA (2020-2021):
  #    El ASPO de marzo 2020 generó un colapso casi inmediato de la actividad.
  #    Los vuelos domésticos se detuvieron antes que los internacionales (que
  #    llegaron a cero al cierre de fronteras) y también se recuperaron antes,
  #    ya que las restricciones internas se fueron levantando durante 2021.
  
  # 2. RECUPERACIÓN:
  #    El tráfico total tardó cerca de 3 años en volver a niveles de 2019.
  #    A partir de mediados de 2022 los vuelos domésticos superaron su nivel
  #    pre-pandemia; los internacionales recién lo lograron hacia fines de 2022
  #    y principios de 2023.
  
  # 3. PATRONES ANTES/DESPUÉS:
  #    La estacionalidad se mantuvo: los picos de verano (enero) e invierno
  #    (julio) siguen siendo los momentos de mayor actividad. Sin embargo,
  #    el volumen promedio post-pandemia es notablemente más alto que en 2019,
  #    lo que sugiere una expansión real del mercado, no solo recuperación.