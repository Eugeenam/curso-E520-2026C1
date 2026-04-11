
# 3.2.5. Ejercicios, Transformacion de datos en flights -------------------
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

nycflights13::flights
View(flights)


# 1) i) Had an arrival delay of two or more hours
flights |> 
  filter(dep_delay > 120)

#ii) Flew to Houston (IAH or HOU)
flights |> 
  filter(dest %in% c("IAH", "HOU"))

#iii) Were operated by United, American, or Delta
flights |> 
  filter(carrier %in% c("UA", "AA", "DL"))

#iv) Departed in summer (July, August, and September)
flights |> 
  filter(month %in% c(7, 8, 9))

#v) Arrived more than two hours late but didn’t leave late
flights |> 
  filter(arr_delay > 120, dep_delay <= 0)

#vi) Were delayed by at least an hour, but made up over 30 minutes in flight
flights |> 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)

#2) flights with the longest departure delays & that left earliest in the morning.
# Mayor retraso
flights |> 
  arrange(desc(dep_delay))

# Más temprano
flights |> 
  arrange(dep_time)

#3) find the fastest flights
flights |> 
  arrange(desc(distance / air_time)) # consideramos la velocidad como distancia/tiempo


#4) Was there a flight on every day of 2013?
flights |> 
  distinct(year, month, day)
#hubieron vuelos todos los dias

#5) farthest distance? & traveled the least distance?
# Mayor distancia
flights |> arrange(desc(distance))

# Menor distancia
flights |> arrange(distance)


#6) Does it matter what order you used filter() and arrange() if you’re using both?
#Sí importa el orden porque, aunque el resultado final es el mismo, es más eficiente usar 
#primero filter() y luego arrange(). Esto se debe a que filter() reduce la cantidad de filas a 
#ordenar, de forma que arrange() trabaje con menos datos. Al revés, ordenas todo el 
#conjunto de datos original y filter descarta muchas de las filas ordenadas


# 19.2.4 Ejercicios, Keys ------------------------------------------------
#1) Relacion weather y airports
#weather tiene una variable origin que se conecta con airports$faa. 
#En el diagrama debe haber una línea que una weather$origin con airports$faa, con una flecha que indique una relación muchos a uno (muchos registros de clima apuntan a un solo aeropuerto)

#2) Si weather tuviera datos de todos los aeropuertos de EE. UU.
#Se conectaría con flights a través de dest. Por el momento, weather solo se conecta con origin. Con datos de todos los aeropuertos, se podría unir weather a flights tanto por origin (clima de salida) como por dest (clima de llegada), permitiendo analizar cómo afecta el clima en el destino a los retrasos.

#3) La hora especial con duplicados en weather
#La hora duplicada en weather se debe al cambio de horario (daylight saving), donde una hora se repite.

#4) Representar días especiales con pocos vuelos

special_days <- tibble(
  year = 2013,
  month = c(12,12),
  day = c(24,25),
  name = c("Christmas Eve","Christmas")
)
#Primary key --> (year, month, day)
# Se conecta con flights mediante year, month, day (relación: muchos vuelos pertenecen a un mismo día especial). También podría conectarse con weather de la misma forma.

#5) Diagramas con la base Lahman
#Batting y Salaries se relacionan con People por playerID, Managers se conecta con People, y AwardsManagers con Managers, mientras que Batting, Pitching y Fielding son tablas paralelas del mismo jugador.


# 19.3.4 Ejercicios, Basic joins -------------------------------------------------------
library(dplyr)
library(ggplot2)

#1) 48hs with worst delays
flights |>
  mutate(hour = dep_time %/% 100) |>
  group_by(year, month, day, hour, origin) |>
  summarise(delay = mean(arr_delay, na.rm = TRUE)) |>
  arrange(desc(delay)) |>
  slice_head(n = 48) |>
  left_join(weather, by = c("year","month","day","hour","origin"))
#las peores horas coinciden con malos climas

#2) How can you find all flights to those destinations?
top_dest <- flights |>
  count(dest, sort = TRUE) |>
  slice_head(n = 10)

flights |>
  filter(dest %in% top_dest$dest)

#3) Does every departing flight have corresponding weather data for that hour?
flights |>
  mutate(hour = dep_time %/% 100) |>
  anti_join(weather, by = c("year","month","day","hour","origin"))
#hay vuelos sin datos del clima

#4) What do the tail numbers that don’t have a matching record in planes have in common? 
flights |>
  anti_join(planes, by = "tailnum") |>
  count(carrier, sort = TRUE)
#muchos pertenecen a ciertos carriers

#5) Confirm or reject this hypothesis using the tools you’ve learned in previous chapters.
flights |>
  distinct(tailnum, carrier) |>
  count(tailnum) |>
  arrange(desc(n))
#no siempre, algunos aviones cambian de aerolinea

#6) Is it easier to rename the columns before or after the join?
flights |>
  left_join(airports, by = c("origin" = "faa")) |>
  left_join(airports, by = c("dest" = "faa"), suffix = c("_origin", "_dest"))

#7) show the spatial distribution of delays.
avg_delay <- flights |>
  group_by(dest) |>
  summarise(delay = mean(arr_delay, na.rm = TRUE))

airports |>
  inner_join(avg_delay, by = c("faa" = "dest")) |>
  ggplot(aes(lon, lat, size = delay, color = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
#se puede observar un patron en donde hay mayor delay en el este
# y en zonas lejanas y de mal clima

#8) What happened on June 13 2013?
flights |>
  filter(year == 2013, month == 6, day == 13) |>
  summarise(delay = mean(arr_delay, na.rm = TRUE))
#hubo delays muy altos




