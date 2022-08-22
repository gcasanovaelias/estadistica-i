# Packages ----
library(tidyverse)
library(agricolae)

# Pregunta 1 ----
#* Represente gráficamente de dos maneras diferentes la información del número de cajas exportadas de las siguientes especies:

datos_frutas <- tibble(Especie = c("Uva blanca", "Uva negra y rosada", "Pómaceas", "Carozos"),
                       N.cajas = c(185, 157, 215, 139))

# Barplot
ggplot(data = datos_frutas, aes(x = Especie, y = N.cajas, fill = Especie)) + 
  geom_bar(stat = "identity") +
  labs(title = "N° cajas por especie de fruta",
       subtitle = "Datos",
       y = "N° cajas (miles)",
       x = "Especie") +
  geom_text(aes(label = N.cajas), 
            hjust = 1.6, color = "white", size = 5.5) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # coord_flip(): cambiar las coordenadas (grafico de barras horizontales)
  coord_flip()

# Gráfico de tortas
ggplot(data = datos_frutas, aes(x = "", y = N.cajas, fill = Especie)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) +
  labs(title = "N° cajas por especie de fruta",
       subtitle = "Datos") +
  scale_fill_manual(values = c("dark green", "dark red", "dark blue", "dark orange")) +
  theme_void() +
  theme(legend.position = "bottom")

# Pregunta 2 ----
#* Las causas más frecuentes de atención en caninos en una clínica veterinaria de la comuna de Santiago en dos épocas del año se presenta a continuación:

# Tabular las bases de datos en función de las variables. tienen que haber tantas columnas como variables haya.
datos_canino <- tibble(Causa = c("Neumonía", "Gastritis", "Enteritis", "Parasitismo", "Distemper", "Dermatitis", "Traumatismos"),
                       N.atenciones.verano = c(15, 55, 50, 60, 24, 8, 20),
                       N.atenciones.invierno = c(48, 58, 41, 52, 56, 4, 20))

datos_canino_longer <- datos_canino %>% 
  pivot_longer(cols = starts_with("N.atenciones"),
               names_prefix = "N.atenciones.",
               names_to = "Temporada",
               values_to = "N.atenciones")

# a) Construya un gráfico de sectores circulares por cada época de atención
ggplot(data = datos_canino_longer, aes(x = "", y = N.atenciones, fill = Causa)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  labs(title = "N° atenciones en verano",
       subtitle = "Datos clínica veterinaria") +
  theme_void() +
  theme(legend.position = "bottom") +
  facet_wrap(~Temporada)

# Verano
ggplot(data = datos_canino, aes(x = "", y = N.atenciones.verano, fill = Causa)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y", start = 0) +
  labs(title = "N° atenciones en verano",
       subtitle = "Datos clínica veterinaria") +
  theme_void() +
  theme(legend.position = "bottom")

# Invierno
ggplot(data = datos_canino, aes(x = "", y = N.atenciones.invierno, fill = Causa)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y", start = 0) +
  labs(title = "N° atenciones en invierno",
       subtitle = "Datos clínica veterinaria") +
  theme_void() +
  theme(legend.position = "bottom")


# b) Construya un gráfico para comparar las causas de atención, sin considerar la época. ¿Cuál(es) s(son) las causas de mayor consulta?
datos_canino_anual <- datos_canino_longer %>% 
  group_by(Causa) %>% 
  summarise(N.atenciones = sum(N.atenciones))

ggplot(data = datos_canino_anual, aes(x = Causa, y = N.atenciones, fill = Causa)) +
  geom_bar(stat = "identity") +
  labs(title = "N° atenciones",
       subtitle = "Datos clinica veterinaria",
       y = "N° atenciones",
       x = " ") + 
  geom_text(aes(label = N.atenciones), vjust = 1.6, color = "white", size = 4) +
  theme(legend.position = "bottom")

# c) Construya un gráfico en que se puedan comparar las causas por época ¿Cuál(es) causas son más importantes en verano? ¿Cuál(es) causas son más importantes en invierno?
ggplot(data = datos_canino_longer, aes(x = Causa, y = N.atenciones, fill = Causa)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "N° atenciones",
       subtitle = "Datos clinica veterinaria",
       y = "N° atenciones",
       x = " ") +
  geom_text(aes(label = N.atenciones), vjust = 1.6, color = "white", size = 4) +
  theme(legend.position = "bottom") +
  facet_wrap(~Temporada)

# d) Construya otro gráfico en que se puedan comparar las épocas por causa ¿En cuál época es más crítica el distemper? ¿En cuál época es más crítica la gastritis?

ggplot(data = datos_canino_longer, aes(x = Temporada, y = N.atenciones, fill = Causa)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "N° atenciones",
       subtitle = "Datos clinica veterinaria",
       y = "N° atenciones",
       x = " ") +
  geom_text(aes(label = N.atenciones), vjust = 1.2, color = "white", size = 4) +
  theme(legend.position = "bottom") +
  facet_wrap(~Causa)

# Pregunta 3 ----
# En una encuesta sobre hábito de consumo de ciertas frutas de las familias en las comunas de Ñuñoa y San Miguel, se obtuvo la siguiente información

data_comunas <- tibble(Fruta = c("Uva de mesa", "Duraznos", "Manzanas", "Peras", "Naranjas", "Kiwis", "Guindas"),
                       Ñuñoa = c(15, 19, 17, 8, 10, 27, 12),
                       San.Miguel = c(14, 10, 20, 12, 18, 12, 6))

data_comunas_longer <- data_comunas %>% 
  pivot_longer(cols = -Fruta,
               names_to = "Comuna",
               values_to = "N.familia")

# b) Represente en un gráfico adecuado estos datos mostrando las preferencias por cada comuna.

ggplot(data = data_comunas_longer, aes(x = Fruta, y = N.familia, fill = Fruta)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Consumo de frutas en familias por comunas",
       subtitle = "Comunas de Ñuñoa y San Miguel",
       y = "N° Familia",
       x = "Frutas") + 
  geom_text(aes(label = N.familia), vjust = 1.6, color = "white", size = 4) +
  theme(legend.position = "bottom") +
  facet_wrap(~Comuna)

# c) Construya otro gráfico que permita la comparación adecuada entre las comunas.

ggplot(data = data_comunas_longer, aes(x = Comuna, y = N.familia, fill = Comuna)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Consumo de frutas en familias por comunas",
       subtitle = "Comunas de Ñuñoa y San Miguel",
       y = "N° Familia",
       x = "Comunas") + 
  geom_text(aes(label = N.familia), vjust = 1.6, color = "white", size = 4) +
  theme(legend.position = "bottom") +
  facet_wrap(~Fruta)


# Pregunta 4 ----
# En una encuesta a 600 productores de trigo se les consultó sobre superficie sembrada y la tecnología empleada en su predio. Posteriormente fueron clasificados en tres categorías de tamaño y tres niveles de tecnología, dando origen a la siguiente información

data_productores <- tibble(Tamaño = c("Pequeño", "Mediano", "Grande"),
                           Bajo = c(182, 68, 20),
                           Mediano = c(85, 60, 41),
                           Alto = c(33, 72, 39))

data_productores_tf <- data_productores %>% 
  pivot_longer(cols = -Tamaño,
               names_to = make.names("Nivel tecnologico"),
               values_to = "f") %>% 
  mutate(h = f/sum(f)*100,
         F = cumsum(f),
         H = cumsum(h))

# a) Construya un gráfico que permita comparar adecuadamente nivel tecnológico según tamaño
ggplot(data = data_productores_longer, 
       # fct_relevel(): Cambiar el orden de los factores
       aes(x = forcats::fct_relevel(Nivel.tecnologico, "Bajo", "Mediano", "Alto"), 
           y = f,
           fill = forcats::fct_relevel(Tamaño, "Pequeño", "Mediano", "Grande"))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nivel tecnológico según tamaño de productores de trigo",
       subtitle = "Datos de superficie sembrada y tecnología empleada",
       x = "Nivel tecnológico",
       y = "Frecuencia absoluta (N°)") +
  # Editar el nombre de la leyenda
  guides(fill = guide_legend(title = "Tamaño")) +
  theme(legend.position = "bottom") +
  facet_wrap(~forcats::fct_relevel(Tamaño, "Pequeño", "Mediano", "Grande"))

# b) Construya un gráfico adecuado para comparar tamaño según nivel tecnológico 
ggplot(data = data_productores_longer, 
       aes(x = fct_relevel(Tamaño, "Pequeño", "Mediano", "Grande"), 
           y = f, 
           fill = fct_relevel(Nivel.tecnologico, "Bajo", "Mediano", "Alto"))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tamaño de productores según nivel tecnológico",
       subtitle = "Datos de superficie sembrada y tecnología empleada",
       x = "Tamaño",
       y = "Frecuencia absoluta (N°)") +
  guides(fill = guide_legend(title = "Nivel Tecnológico")) +
  theme(legend.position = "bottom") +
  facet_wrap(~fct_relevel(Nivel.tecnologico, "Bajo", "Mediano", "Alto"))

# Pregunta 5 ----
# La tabla muestra la distribución de 340 plantas enfermas que fueron sometidas a uno de los cuatro tratamientos curativos A, B, C y D, de acuerdo a su condición después de finalizado el tratamiento

datos_plantas <- tibble(Tratamiento = LETTERS[1:4],
                        Mejor = c(13, 34, 22, 35),
                        Igual = c(43, 28, 18, 31),
                        Peor = c(14, 38, 10, 54))

datos_plantas_longer <- datos_plantas %>% 
  pivot_longer(cols = -Tratamiento,
               names_to = "Condicion",
               values_to = "f")

# Construya gráficos en que se puedan comparar los resultados por tratamiento:
s <- ggplot(data = datos_plantas_longer, aes(x = Tratamiento, y = f, 
                                             fill = forcats::fct_relevel(Condicion, "Mejor", "Igual", "Peor"))) +
              labs(title = "Condición de plantas enfermas posterior a tratamientos",
                   subtitle = "Distribución de 340 plantas enfermas",
                   x = "Tratamiento",
                   y = "Frecuencia") +
              theme(legend.position = "bottom")
            
# a) En valores absolutos
s + geom_bar(stat = "identity", position = "stack") 
            
# b) En valores porcentuales
s + geom_bar(stat = "identity", position = "fill")

# Pregunta 6 ----
#  La información de la tabla corresponde a la producción de carne de ganado bovino (en ton.), por categoría, durante 5 años en un matadero de Santiago

dato_carne <- tibble(Año = seq(from = 1997, to = 2001),
                     Novillos = c(89762, 96710, 94104, 114023, 123071),
                     Vacas = c(67270, 74084, 80764, 85450, 90127),
                     Bueyes = c(12941, 14105, 16730, 19836, 21320), 
                     Vaquillas = c(59742, 64200, 70465, 73015, 76842),
                     Terneros = c(12345, 8920, 7450, 6678, 8240))

dato_carne_longer <- dato_carne %>% 
  pivot_longer(cols = -Año,
               names_to = "Categoría",
               values_to = "Producción")

# a) Construya un gráfico lineal que muestre la producción de carne por categoría.
ggplot(data = dato_carne_longer, aes(x = Año, y = Producción, color = Categoría)) +
  geom_line() +
  geom_point() +
  labs(title = "Producción anual de carne bovina por categoría",
       subtitle = "Matadero de Santiago", 
       x = "Años",
       y = "Producción (ton)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# b) Muestre la información anterior mediante un gráfico de barras agrupadas por categoría.
ggplot(data = dato_carne_longer, aes(x = Año, y = Producción, fill = Categoría)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Producción anual de carne bovina por categoría",
       subtitle = "Matadero de Santiago", 
       x = "Años",
       y = "Producción (ton)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Categoría)

# Pregunta 7 ----
# Los embarques de frambuesas frescas a Europa y USA, durante 8 semanas, en miles de cajas, se resume en la tabla a continuación

frambuesas <- tibble(Destino = c("USA", "EUROPA"),
                     S1 = c(34, 10),
                     S2 = c(80, 14),
                     S3 = c(48, 20),
                     S4 = c(59, 27),
                     S5 = c(49, 25),
                     S6 = c(83, 30),
                     S7 = c(47, 13),
                     S8 = c(62, 8))

frambuesas_longer <- frambuesas %>% 
  pivot_longer(cols = starts_with("S"),
               names_to = "Semana",
               names_prefix = "S",
               values_to = "Cantidad")

# Construir un grafico adecuado
# a) Que muestre las cajas totales embarcadas.

frambuesas_total <- frambuesas_longer %>% 
  group_by(Semana) %>% 
  summarise(Cantidad.total = sum(Cantidad))

ggplot(data = frambuesas_total, aes(x = Semana, y = Cantidad.total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cajas totales embarcadas",
       subtitle = "Miles de cajas",
       x = "Semana",
       y = "Cantidad [miles de cajas]") +
  geom_text(aes(label = Cantidad.total), vjust = 1.6, color = "white", size = 4) +
  theme_minimal()
  
# b) Que muestre comparativamente los embarques por destino.

ggplot(data = frambuesas_longer, aes(x = Semana, y = Cantidad, fill = Destino)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cajas totales embarcadas",
       subtitle = "Miles de cajas",
       x = "Semana",
       y = "Cantidad [miles de cajas]") +
  geom_text(aes(label = Cantidad), vjust = 1.6, color = "white", size = 4)+
  facet_wrap(~Destino)

# Pregunta 8 ----
#  La tabla especifica la natalidad y mortalidad por cada 1000 habitantes entre 1950 y 1995

poblacion <- tibble(Metrica = c("Natalidad", "Mortalidad"),
                    "1950" = c(25, 13.2),
                    "1955" = c(23.7, 13),
                    "1960" = c(21.3, 11.7),
                    "1965" = c(18.9, 11.3),
                    "1970" = c(16.9, 10.6),
                    "1975" = c(17.9, 10.8),
                    "1980" = c(19.5, 10.6),
                    "1985" = c(23.6, 9.6),
                    "1990" = c(24.6, 9.3),
                    "1995" = c(25, 8.5))

poblacion_longer <- poblacion %>% 
  pivot_longer(cols = -Metrica,
               names_to = "Año",
               values_to = "Valores") %>% 
  mutate(Año = as.integer(Año))

# a) Represente los datos mediante gráficos adecuados, en tres formas diferentes, uno de tipo lineal
# Lineas y puntos
ggplot(data = poblacion_longer, aes(x = as.integer(Año), y = Valores, color = Metrica)) +
  geom_line() +
  geom_point() +
  labs(title = "Natalidad y mortalidad a través de los años",
       subtitle = "Entre 1950 y 1995",
       x = "Años",
       y = "Valor por cada 100o habitantes") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Barras
ggplot(data = poblacion_longer, aes(x = forcats::fct_relevel(Metrica, "Natalidad", "Mortalidad"), 
                                    y = Valores, fill = forcats::fct_relevel(Metrica, "Natalidad", "Mortalidad"))) +
  geom_bar(stat = "identity") +
  labs(title = "Natalidad y mortalidad a través de los años",
       subtitle = "Entre 1950 y 1995",
       x = "Años",
       y = "Valor por cada 100o habitantes") +
  geom_text(aes(label = Valores), vjust = 1.6, color = "black", size = 4) +
  theme_grey() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Métrica")) + 
  scale_fill_manual(values = c("light blue", "pink")) +
  facet_wrap(~Año)

ggplot(data = poblacion_longer, aes(x = Año, y = Valores, color = Metrica)) +
  geom_bar(stat = "identity", fill = "white") +
  labs(title = "Natalidad y mortalidad a través de los años",
       subtitle = "Entre 1950 y 1995",
       x = "Años",
       y = "Valor por cada 100o habitantes") +
  geom_text(aes(label = Valores), vjust = -0.5, color = "black", size = 4) +
  theme_grey() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Métrica")) + 
  scale_fill_manual(values = c("light blue", "pink")) +
  facet_grid(Metrica ~ .)

# Pregunta 10 ----
# Se cuenta el número de arañitas rojas en 50 hojas de un manzano seleccionadas aleatoriamente, obteniéndose los siguientes datos:
  
arañas <- tibble(Hojas = seq(from = 1, to = 50),
                 N.arañas = c(8, 6, 5, 3, 3, 4, 0, 2, 4, 5, 0, 6, 5, 2, 4, 6, 7, 1, 4, 3, 7, 6, 5, 3, 0, 4, 6, 2, 1, 0, 3, 5, 5, 4, 3, 1, 1, 2, 0, 6, 4, 1, 3, 2, 8, 4, 5, 6, 2, 3))

# Clasifique los datos en una tabla de frecuencias y resuelva los siguientes puntos:
# a) Tabla frecuencia variables cuantitativas discretas
arañas.tb <- arañas %>% 
  pull(N.arañas) %>% 
  table() %>% 
  as_tibble() %>% 
  rename(VC = ".", f = n) %>% 
  mutate(h = f/sum(f)*100,
         F = cumsum(f),
         H = cumsum(h))

# b) Metricas resumen
arañas %>% 
  summarise(Mean = mean(N.arañas),
            Median = median(N.arañas),
            SD = sd(N.arañas),
            Min = min(N.arañas),
            Max = max(N.arañas)) %>% 
  mutate(CV = SD/Mean,
         Range = Max - Min) %>% 
  select(-c(Min, Max)) %>% 
  relocate(CV, .after = Range)

# c) Gráficos (histograma)

ggplot(data = arañas, aes(x = N.arañas)) + 
  # geom_histogram(): histogramas
  geom_histogram(binwidth = 1, fill = "light blue", color = "steelblue") +
  # geom_freqpoly(): polígono de frecuencia
  geom_freqpoly(binwidth = 1, color = "red") +
  geom_vline(data = arañas, 
             aes(xintercept = mean(N.arañas)), linetype = "dashed", color = "dark red") +
  labs(title = "Histograma",
       subtitle = "N° arañas en 50 hojas de manzano",
       x = "N° arañas",
       y = "Conteo [n]")

# Pregunta 11 ----
# En una encuesta a 750 familias se obtuvo la información del número de hijos de cada una de ellas, resumida en la siguiente tabla

# a) Tabla de frecuencia
familia <- tibble(N.hijos = seq(from = 0, to = 8),
                  f = c(40, 140, 220, 160, 85, 45, 25, 20, 15)) 

familia.tf <- familia %>% 
  mutate(h = f/sum(f)*100,
         F = cumsum(f),
         H = cumsum(h))

# b) Medidas resumen
# Media aritmética
familia.mean <- map2(.x = familia$N.hijos,
                     .y = familia$f,
                     # Emplear una fórmula y no una función
                     .f = ~.x*.y/sum(familia$f)) %>% 
  # Sumar todos los elementos de las listas y dividirlos por n
  reduce(sum)

# Desviación estándar
familia.sd <- map2(.x = familia$N.hijos,
                   .y = familia$f,
                   .f = ~(.y*(.x - familia.mean)^2)/sum(familia$f)) %>% 
  reduce(sum) %>% 
  sqrt()

# Coeficiente de variación
familia.CV <- familia.sd/familia.mean*100

# Pregunta 12 ----

puntajes <- {c(42,32,13,18,23,44,41,18,15,25,
               35,28,17,28,42,51,50,21,27,36,
               68,84,75,82,68,90,62,88,76,93,
               73,79,88,73,60,93,71,59,85,75,
               61,65,75,87,74,62,95,78,63,72,
               66,78,82,75,94,77,69,74,68,60,
               46,38,89,21,75,35,60,79,23,31,
               39,42,27,97,78,85,76,65,71,55,
               55,80,63,57,78,68,62,76,53,74,
               66,67,73,81,52,63,76,75,85,47)}

# Rango
max(puntajes) - min(puntajes)

# Tablas de frecuencia. Encuentre el rango de los datos y clasifíquelos en una tabla de frecuencias. Use 7 intervalos
puntajes.tb <- puntajes %>% 
  hist(plot = F, breaks = seq(from = 13, to = 97, by = 12)) %>% 
  agricolae::table.freq() %>% 
  as_tibble() %>% 
  rename(MC = Main, f = Frequency, h = Percentage, F = CF, H = CPF)

# b) ¿cuántos estudiantes obtuvieron MÁS de 75 puntos?
P <- 75; Li <- 73; ci <- 12; fi <- 28; Fi1 <- 62; N <- 100

# Ecuación con la variable k despejada (la original tiene despejada la variable P)
k <- ((P-Li)*fi/ci + Fi1)*100/N; k

100-k #33.3


# c) ¿Qué % de estudiantes obtuvo entre 50 y 70 puntos?
# P70
P <- 70; Li <- 61; ci <- 12; fi <- 22; Fi1 <- 40; N <- 100

k70 <- ((P-Li)*fi/ci + Fi1)*100/N; k70

# P50
P <- 50; Li <- 49; ci <- 12; fi <- 12; Fi1 <- 28; N <- 100

k50 <- ((P-Li)*fi/ci + Fi1)*100/N; k50

k70 - k50 #27.5


# d) Calcule e interprete la media, la mediana
# Media
puntajes.mean <- map2(.x = puntajes.tb$MC,
                      .y = puntajes.tb$f,
                      .f = ~.x*.y/sum(puntajes.tb$f)) %>% 
  reduce(sum)

# Me = P50
k <- 50; Li <- 61; ci <- 12; fi <- 22; Fi1 <- 40; N <- 100

# Ecuación original
P50 <- Li + ((k*N/100)-Fi1)*ci/fi; P50


# e) Calcule e interprete Q1, Q3 y P95
# Q1 = P25
k <- 25; Li <- 37; ci <- 12; fi <- 9; Fi1 <- 19; N <- 100

P25 <- Li + ((k*N/100)-Fi1)*ci/fi; P25

# Q3 = P75
k <- 75; Li <- 73; ci <- 12; fi <- 28; Fi1 <- 62; N <- 100

P75 <- Li + ((k*N/100)-Fi1)*ci/fi; P75

# P95
k <- 95; Li <- 85; ci <- 12; fi <- 10; Fi1 <- 90; N <- 100

P95 <- Li + ((k*N/100)-Fi1)*ci/fi; P95


# f) Calcule la varianza y la desviación estándar de los puntajes obtenidos

puntajes.sd <- map2(.x = puntajes.tb$MC,
                    .y = puntajes.tb$f,
                    .f = ~(.y*(.x-puntajes.mean)^2)/sum(puntajes.tb$f)) %>% 
  reduce(sum) %>% 
  sqrt()

# Pregunta 13 ----
# La tabla corresponde a la clasificación de los pesos de 250 manzanas Granny seleccionadas al azar de la producción de un huerto

# Tabla de frecuencia (datos cuantitativos continuos)
manzanas.tf <- tibble(Lower = seq(from = 120, to = 210, by = 15),
                      Upper = seq(from = 135, to = 225, by = 15),
                      f = c(15, 35, 40, 45, 50, 42, 25)) %>% 
  mutate(MC = (Lower + Upper)/2,
         h = f/sum(f)*100,
         F = cumsum(f),
         H = cumsum(h)) %>% 
  relocate(MC, .after = Upper)

# a) Calcule la media y mediana de los pesos e interprete estos valores.
# Media aritmética
manzanas.mean <- map2(.x = manzanas.tf$MC,
                      .y = manzanas.tf$f,
                      .f = ~.x*.y/sum(manzanas.tf$f)) %>% reduce(sum)

# Median
k <- 50; Li <- 165; N <- sum(manzanas.tf$f); fi <- 45; Fi1 <- 90; ci <- 15

manzanas.Me <- Li + ((k*N/100)-Fi1)*ci/fi; manzanas.Me

# b) Calcule e interprete la varianza, desviación estándar y C.V de los pesos

manzanas.sd <- map2(.x = manzanas.tf$MC,
                    .y = manzanas.tf$f, 
                    .f = ~(.y*(.x - manzanas.mean)^2)/sum(manzanas.tf$f)) %>% 
  reduce(sum) %>% 
  sqrt()

manzanas.CV <- manzanas.sd/manzanas.mean*100

# c) Construya el histograma y el polígono de frecuencia

manzanas.tib <- map2(.x = manzanas.tf$MC, 
                     .y = manzanas.tf$f,
                     .f = ~rep(.x, .y)) %>% 
  # append(): similar a concatenar
  reduce(append) %>% 
  as_tibble()


ggplot(data = manzanas.tib, aes(x = value, y = ..density..)) +
  geom_histogram(fill = "light blue",
                 color = "steelblue",
                 # Breaks de los intervalos
                 breaks = seq(from = 120, to = 225, by = 15)) +
  geom_freqpoly(breaks = seq(from = 120, to = 225, by = 15), color = "red") +
  geom_vline(data = manzanas.tib, 
             # La intercepción será en el valor de la media aritmética de los datos
             aes(xintercept = mean(value)), linetype = "dashed", color = "dark red",
             # No mostrar la leyenda
             show.legend = F) +
  labs(title = "Histograma",
       subtitle = "Frecuencia peso de manzanas",
       x = "Peso manzanas (g)",
       y = "Conteo [n]") +
  theme(legend.position = "bottom")

# d) Calcule e interprete P10 y P75
# P10
k <- 10; Li <- 135; ci <- 15; fi <- 35; Fi1 <- 15; N <- manzanas.tf$f %>% sum()

P10 <- Li + ((k*N/100)-Fi1)*ci/fi; P10

# P75
k <- 75; Li <- 195; ci <- 15; fi <- 42; Fi1 <- 185; N <- manzanas.tf$f %>% sum()

P75 <- Li + ((k*N/100)-Fi1)*ci/fi; P75

# e) ¿Qué % de las manzanas pesa menos de 140 gr?
P <- 140; Li <- 135; ci <- 15; fi <- 35; Fi1 <- 15; N <- manzanas.tf$f %>% sum()

k <- ((P-Li)*fi/ci + Fi1)*100/N; k

# f) ¿Cuántas de las 250 manzanas pesan más de 200 gr?
P <- 200; Li <- 195; ci <- 15; fi <- 42; Fi1 <- 185; N <- manzanas.tf$f %>% sum()

k <- ((P-Li)*fi/ci + Fi1)*100/N

(k/100) * manzanas.tf$f %>% sum()

# g) ¿Qué % de las manzanas tienen pesos entre µ ± ?????
# sup: 201.5301
manzanas.mean + manzanas.sd
# inf: 149.8985
manzanas.mean - manzanas.sd

# sup
P <- manzanas.mean + manzanas.sd; Li <- 195; ci <- 15; fi <- 42; Fi1 <- 185; N <- manzanas.tf$f %>% sum()

k <- ((P-Li)*fi/ci + Fi1)*100/N;k # 80.66832

# inf
P <- manzanas.mean - manzanas.sd; Li <- 135; ci <- 15; fi <- 35; Fi1 <- 15; N <- manzanas.tf$f %>% sum()

k <- ((P-Li)*fi/ci + Fi1)*100/N;k # 19.7473

80.66832 - 19.7473 # 60.92102

# h) ¿Entre qué pesos está comprendido el 90% central de las manzanas?
# 90% central: entre el 5% y 95%
# P95
k <- 95; Li <- 210; ci <- 15; fi <- 25; Fi1 <- 227; N <- manzanas.tf$f %>% sum()

P95 <- Li + ((k*N/100)-Fi1)*ci/fi; P95 # 217.44

# P5
k <- 5; Li <- 120; ci <- 15; fi <- 15; Fi1 <- 0; N <- manzanas.tf$f %>% sum()

P5 <- Li + ((k*N/100)-Fi1)*ci/fi; P5 # 132.6

# Pregunta 14 ----
# . Calcule el promedio ponderado de un alumno que obtuvo en un ramo las siguientes calificaciones con sus correspondientes ponderaciones:

notas <- tibble(Notas = c(4.5, 3.2, 5.4, 5),
                Ponderacion = 1:4)

# weighted.mean(): Calcular el promedio ponderado
weighted.mean(
  # Notas
  x = notas$Notas,
  # Ponderación
  w = notas$Ponderacion)

# Pregunta 15 ----
# . Un inversionista posee tres tipos de acciones A, B y C en proporción 3:7:5 ¿Cuál es su ganancia promedio por acción si la ganancia de las acciones tipo A, B y C son $250, $380 y $170 respectivamente?

acciones <- tibble(Acciones = LETTERS[1:3],
                   Proporcion = c(3, 7, 5),
                   Ganancias = c(250, 380, 170))

weighted.mean(x = acciones$Ganancias,
              w = acciones$Proporcion)

