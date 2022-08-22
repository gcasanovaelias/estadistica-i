# Pregunta 18 ----
# En un predio se determinó el porcentaje de animales enfermos y el número de cabezas por raza, los que se resumen en la tabla:

raza <- tibble(Raza = c("Hereford", "Angus", "Charoláis"),
               h.enfermos = c(2.5, 3.4, 5),
               f.total = c(1200, 800, 2400))

# a) Calcule el número de animales enfermos por raza

raza <- raza %>% 
  group_by(Raza) %>% 
  mutate(f.enfermos = f.total*h.enfermos/100)

# b) Calcule el porcentaje promedio simple de animales enfermos en el predio.

weighted.mean(x = raza$h.enfermos,
              w = raza$f.total)

# c) Calcule el porcentaje de animales enfermos en el predio, sin importar raza.

raza %>% 
  pull(h.enfermos) %>% 
  mean()

# Pregunta 19 ----
# Durante un mes los siguientes ingredientes de una ración tuvieron la variación de precios que se indican:

ingredientes <- tibble(Ingrediente = c("Maíz", "Cebada", "Heno", "Afrechillo", "Harina de pescado", "Otros"),
                       Var.percen = c(10, -6, -8, 5, 7, 12),
                       Costo = c(15, 5, 4, 6, 9, 3))

# a) calcule la variación promedio en el mes, sin considerar el costo de los ingredientes

ingredientes %>% 
  pull(Var.percen) %>% 
  mean()

# b) calcule la variación promedio en el mes, considerando el costo de los ingredientes

weighted.mean(x = ingredientes$Var.percen,
              w = ingredientes$Costo)

# Pregunta 20 ----
# Un enfermo obtuvo los siguientes resultados en 3 exámenes: A= 50,35; B= 5,48; C= 0,03 Se sabe que estas pruebas en individuos sanos se caracteriza por los siguientes valores

examen <- tibble(Examen = LETTERS[1:3],
                 Results = c(50.35, 5.48, 0.03),
                 Mean = c(45.2, 5.31, 0.02),
                 SD = c(3.432, 0.574, 0.003))

s <- ggplot(data = examen, aes(x = "", y = Mean, color = Examen)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2,
                position = position_dodge(0.05),
                show.legend = F) +
  geom_point(data = examen, aes(y = Results), shape = "circle open") +
  labs(title = "Comparación exámenes",
       subtitle = "Resultados A, B y C",
       x = "",
       y = "Valores promedio")

s + facet_wrap(~ Examen, 
               dir = "v",
               scales = "free",
               strip.position = "top")

s + facet_grid(Examen ~ .,
               scales = "free")

# Pregunta 22 ----
# En dos poblaciones A y B los pesos promedios y su correspondiente desviación estándar son 65 ± 3 kg para la población A y 68 ± 10 kg para la población B

pesos <- tibble(Población = LETTERS[1:2],
                Mean = c(65, 68),
                SD = c(3, 10))

ggplot(data = pesos, aes(x = Población, y = Mean, color = Población)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +
  geom_hline(aes(yintercept = 55), color = "steelblue", linetype = "dashed") +
  labs(title = "Comparación poblaciones",
       subtitle = "Pesos promedios y desviación estándar",
       x = "Población",
       y = "Peso promedio [kg]")

# Pregunta 23 ----
# Tres atletas A, B, C a ser seleccionados para las olimpiadas marcaron los siguientes tiempos en 5 ensayos de los 100 metros planos.

tiempos <- tibble(A = c(11.1, 11, 11.8, 15.8, 11.1),
                  B = c(11.3, 11.4, 11.5, 11.6, 11.4),
                  C = c(10.9, 11, 11.8, 11.7, 11.6)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Atleta",
               values_to = "Tiempo")

tiempos %>% 
  group_by(Atleta) %>% 
  summarise(t.mean = mean(Tiempo),
            t.sd = sd(Tiempo))

# Pregunta 25 ----
# La producción diaria de leche, en litros, obtenida por 7 productores son:

leche <- tibble(Produccion = c(1000, 500, 800, 2000, 1350, 950, 23500))

leche %>% 
  summarise(P.mean = mean(Produccion),
            P.median = median(Produccion))

# Pregunta 29 ----
# De la tabla de frecuencia del problema 4, ¿Cuál es el:
# a) % de predios tamaño mediano?
data_productores_tf %>% 
  group_by(Tamaño) %>% 
  summarise(f = sum(f)) %>% 
  mutate(h = f/sum(f)*100)

# b) % de predios con nivel tecnológico alto?
data_productores_tf %>% 
  group_by(Nivel.tecnologico) %>% 
  summarise(f = sum(f)) %>% 
  mutate(h = f/sum(f)*100)

# Pregunta 30 ----
# El siguiente cuadro corresponde a la distribución de edades de los padres en un colegio:

padres <- tibble("Mujeres" = c("20 a 25", "25 a 30", "30 a 35", "35 a 40", "40 a 45", "45 a 50"),
                 "20 a 25" = c(5, 8, 2, 0, 0, 0),
                 "25 a 30" = c(3, 10, 7, 8, 0, 0),
                 "30 a 35" = c(0, 2, 12, 18, 4, 3),
                 "35 a 40" = c(0, 0, 4, 12, 3, 5),
                 "40 a 45" = c(0, 0, 0, 2, 6, 7),
                 "45 a 50" = c(0, 0, 0, 0, 7, 15))

padres_tf <- padres %>% 
  pivot_longer(cols = -Mujeres,
               names_to = "Hombres", 
               values_to = "f")

# a) ¿Qué porcentaje de las madres tienen entre 30 y 35 años?
mujeres <- padres_longer %>% 
  group_by(Mujeres) %>% 
  summarise(f = sum(f)) %>% 
  mutate(h = f/sum(f)*100,
         "Sexo" = "Mujeres")

# b) ¿Qué porcentaje de los hombres tienen edades entre 40 y 50 años?
hombres <- padres_longer %>% 
  group_by(Hombres) %>% 
  summarise(f = sum(f)) %>% 
  mutate(h = f/sum(f)*100,
         "Sexo" = "Hombres")

# c) Calcule los promedios de edades de hombres y mujeres y comente cual es la diferencia de edades entre los padres y las madres.
padres_list <- list(Mujeres = mujeres,
                    Hombres = hombres) %>% 
  map(.f = ~mutate(.data = .x, 
                   LI = seq(20, 45, by = 5),
                   LS = seq(25, 50, by = 5),
                   MC = seq(22.5, 47.5, by =5))) %>% 
  map(.f = ~select(.data = .x, f, h, MC, Sexo, LI, LS)) %>% 
  map(.f = ~relocate(.data = .x, 
                     c(LI, LS, MC), 
                     .before = everything())) %>% 
  # tambien puede ser bind_rows(), rbind(), addrow()
  reduce(add_row)

map2(.x = padres_list %>% filter(Sexo == "Hombres") %>% pull(MC),
     .y = padres_list %>% filter(Sexo == "Hombres") %>% pull(f),
     .f = ~.x*.y/sum(padres_list %>% filter(Sexo == "Hombres") %>% pull(f))) %>% 
  reduce(sum)


