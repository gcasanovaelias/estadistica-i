# Packages ----
library(tidyverse)
library(agricolae)

# Datos ----
datos <- datos.1

str(datos)

# Medidas estadisticas ----
# Agrupación por Zona y sexo
datos %>% 
  group_by(Zona, Sexo) %>% 
  summarise_all(funs(mean, median, sd, min, max)) %>% 
  mutate(Pesos_CV = Pesos_mean/Pesos_sd,
         Pesos_range = Pesos_max - Pesos_min,
         Largo_CV = Largo_mean/Largo_sd,
         Largo_range = Largo_max - Largo_min) %>% 
  relocate(starts_with("Pesos"), .before = Largo_mean) %>% 
  select(-ends_with(c("max", "min"))) %>% View()

# Agrupación por zona
datos %>% 
  group_by(Zona) %>% 
  select(-Sexo) %>% 
  summarise_all(funs(mean, median, sd, min, max)) %>% 
  mutate(Pesos_CV = Pesos_mean/Pesos_sd,
         Pesos_range = Pesos_max - Pesos_min,
         Largo_CV = Largo_mean/Largo_sd,
         Largo_range = Largo_max - Largo_min) %>% 
  relocate(starts_with("Pesos"), .before = Largo_mean) %>% 
  select(-ends_with(c("max", "min"))) %>% View()

# Agrupación por sexo
datos %>% 
  group_by(Sexo) %>% 
  select(-Zona) %>% 
  summarise_all(funs(mean, median, sd, min, max)) %>% 
  mutate(Pesos_CV = Pesos_mean/Pesos_sd,
         Pesos_range = Pesos_max - Pesos_min,
         Largo_CV = Largo_mean/Largo_sd,
         Largo_range = Largo_max - Largo_min) %>% 
  relocate(starts_with("Pesos"), .before = Largo_mean) %>% 
  select(-ends_with(c("max", "min"))) %>% View()

# Tablas de frecuencia ----
# Variable cuantitativa continua (peso)
tf_pesos <- hist(x = datos$Pesos, plot = F) %>% 
  agricolae::table.freq() %>% 
  as_tibble() %>% 
  rename(MC = Main, f = Frequency, h = Percentage, F = CF, H = CPF)

# Variable cuantitativa continua (largo)
tf_largo <- hist(x = datos$Largo, plot = F) %>% 
  agricolae::table.freq() %>% 
  as_tibble() %>% 
  rename(MC = Main, f = Frequency, h = Percentage, F = CF, H = CPF)

# Histogramas
# Histograma (peso)

ggplot(data = datos, aes(x = Pesos, fill = Sexo)) +
  geom_histogram(binwidth = 50,
                 alpha = 0.5) +
  geom_vline(data = datos, aes(xintercept = mean(Pesos), color = Sexo), linetype = "dashed") +
  labs(title = "Histograma",
       subtitle = "Peso chinchillas según sexo y zona",
       x = "Peso individuos [g]",
       y = "Conteo [n]") +
  # facet_grid(): Divide los graficos en paneles de manera vertical y horizontal según distintas variables factores
  facet_grid(Zona ~ Sexo)

# Histograma (largo)

ggplot(data = datos, aes(x = Largo, fill = Sexo)) +
  geom_histogram(binwidth = 2,
                 alpha = 0.5) +
  geom_vline(data = datos, aes(xintercept = mean(Largo), color = Sexo), linetype = "dashed") +
  labs(title = "Histograma",
       subtitle = "Largo chinchillas según sexo y zona",
       x = "Largo individuos [cm]",
       y = "Conteo [n]") +
  facet_grid(Zona ~ Sexo)

# Gráficos ----
# Boxplots
# Pesos
  # Tipo 1
ggplot(data = datos, aes(x = Sexo, y = Pesos, color = Sexo)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(title = "Peso individuos según zona y sexo",
       subtitle = "Población de chinchillas",
       x = "Sexo",
       y = "Peso [g]") +
  theme(legend.position = "bottom") +
  facet_wrap(~Zona)

  # Tipo 2
ggplot(data = datos, aes(x = "", y = Pesos, color = Sexo)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(title = "Peso individuos según zona y sexo",
       subtitle = "Población de chinchillas",
       x = "",
       y = "Peso [g]") +
  # Para eliminar la leyenda
  theme(legend.position = "none") +
  facet_grid(Zona ~ Sexo)

# Largo
ggplot(data = datos, aes(x = Sexo, y = Largo, color = Sexo)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(title = "Largo individuos según zona y sexo",
       subtitle = "Población de chinchillas",
       x = "Sexo",
       y = "Largo [cm]") +
  theme(legend.position = "bottom") +
  facet_wrap(~Zona)

# Gráfico de barras
ggplot(data = datos, aes(x = Zona, y = Pesos, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Peso individuos según zona y sexo",
       subtitle = "Población de chinchillas",
       x = "Sexo",
       y = "Peso [g]") +
  theme(legend.position = "bottom")
