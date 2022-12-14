# R ----
# R es un lenguaje de programaci?n basado en el objetos. 
#* No s?lo se pueden crear objetos si no que estos pueden formar parte de las operaciones (dentro de ellos o entre distintos objetos)

# Clases de objetos
#* Existen 5 tipos de objetos que son manejables en R 

chr <- c("Estadistica", "RRNN", "UCHILE") # (1) Caracter

num <- c(1,2,3) # (2) Num?rico (continuo)

int <- c(1L,# (3) Entero (num?rico discreto) 
         2L,3L) #Se designan con un L (may?scula). Discretizaci?n de un valor, necesario para la construcci?n de tabla de frecuencias m?s adelante (si se realiza con continuas sale error)

cplx <- c(8i, # (4) Complejo. i = raiz de -1 (imaginario).
          9 + 9i, # Designaci?n de n?mero complejos tambien involucra n?mero reales (adem?s de imaginarios)
          10)

logi <- c(FALSE, FALSE, TRUE, F, T) # (5) L?gico. Son objetos que en su interior presentan verdaderos (T) o falsos (F). Presentan una gran utilidad a la hora de filtrar.

# ?Qu? tipo de objeto es? ?Cual es su naturaleza?

str(cplx)

class(cplx)

typeof(cplx)

# Coercion ----
# Implicit coercion
#* R acepta objetos "puros" (de un solo tipo). El tipo de objetos es excluyente. No se puede tener dos naturalezas en el mismo objeto.
#* R toma la decisi?n de si un objeto es de un tipo u otro

b <- c(1,2,FALSE);b;str(b) #F: se convierte en 0 debido a que el objeto creado es del tipo num?rico (coerciona a otros valores a convertirse en num)

# Explicit coercion
#* Cambio forzado (por el usuario) del tipo de objeto

# Forzar al objeto b (num) a ser l?gico (logi)
as.logical(b) # F: 0, T: todo lo que sea distinto de 0 ser? T

# No siempre ser? posible realizar una coersi?n expl?cita sobre un objeto
f <- c("hola", "chao");f;str(f)

as.numeric(f) #NA: Not Available

# Datos ----
#* Existen las matrices, data frames, vectores y listas. 

# A pesar de que los objetos son excluyentes, se pueden combinar objetos de distinta naturaleza manteniendo su independencia (lista)
list(chr, cplx, logi)

# Del mismo modo, en los data frames cada columna puede presentarse una naturaleza distinta
df <- data.frame(Pais = c("Chile", "Per?", "Bolivia", "Uruguay"),
                 Habitantes = c(18, 22, 9, 5),
                 PIB = c(100, 200, 300, 350))

str(df)

df.1 <- data.frame(A = chr, B = cplx)

# Subseteo 
df[,1]

# Seleccionar los paises que presenten una cantidad de habitantes mayores de 10

df$Habitantes > 10 # Vector del tipo l?gico que permite realizar el filtro

df$Pais[df$Habitantes > 10]

df[df$Habitantes >10 & df$Habitantes < 19,]

# Tidyverse 
df %>% filter(Habitantes < 10, PIB > 300) %>% pull(Pais)

# Modificar df
colnames(df) <- c("Paises", "Hab", "PIB");df

df[df$PIB == 200,]

df[2,3] <- 250

rownames(df) <- c("P1", "P2", "P3", "P4")

# Importar un set de datos

# Directorio
getwd() # Para saber donde est? el directorio

setwd() # Para seleccionar un directorio

# Funciones para importar
#* Tambien se puede emplear el "Import Dataset" en la ventana de Environment (al lado de la escoba) 
data <- read.table(file = "ejemplotext.txt", header = T, sep = " ") # Documento de texto
  #* Tiene que asignarse a un objeto para que funcione

read.csv(file = "ejemplocsv.csv", header = T, sep = ";") # Documento csv

install.packages("readxl")
library(readxl)
readxl::read_xls("ejemploexel.xlls") #tibble es un tipo de data frame

# Tambien se puede poner la ruta completa de un archivo que no se encuentre en el directorio
read.table("~/Casanova/Estudio Personal/ejemplotext.txt")

# Exportar datos
# Llevarlo de un objeto R a un archivo excel

install.packages("writexl") # An?logo a readxl
library(writexl)

writexl::write_xlsx(x = df, path = "datox.xlsx")

# Tidyverse ----
data(iris)

# Primera aproximaci?n
str(iris)

head(x = iris, n = 10) # Ver las primeras observaciones de la base de datos

tail(iris) # Ver las ?ltimas observaciones de la base de datos

# ?Qu? especies est?n presentes?
levels(iris$Species)

unique(iris$Species)

# Filtrado
virginica <- iris[iris$Species == "virginica" & iris$Sepal.Length > 6.5,]

# Tidyverse
install.packages("tidyverse") # Colecci?n de 8 librerias
library(tidyverse)

# Pipeline ( %>% ) hace referencia a una secuencia de instrucciones (chaining)

iris %>% 
  # filter(): filtrado
  dplyr::filter(Species == "versicolor", Sepal.Length > 6.5) %>% 
  # arrange(): Ordenar tus datos, desc(): ordenar de manera descendente 
  arrange(desc(Sepal.Width)) %>% 
  # mutate(): Modifica columna/variable o agrega una adicional
  mutate(Ancho = c(1,2,3,4,5,6,7,8), # Agregar una columna nueva (a partir de un nuevo nombre)
         Sepal.Width = Sepal.Width/100) %>% # Modificar una columna ya existente (ocupar el mismo nombre)
  # Exportar con write_xlsx
  writexl::write_xlsx("prueba.xlsx")

iris %>% 
  # group_by(): agrupaci?n de observaciones seg?n una columna o variable. Si no deseo hacer una distinci?n por especie simplemente se elimina el group_by()
  group_by(Species) %>% 
  # summarise(): resumir descriptivamente una variable
  summarise(media = mean(Sepal.Length),
            desvest = sd(Sepal.Length),
            median = median(Sepal.Length),
            rango = max(Sepal.Length) - min(Sepal.Length),
            CV = sd(Sepal.Length)/mean(Sepal.Length)*100)

iris %>% 
  # select(): seleccionar columnas
  select(-Species) %>% 
  # summarise_all(): calcular estad?grafos para todas las variables
  summarise_all(funs(mean, sd,median))

iris %>% 
  group_by(Species) %>% 
  summarise_all(.funs = list(mean, sd, median))

# Tablas de frecuencia variables cualitativas ----
# table(): calcular la tabla de frecuencia para una variable cualitativa
tabla <- table(iris$Species[iris$Sepal.Length > 5])

str(tabla)
# Es un objeto de tipo tabla, mejor transformarlo a un data frame

tabla <- table(iris$Species[iris$Sepal.Length > 5]) %>% 
  # as.data.frame(): conversi?n a un data frame
  as.data.frame() %>% 
  rename(Species = Var1, Frequency = Freq) %>% 
  # Calcular la frecuencia relativa
  mutate(Rel.Freq = round(Frequency/sum(Frequency)*100, 2));tabla


# prop.table(): calcular la proporci?n de una tabla de frecuencia
prop.table(table(iris$Species[iris$Sepal.Length > 5]))

# Gr?ficos con ggplot2 ----

# Librer?a base (graphic)
iris$Species[iris$Sepal.Length > 5] %>% 
  table() %>% 
  barplot()

  # Gr?fico de barras
barplot(Frequency ~ Species, 
        data = tabla,
        xlab = "Species",
        ylab = "Frequency",
        main = "Gr?fico sencillo",
        col = c("dark blue", "dark red", "dark green"))

  # Gr?fico de tortas
iris$Species[iris$Sepal.Length > 5] %>% 
  table() %>% 
  pie()

# ggplot2
#* Permite ir construyendo gr?ficos por capas
#* www.sthda.com/english/wiki

# Gr?fico de tortas
ggplot(data = tabla, aes(x = "", y = Frequency, fill = Species)) +
  geom_bar(stat = "identity",
           # Poner l?neas blancas
           color = "white") +
  # coord_polar(): transformar a un gr?fico de tortas
  coord_polar(theta = "y", start = 0) +
  # scale_fill_brewer(): usar paleta de colores del paquete RColorBrewer
  # scale_fill_manual(): usar colores designados por el usuario
  scale_fill_brewer(palette = "Greens") +
  theme_void()

install.packages("RcolorBrewer")
library(RColorBrewer)
display.brewer.all()

# Gr?fico de barras
ggplot(data = tabla, aes(x = Species, y = Frequency, fill = Species)) +
  geom_bar(stat = "identity",
           # Cambiar el ancho (0.9 el es valor por defecto)
           width = 0.6) +
  # Cambiar la paleta de colores (requiere que se indique fill en aes())
  scale_fill_brewer(palette = "Reds") +
  theme_minimal() +
  # geom_text(): agregar valores al gr?fico
  geom_text(aes(label = Frequency),
            # hjust: corregir la posici?n en el eje horizontal (vjust an?logo en el eje vertical)
            vjust = -0.8, size = 4)

# Otros gr?ficos de barras
fact2 <- readxl::read_excel("Factorial2.xlsx")

  # Estad?stica descriptiva mediante agrupaci?n
resumen <- fact2 %>% 
  group_by(Sexo, Estacion) %>% 
  summarise(mean.conc.Pr = mean(Conc.Prot.),
            sd.conc.Pr = sd(Conc.Prot.))

s <- ggplot(data = resumen, aes(x = Sexo, y = mean.conc.Pr, fill = Estacion)) + 
  theme_minimal()

s + geom_bar(stat = "identity", position = "stack") # Gr?fico de barras apiladas 

s + geom_bar(stat = "identity", position = "dodge") # Barras ordenadas al lado

s + geom_bar(stat = "identity", position = "fill") # Barras apiladas pero en t?rminos porcentuales

# Tablas de frecuencia variables cuantitativas continuas ----
#* Existen varias librerias que sirven para crear tablas de frecuencias
install.packages("agricolae")
library(agricolae)

# range(): entrega los valores min y max de los datos
range(iris$Sepal.Length)

# Grafico histograma de graphics
tmp <- hist(iris$Sepal.Length, 
            plot = F, # Para no graficar sino que entregue la informaci?n
            breaks = c(4,5,5.5,6,6.3,8)) # Modificaci?n de los breaks en el histograma

# A partir del histograma se identifican los rangos a emplear para la tabla de frecuencia
# table.freq(): construcci?n de una tabla de frecuencia
tablafrecuencia <- agricolae::table.freq(tmp)

colnames(tablafrecuencia) <- c("LI", "LS", "MC", "f", "h", "F", "H")

# IMPORTANTE: Los rangos de los intervalos son cerrados a la derecha (contienen el valor del LS)

# Armar nuestra propia tabla de frecuencias para variables cuantitativas continuas
iris %>% 
  pull(Sepal.Length) %>% 
  # cut(): segmentar los datos seg?n intervalos designados. El resultado es un vector que indica el rango en el que la observaci?n se encuentra presente.
  cut(breaks = seq(4, 8, by = 0.5), 
      # Considerar el intervalo cerrado a la derecha y abierto a la izquierda
      right = T) %>% 
  # Calcular la frecuencia de observaciones dentro de los intervalos
  table() %>% 
  # Convertir del formato table a data frame
  as.data.frame() %>% 
  as_tibble() %>% 
  # Agregar las dem?s frecuencias
  mutate(h = round(Freq/sum(Freq)*100, 1),
         # cumsum(): funci?n de suma acumulada (cumulative sum)
         F = cumsum(Freq),
         H = cumsum(h)) %>% 
  rename(f = Freq)

# Tablas de frecuencia variables cuantitativas discretas ----

# Simulaci?n para crear datos discretos
# set.seed(): insertar una semilla para que nuestros datos sean replicables
#* El resultado de un proceso aleatorio ser? el mismo para todos los computadores en donde se realice
set.seed(seed = 1)
#* Los datos cuantitativos discretos los vamos a obtener a trav?s de una distribuci?n de Poisson (variable aleatoria discreta)
# rpois(): selecci?n al azar de 100 datos de una distribuci?n de Poisson
dato <- rpois(n = 150, lambda = 7) # La distribuci?n Poisson genera datos cuantitativos discretos

str(dato)

dato %>% 
  table() %>% 
  as_tibble() %>% 
  rename(Value = ".", f = n) %>% 
  mutate(h = round(f/sum(f)*100, digits = 1),
         F = cumsum(f),
         H = cumsum(h))

# Boxplot con ggplot ----

ggplot(data = iris, aes(x = Species, y = Sepal.Length, color = Species)) +
  # outlier.alpha = 0: Eliminar los outliers de la visualizaci?n
  geom_boxplot(outlier.color = "black") +
  # Agregar la media a los boxplots
  stat_summary(fun = mean, color = "dark grey")

# Cuantiles ----
#* Los cuantiles son los percentiles ya no en formato % si no en decimales
tabla <- iris %>% 
  filter(Species == "versicolor") %>% 
  pull(Sepal.Length) %>% 
  quantile(c(0.25, 0.5, 0.75)) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  mutate(rowname = as.numeric(str_remove(rowname, pattern = "%")),
         Q = c("Q1", "Q2", "Q3")) %>%
  rename(k = rowname, P = ".") %>% 
  relocate(Q, .after = "k")

# Rango intercuart?lico de los datos
RI <- tabla$P[tabla$Q == "Q3"] - tabla$P[tabla$Q == "Q1"]

iris %>% 
  group_by(Species) %>% 
  summarise(Q1 = quantile(Sepal.Length, 0.25),
            Q2 = quantile(Sepal.Length, 0.50),
            Q3 = quantile(Sepal.Length, 0.75),
            RI = quantile(Sepal.Length, 0.75) - quantile(Sepal.Length, 0.25))