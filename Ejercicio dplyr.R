# Cargamos lo que usaremos en todo el tutorial
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

url_tinto  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
url_blanco <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
#Esto es para cargar directamente los csv desde los servidores web, no lo descarga al ordenador actual, se coloca como
#objeto interno de R
vino_tinto  <- read_delim(url_tinto,  delim = ";", show_col_types = FALSE)
vino_blanco <- read_delim(url_blanco, delim = ";", show_col_types = FALSE)

# Añadimos columna "tipo" para identificarlos al unirlos
vino_tinto  <- mutate(vino_tinto,  tipo = "tinto")
vino_blanco <- mutate(vino_blanco, tipo = "blanco")

# bind_rows() apila las dos tablas (como pegar una encima de la otra)
vinos <- bind_rows(vino_tinto, vino_blanco)

glimpse(vinos)




salud <- read_csv("01_RawData/dataset_categorical_NA.csv", show_col_types = FALSE)
#En este caso es con una base de datos interna
glimpse(salud)


#Pipe Ctls shift M %>% 

  
  # Con vinos: filtrar, seleccionar y ordenar en una sola cadena
vinos |>
filter(tipo == "tinto") |>
filter(quality >= 7) |>
select(tipo, quality, alcohol, pH) |>
arrange(desc(alcohol)) |>
head(8)


# Con salud: participantes que fuman, seleccionando variables clave
salud |>
  filter(SmokingStatus == "Fuma") |>
  select(ID, Age, BMI, Cholesterol, SmokingStatus) |>
  head(8)

# R BASE: vinos con quality == 9
vinos[vinos$quality == 9, ]

# DPLYR: mismo resultado, más claro
filter(vinos, quality == 9)
#== es para comparar 

# Quedarnos solo con vinos tintos
filter(vinos, tipo == "tinto")

# Participantes con BMI mayor a 30 (obesidad)
filter(salud, BMI > 30)

# Participantes con colesterol mayor a 30 
filter(salud, Cholesterol > 30)

# Vinos tintos Y de calidad alta (los dos al mismo tiempo)
filter(vinos, tipo == "tinto" & quality >= 8)
filter(vinos, tipo == "tinto" & quality == 10)
filter(vinos, tipo == "tinto" & quality == 9)
vinos |>filter(quality==9) #esta es la forma de escribir el código para que pueda correrlo

# Participantes que fuman Y tienen BMI mayor a 25
filter(salud, SmokingStatus == "Fuma", BMI > 25)

salud |> filter(SmokingStatus == "Fuma", BMI> 25, BloodPressure > 25)

# Vinos que tengan quality == 9 O alcohol > 14
filter(vinos, quality == 9 | alcohol > 14) |>
  select(tipo, quality, alcohol) |>
  head(8) #para cuando debe cumplir una u otra condición

# Vinos de calidad 8 o 9
filter(vinos, quality %in% c(8, 9)) |>
  select(tipo, quality, alcohol) |>
  head(8) #filtrar varios valores en la misma columna, seleccionar elementos en un conjunto

# Participantes de zona urbana o suburbana
filter(salud, ResidenceType %in% c("Urbano", "Suburbano")) |>
  count(ResidenceType) #Tambien se puede usar con el and pero se tendría que escribir más
 #count es para valores numéricos, es decir, cuenta cuántos hay en las categorias nominales

# Vinos con pH entre 3.0 y 3.1 (incluyendo los extremos)
filter(vinos, between(pH, 3.0, 3.1)) |>
  select(tipo, pH, quality) |>
  head(6) #head es para regresar los primeros datos y poder verificar

filter(vinos, between(pH, 2.9, 3.5)) |> #ejemplo con cambio de datos
  select(tipo, pH, quality) |>
  head(6)

# Participantes en edad laboral (25 a 55 años)
filter(salud, between(Age, 25, 55)) |>
  select(ID, Age, EmploymentStatus) |>
  head(8)

filter(salud, between(Age, 25, 45)) |> #ejemplo con cambio de datos
  select(ID, Age, EmploymentStatus) |>
  head(5)

# Participantes sin dato de edad O menores de 30
filter(salud, is.na(Age) | Age < 30) |>
  select(ID, Age, SmokingStatus)

filter(salud, is.na(Age) | Age < 30) |>
  select(ID, Age, SmokingStatus, EmploymentStatus) #ejemplo con cambio de datos

# Vinos tintos de calidad alta, ordenados por alcohol
vinos |>
  filter(tipo == "tinto") |>
  filter(quality >= 7) |>
  select(tipo, quality, alcohol, pH) |>
  arrange(desc(alcohol)) |>
  head(8)

# Participantes urbanos, con empleo de tiempo completo y estrés alto
salud |>
  filter(ResidenceType == "Urbano") |>
  filter(EmploymentStatus == "Tiempo completo") |>
  filter(StressLevel > 60) |>
  select(ID, Age, BMI, SmokingStatus, StressLevel) |>
  head(10)

# Participantes urbanos, con empleo de tiempo completo y estrés alto
varios <- salud |>
  filter(ResidenceType == "Urbano") |>
  filter(StressLevel > 1) |>
  select(ID, Age, BMI, SmokingStatus, StressLevel) |>
  head(10)

print (varios)

#ID      Age   BMI SmokingStatus StressLevel
#<chr> <dbl> <dbl> <chr>               <dbl>
#  1 id3      18  26.6 Ex-fumadora          51.5
#2 id4      44  75.1 Fuma                 45.0
#3 id5      39  35.7 Ex-fumadora          92.3
#4 id9      56  64.2 No fuma              94.8
#5 id13     51  56.9 Fuma                 20.8
#6 id16     25  69.1 No fuma              29.7
#7 id17     23  70.9 Fuma                 42.9
#8 id19     NA  99.0 NA                   26.7

#EJERCICIOS
#1. ¿Cuántos vinos blancos tienen una calidad de 5 o menos?
filter(vino_blanco) |> quality >=5 #primer intento

vinos |> filter(tipo=="blanco", quality <=5) #respuesta

#2. Filtra los vinos tintos que tengan alcohol mayor al promedio de todos los vinos.

vinos |> filter(tipo == "tinto", alcohol > mean(vinos$alcohol)) |> 
head(10) #lo agregué porque me aparecía en la consola que eran aprox 2000 valores

#3. ¿Cuántos participantes del dataset salud son ex-fumadores y residen en zona urbana?

salud |> filter(SmokingStatus == "Ex-fumadora", ResidenceType== "Urbano") |> 
  count()
#4. Filtra los participantes con nivel de estrés mayor a 80. ¿Qué estatus de tabaquismo predomina entre ellos?

salud |>
  filter(StressLevel > 80) |>
  count(SmokingStatus, sort = TRUE)

# R BASE: seleccionar columnas por nombre (con comillas y c())
vinos[, c("tipo", "quality", "alcohol")]

# DPLYR: más limpio, sin comillas, sin c()
select(vinos, tipo, quality, alcohol)

# Vinos: tres columnas clave
vinos |>
  select(tipo, quality, alcohol) |>
  head(5)

# Salud: perfil básico de cada participante
salud |>
  select(ID, Age, BMI, SmokingStatus, ResidenceType) |>
  head(5)

# Quitamos las columnas de dióxido de azufre
vinos |>
  select(-`free sulfur dioxide`, -`total sulfur dioxide`) |>
  names()

# Quitamos las medidas corporales directas del dataset de salud
salud |>
  select(-Weight, -Height, -WaistCircumference, -HipCircumference) |>
  names()

vinos |>
  select(
    tipo,
    calidad        = quality,
    alcohol,
    acidez_volatil = `volatile acidity`
  ) |>
  head(4)

salud |>
  select(
    participante = ID,
    edad         = Age,
    imc          = BMI,
    tabaquismo   = SmokingStatus,
    residencia   = ResidenceType
  ) |>
  head(4)

# Columnas cuyo nombre empieza con "total" (vinos)
vinos |>
  select(tipo, quality, starts_with("total")) |>
  head(4)

# Columnas que contienen "Circumference" (salud)
salud |>
  select(ID, contains("Circumference")) |>
  head(4)

# Solo columnas numéricas (salud)
salud |>
  select(where(is.numeric)) |>
  head(3)

# Solo columnas categóricas (salud) — muy útil para explorar variables de texto
salud |>
  select(where(is.character)) |>
  head(3)

# Vinos de calidad alta: solo las columnas relevantes
vinos |>
  filter(quality >= 8) |>
  select(tipo, quality, alcohol, pH) |>
  head(10)

# Participantes que fuman: perfil clínico básico
salud |>
  filter(SmokingStatus == "Fuma") |>
  select(ID, Age, BMI, Cholesterol, BloodPressure) |>
  head(8)


# 1. Del dataset vinos, selecciona solo las columnas de texto (tipo character).

vinos |> select(where(is.character)) |> 
  head(10) #en este caso solo aparece tinto porque es la columna character ordenada, para tail aparecerían los blancos

# 2. Del dataset vinos, selecciona todas las columnas cuyo nombre contenga la palabra "acid".
vinos |> select(contains("acid"))

#3. Del dataset salud, selecciona ID, Age y todas las columnas que contienen medidas de circunferencia

salud |> select(ID, Age, contains("Circun"), Cintura=WaistCircumference)

# 4. Filtra solo los participantes del dataset salud que residan en zona rural y luego selecciona únicamente ID, Age, BMI y SmokingStatus.

salud |> filter(ResidenceType== "Rural") |> 
  select(ID, Age, BMI, SmokingStatus) |> 
  head(5)

# DPLYR: mismo resultado, mucho más claro
vinos |>
  select(tipo, quality, alcohol) |>
  arrange(desc(alcohol)) |>
  head(5)

# Vinos con menor alcohol primero
vinos |>
  select(tipo, quality, alcohol) |>
  arrange(alcohol) |>
  head(6)

# Participantes más jóvenes primero (NA van al final automáticamente)
salud |>
  select(ID, Age, BMI, SmokingStatus) |>
  arrange(Age) |>
  head(6)

# Vinos con mayor calidad primero
vinos |>
  select(tipo, quality, alcohol) |>
  arrange(desc(quality)) |>
  head(6)

# Participantes con mayor colesterol
salud |>
  select(ID, Age, Cholesterol, SmokingStatus, EducationLevel) |>
  arrange(desc(Cholesterol)) |>
  head(8)

# Primero ordena por quality (desc), en empate ordena por alcohol (desc)
vinos |>
  select(tipo, quality, alcohol, pH) |>
  arrange(desc(quality), desc(alcohol)) |>
  head(10)

# Primero por estatus de tabaquismo (A-Z), dentro de cada grupo por BMI (desc)
salud |>
  filter(!is.na(SmokingStatus)) |>
  select(ID, SmokingStatus, BMI, Age) |>
  arrange(SmokingStatus, desc(BMI)) |>
  head(10)

#1. ¿Cuáles son los 10 vinos con mayor pH? Muestra también su tipo y calidad.

vinos |>
  select(pH, tipo, quality) |>
  arrange(desc(pH)) |>
  head(10)

# 2. ¿Qué participante del dataset salud tiene el nivel de glucosa más alto? Muestra el top 5 con su ID, edad y estatus de tabaquismo.

salud |> arrange(desc(Glucose)) |> 
  select(ID, Age, SmokingStatus) |> 
head(5)

# 3. Ordena los vinos blancos primero por calidad descendente y luego por acidez volátil (volatile acidity) ascendente. Muestra las primeras 10 filas.

vinos |> filter(tipo=="blanco") |> 
  arrange(desc(quality), `volatile acidity`) |> 
  head(10)

usethis::use_github()
