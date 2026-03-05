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




salud <- read_csv("dataset_categorical_NA.csv", show_col_types = FALSE) #aqui no pude obtener el mismpo directorio de datos y tuve que cargarlo de manera manual
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


#MUTATE

# R BASE: crear columna nueva en salud
salud$riesgo_cardio <- (salud$BloodPressure + salud$Cholesterol) / 2

# DPLYR: mismo resultado, sin modificar el objeto original
salud |>
  mutate(riesgo_cardio = (BloodPressure + Cholesterol) / 2) |>
  select(ID, BloodPressure, Cholesterol, riesgo_cardio) |>
  head(5)

# Guardamos el dataset de vinos con columnas nuevas
vinos <- vinos |>
  mutate(
    calidad_alta   = ifelse(quality >= 7, "Sí", "No"),
    alcohol_c      = alcohol - mean(alcohol, na.rm = TRUE),
    proporcion_so2 = `free sulfur dioxide` / `total sulfur dioxide`
  )

vinos |>
  select(tipo, quality, calidad_alta, alcohol, alcohol_c, proporcion_so2) |>
  head(5)

# Guardamos el dataset de salud con columnas derivadas
salud <- salud |>
  mutate(
    riesgo_cardio = round((BloodPressure + Cholesterol) / 2, 1),
    ratio_cc      = round(WaistCircumference / HipCircumference, 3)
  )

salud |>
  select(ID, BloodPressure, Cholesterol, riesgo_cardio, ratio_cc) |>
  arrange(desc(riesgo_cardio)) |>
  head(6)

vinos <- vinos |>
  mutate(azucar_alta = ifelse(`residual sugar` > 10, "Sí", "No"))

vinos |>
  group_by(tipo, azucar_alta) |>
  summarise(n = n(), .groups = "drop")

salud <- salud |>
  mutate(obesidad = ifelse(BMI >= 30, "Sí", "No"))

count(salud, SmokingStatus, obesidad)

# Clasificamos la calidad de los vinos en tres niveles
vinos <- vinos |>
  mutate(
    categoria = case_when(
      quality <= 4 ~ "Baja",
      quality <= 6 ~ "Media",
      quality <= 8 ~ "Alta",
      TRUE         ~ "Excepcional"   # TRUE actúa como "else"
    )
  )

vinos |>
  count(tipo, categoria) |>
  arrange(tipo, categoria)

# Clasificación de IMC en salud — con manejo explícito de NA
salud <- salud |>
  mutate(
    categoria_imc = case_when(
      is.na(BMI)   ~ "Sin dato",    # ← siempre primero los NA
      BMI < 18.5   ~ "Bajo peso",
      BMI < 25     ~ "Normal",
      BMI < 30     ~ "Sobrepeso",
      TRUE         ~ "Obesidad"
    )
  )

count(salud, categoria_imc, sort = TRUE)

# Nivel de estrés con manejo de NA
salud <- salud |>
  mutate(
    nivel_estres = case_when(
      is.na(StressLevel) ~ "Sin dato",
      StressLevel < 30   ~ "Bajo",
      StressLevel < 60   ~ "Moderado",
      StressLevel < 80   ~ "Alto",
      TRUE               ~ "Muy alto"
    )
  )

count(salud, nivel_estres) |> arrange(nivel_estres)

#1. Crea una nueva columna en vinos que se llame nivel_alcohol que clasifique así: - Menos de 10%: "Bajo" · Entre 10% y 12%: "Medio" · Más de 12%: "Alto"
#Pista: usa case_when().

vinos |>
  mutate(nivel_alcohol = case_when(alcohol < 10  ~ "Bajo",
  alcohol <= 12 ~ "Medio",
  TRUE ~ "Alto")) |>
  count(tipo, nivel_alcohol)

#2. En salud, crea una columna grupo_edad que clasifique a los participantes en:
#  - Menos de 30: "Joven" · Entre 30–50: "Adulto" · Más de 50: "Mayor"
# Recuerda manejar los NA como primera condición.

salud |>
  mutate(grupo_edad = case_when(is.na(Age) ~ "Sin dato",
      Age < 30   ~ "Joven",
      Age <= 50  ~ "Adulto",
      TRUE       ~ "Mayor")) |>
  count(grupo_edad)

#3. Crea una columna fumador_activo en salud que sea TRUE si el participante fuma actualmente (SmokingStatus == "Fuma"), FALSE en caso contrario.

salud |>
  mutate(fumador_activo = SmokingStatus == "Fuma")|>
  count(fumador_activo)

#7. SUMMARISE

# R BASE: calcular varios estadísticos del alcohol en vinos
c(
  media   = mean(vinos$alcohol),
  mediana = median(vinos$alcohol),
  sd      = sd(vinos$alcohol),
  n       = nrow(vinos)
)

# DPLYR: mismo resultado, más organizado y extendible
vinos |>
  summarise(
    n               = n(),
    media_alcohol   = round(mean(alcohol), 2),
    mediana_alcohol = median(alcohol),
    sd_alcohol      = round(sd(alcohol), 2),
    min_calidad     = min(quality),
    max_calidad     = max(quality)
  )

# Resumen del dataset de salud — con na.rm = TRUE para manejar los NA
salud |>
  summarise(
    n              = n(),
    media_imc      = round(mean(BMI, na.rm = TRUE), 1),
    mediana_imc    = round(median(BMI, na.rm = TRUE), 1),
    media_edad     = round(mean(Age, na.rm = TRUE), 1),
    max_colesterol = round(max(Cholesterol, na.rm = TRUE), 1),
    n_sin_edad     = sum(is.na(Age))          # contar NA explícitamente
  )

#8. GROUP_BY + SUMMARISE

# R BASE: calcular el promedio de calidad por tipo de vino
tapply(vinos$quality, vinos$tipo, mean)

# DPLYR: más claro y fácilmente extendible a más estadísticos
vinos |>
  group_by(tipo) |>
  summarise(
    n             = n(),
    calidad_media = round(mean(quality), 2),
    alcohol_medio = round(mean(alcohol), 2),
    ph_medio      = round(mean(pH), 2),
    .groups       = "drop"    # importante: siempre añadir para limpiar los grupos
  )

# Distribución de calidades por tipo de vino
vinos |>
  group_by(tipo, quality) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(tipo, quality)

# Indicadores clínicos por estatus de tabaquismo y tipo de residencia
salud |>
  filter(!is.na(SmokingStatus), !is.na(ResidenceType)) |>
  group_by(SmokingStatus, ResidenceType) |>
  summarise(
    n             = n(),
    media_imc     = round(mean(BMI, na.rm = TRUE), 1),
    media_colest  = round(mean(Cholesterol, na.rm = TRUE), 1),
    .groups       = "drop"
  ) |>
  arrange(SmokingStatus, ResidenceType)

salud |>
  filter(!is.na(SmokingStatus)) |>
  group_by(SmokingStatus) |>
  summarise(
    n                    = n(),
    media_presion        = round(mean(BloodPressure, na.rm = TRUE), 1),
    media_colesterol     = round(mean(Cholesterol, na.rm = TRUE), 1),
    media_riesgo_cardio  = round(mean(riesgo_cardio, na.rm = TRUE), 1),
    pct_obesidad         = round(mean(BMI >= 30, na.rm = TRUE) * 100, 1),
    .groups              = "drop"
  ) |>
  arrange(desc(media_riesgo_cardio))

# Estas dos instrucciones producen el mismo resultado en vinos:
vinos |>
  group_by(tipo, categoria) |>
  summarise(n = n(), .groups = "drop")

# Atajo con count():
vinos |>
  count(tipo, categoria)

# Conteo en salud: participantes por nivel educativo
salud |>
  count(EducationLevel, sort = TRUE)

#EJERCICIOS GROUP:BY + SUMMARISE
#1. Calcula el promedio de alcohol, pH y calidad para cada combinación de tipo y categoria de vino.

vinos |>
  group_by(tipo, categoria) |>
  summarise(n = n(),
    media_alcohol = round(mean(alcohol), 2),
    media_ph = round(mean(pH), 2),
    media_quality = round(mean(quality), 2),
    .groups = "drop") #EN ESTE EJERCICIO NO ENTENDÍA POR QUÉ NO ME SALÍA HASTA QUE VI QUE LLEVABA PUNTO EN .GROUPS = DROP

#2. ¿Qué estado civil del dataset salud tiene el nivel de estrés más alto en promedio? Muestra el top 3.

salud |>
  filter(!is.na(MaritalStatus)) |>
  group_by(MaritalStatus) |>
  summarise(media_estres = round(mean(StressLevel, na.rm = TRUE), 1), .groups = "drop") |>
  slice_max(media_estres, n = 3)

#3. Calcula el promedio de glucosa y colesterol por estatus de tabaquismo en salud.

salud |>
  filter(!is.na(SmokingStatus)) |>
  group_by(SmokingStatus) |>
  summarise(media_glucosa = round(mean(Glucose, na.rm = TRUE), 1),
  media_colesterol = round(mean(Cholesterol, na.rm = TRUE), 1),
  .groups = "drop")

#4. Desafío: ¿En qué combinación de EducationLevel y ResidenceType se observa el mayor BMI promedio en salud? Muestra el top 5.

salud |>
  group_by(EducationLevel, ResidenceType) |>
  summarise(media_imc = round(mean(BMI, na.rm = TRUE), 1), n = n(), .groups = "drop") |>
  filter(!is.na(EducationLevel), !is.na(ResidenceType))|>
  slice_max(media_imc, n = 5)

#9. COMBINAR DOS TABLAS

# Datos de ejemplo pequeños para entender los joins
clientes <- tibble(
  id     = c(1, 2, 3, 4),
  nombre = c("Ana", "Beto", "Carla", "David")
)

pedidos <- tibble(
  id      = c(2, 3, 4, 5),
  producto = c("Libro", "Lámpara", "Silla", "Mesa")
)

clientes

pedidos

# Solo aparecen los IDs que existen en AMBAS tablas (2, 3, 4)
inner_join(clientes, pedidos, by = "id")

# TODOS los clientes aparecen. Ana aparece con NA en producto
left_join(clientes, pedidos, by = "id")

right_join(clientes, pedidos, by = "id")

full_join(clientes, pedidos, by = "id")

# ¿Qué clientes NO tienen pedido?
anti_join(clientes, pedidos, by = "id")

semi_join(clientes, pedidos, by = "id")

# Tabla 1: registros individuales
detalle_individual <- salud |>
  select(ID, SmokingStatus, ResidenceType, Age, BMI, Cholesterol)

# Tabla 2: resumen de indicadores por tipo de residencia
resumen_zona <- salud |>
  group_by(ResidenceType) |>
  summarise(
    imc_promedio_zona        = round(mean(BMI, na.rm = TRUE), 1),
    colesterol_promedio_zona = round(mean(Cholesterol, na.rm = TRUE), 1),
    n_en_zona                = n(),
    .groups                  = "drop"
  )

resumen_zona

# Unimos: para cada participante, agregamos el contexto de su zona
detalle_con_contexto <- detalle_individual |>
  left_join(resumen_zona, by = "ResidenceType")

# Ahora podemos calcular cuánto se aleja cada participante del promedio de su zona
detalle_con_contexto |>
  mutate(diferencia_imc = round(BMI - imc_promedio_zona, 2)) |>
  filter(!is.na(diferencia_imc)) |>
  select(ID, ResidenceType, BMI, imc_promedio_zona, diferencia_imc) |>
  arrange(desc(diferencia_imc)) |>
  head(8)

# anti_join: ¿hay participantes sin zona de residencia registrada?
sin_zona <- salud |> filter(is.na(ResidenceType)) |> select(ID)
anti_join(salud |> select(ID, ResidenceType), resumen_zona, by = "ResidenceType") |> head(5)

#EJERCICIOS JOINS

#1. Construye una tabla resumen_educacion con el BMI promedio y nivel de estrés promedio por EducationLevel. Luego usa left_join() para agregarla al dataset salud. ¿Cuántas filas tiene el resultado?
  
resumen_educacion <- salud |>
  filter(!is.na(EducationLevel)) |>
  group_by(EducationLevel) |>
  summarise(imc_prom_educ = round(mean(BMI, na.rm = TRUE), 1),
    estres_prom_educ = round(mean(StressLevel, na.rm = TRUE), 1),
    .groups = "drop")
resultado_join <- salud |>
  left_join(resumen_educacion, by = "EducationLevel")
nrow(resultado_join) #20

resultado_join |> select(ID, EducationLevel, BMI, imc_prom_educ) |> head(5)

#2. ¿Existen niveles educativos en resumen_educacion que no aparezcan en salud? Usa anti_join() para verificarlo.

anti_join(resumen_educacion, salud, by = "EducationLevel")

#3. Usando left_join(), combina salud con resumen_zona y luego muestra el BMI promedio por ResidenceType, comparándolo con el imc_promedio_zona calculado en el resumen.

salud |>
  left_join(resumen_zona, by = "ResidenceType") |>
  group_by(ResidenceType) |>
  summarise(media_imc_real = round(mean(BMI, na.rm = TRUE), 1),
    imc_zona = first(imc_promedio_zona),
    .groups = "drop")

#10. FUNCIONES DE VENTANA

# Top 5 vinos tintos de mayor calidad
vinos |>
  filter(tipo == "tinto") |>
  mutate(ranking = row_number(desc(quality))) |>
  filter(ranking <= 5) |>
  select(tipo, quality, alcohol, ranking) |>
  arrange(ranking)

# Top 3 IMC más alto por grupo de tabaquismo
salud |>
  filter(!is.na(SmokingStatus), !is.na(BMI)) |>
  group_by(SmokingStatus) |>
  mutate(ranking_imc = dense_rank(desc(BMI))) |>
  filter(ranking_imc <= 3) |>
  select(ID, SmokingStatus, BMI, ranking_imc) |>
  arrange(SmokingStatus, ranking_imc)

# Para cada vino: ¿cuánto se aleja su calidad del promedio de su tipo?
vinos |>
  group_by(tipo) |>
  mutate(
    promedio_tipo = mean(quality),
    diferencia    = round(quality - promedio_tipo, 2)
  ) |>
  ungroup() |>
  select(tipo, quality, promedio_tipo, diferencia) |>
  filter(abs(diferencia) > 1.5) |>
  arrange(desc(diferencia)) |>
  head(10)

# Para cada participante: ¿cuánto se aleja su colesterol del promedio de su grupo educativo?
salud |>
  filter(!is.na(EducationLevel)) |>
  group_by(EducationLevel) |>
  mutate(
    promedio_educacion = round(mean(Cholesterol, na.rm = TRUE), 1),
    diferencia         = round(Cholesterol - promedio_educacion, 1)
  ) |>
  ungroup() |>
  filter(diferencia > 30) |>
  select(ID, EducationLevel, Cholesterol, promedio_educacion, diferencia) |>
  arrange(desc(diferencia)) |>
  head(8)

# Diferencia de BMI entre participantes consecutivos (ordenados por edad)
salud |>
  filter(!is.na(Age), !is.na(BMI)) |>
  arrange(Age) |>
  mutate(
    diferencia_imc_anterior = round(BMI - lag(BMI), 2)
  ) |>
  select(ID, Age, BMI, diferencia_imc_anterior) |>
  head(10)

#11. VERBOS ADICIONALES ÚTILES

vinos |>
  rename(calidad = quality, tipo_vino = tipo) |>
  select(tipo_vino, calidad, alcohol) |>
  head(4)

salud |>
  rename(imc = BMI, tabaquismo = SmokingStatus) |>
  select(ID, imc, tabaquismo) |>
  head(4)

# ¿Cuántas calidades distintas hay en vinos?
vinos |>
  distinct(quality) |>
  arrange(quality)

# Valores únicos de estatus de tabaquismo en salud
salud |>
  distinct(SmokingStatus) |>
  arrange(SmokingStatus)

# Los 3 vinos con mayor calidad de cada tipo
vinos |>
  group_by(tipo) |>
  slice_max(quality, n = 3) |>
  select(tipo, quality, alcohol) |>
  arrange(tipo, desc(quality))

# El participante con mayor BMI por tipo de residencia
salud |>
  filter(!is.na(ResidenceType), !is.na(BMI)) |>
  group_by(ResidenceType) |>
  slice_max(BMI, n = 1) |>
  select(ID, ResidenceType, BMI, Age, SmokingStatus)

# Media de varias columnas numéricas de vinos por tipo
vinos |>
  group_by(tipo) |>
  summarise(
    across(c(quality, alcohol, pH), ~ round(mean(.x), 2)),
    .groups = "drop"
  )

# Media de TODAS las columnas numéricas de salud por estatus de tabaquismo
salud |>
  filter(!is.na(SmokingStatus)) |>
  group_by(SmokingStatus) |>
  summarise(
    across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 1)),
    .groups = "drop"
  ) |>
  select(SmokingStatus, Age, BMI, Cholesterol, BloodPressure, Glucose)

#12. CASO INTEGRADOR: ANÁLISIS COMPLETO CON AMBAS FUENTES

# Promedio de alcohol por nivel de calidad y tipo
vinos |>
  group_by(tipo, quality) |>
  summarise(
    n             = n(),
    alcohol_medio = round(mean(alcohol), 2),
    ph_medio      = round(mean(pH), 2),
    .groups       = "drop"
  ) |>
  arrange(tipo, quality)

# Visualizamos la relación
vinos |>
  group_by(tipo, quality) |>
  summarise(alcohol_medio = mean(alcohol), .groups = "drop") |>
  ggplot(aes(x = quality, y = alcohol_medio, color = tipo, group = tipo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("tinto" = "#8B1A1A", "blanco" = "#D4A820")) +
  labs(
    title = "¿El alcohol distingue a los vinos de alta calidad?",
    x     = "Calidad (puntuación del catador)",
    y     = "Alcohol promedio (%)",
    color = "Tipo de vino"
  ) +
  theme_minimal()

#Pregunta 2: ¿Cómo se distribuye el IMC por tabaquismo y nivel educativo en el dataset de salud?


salud |>
  filter(!is.na(SmokingStatus), !is.na(EducationLevel), !is.na(BMI)) |>
  group_by(SmokingStatus, EducationLevel) |>
  summarise(
    media_imc = round(mean(BMI, na.rm = TRUE), 1),
    n         = n(),
    .groups   = "drop"
  ) |>
  arrange(SmokingStatus, desc(media_imc))

# Visualizamos el perfil por educación y tabaquismo
salud |>
  filter(!is.na(SmokingStatus), !is.na(EducationLevel), !is.na(BMI)) |>
  group_by(SmokingStatus, EducationLevel) |>
  summarise(media_imc = round(mean(BMI, na.rm = TRUE), 1), .groups = "drop") |>
  ggplot(aes(x = EducationLevel, y = media_imc, fill = SmokingStatus)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(
    values = c("Fuma" = "#E03131", "No fuma" = "#2F9E44", "Ex-fumadora" = "#F08C00")
  ) +
  labs(
    title = "IMC promedio por nivel educativo y estatus de tabaquismo",
    x     = "Nivel educativo",
    y     = "IMC promedio",
    fill  = "Tabaquismo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#EJERCICIO INTEGRADOR FINAL USANDO DATASETS DE VINOS Y SALUD

#A) ¿Cuál es el promedio de calidad de los vinos blancos de categoría “Alta” o “Excepcional”, agrupado por nivel de alcohol (nivel_alcohol)?
#Pista: primero crea la columna nivel_alcohol con mutate() y case_when().

vinos |>
  filter(tipo == "blanco", categoria %in% c("Alta", "Excepcional")) |>
  mutate(nivel_alcohol = case_when(alcohol < 10  ~ "Bajo", alcohol <= 12 ~ "Medio", TRUE ~ "Alto")) |>
  group_by(nivel_alcohol) |>
  summarise(n = n(), calidad_media = round(mean(quality), 2), .groups       = "drop") |>
  arrange(desc(calidad_media))


#B) En el dataset salud, ¿qué 5 participantes tienen la mayor diferencia entre su BMI y el BMI promedio de su grupo educativo? Muestra su ID, nivel educativo, BMI individual y la diferencia.

salud |>
  filter(!is.na(EducationLevel), !is.na(BMI)) |>
  group_by(EducationLevel) |>
  mutate(imc_prom_educ  = round(mean(BMI, na.rm = TRUE), 1), diferencia_imc = round(abs(BMI - imc_prom_educ), 2)) |>
  ungroup() |>
  slice_max(diferencia_imc, n = 5) |>
  select(ID, EducationLevel, BMI, imc_prom_educ, diferencia_imc)















