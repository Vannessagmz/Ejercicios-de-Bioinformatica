#Genera dos matrices aleatorias de tamaño 3 × 3 y luego suma ambas matrices.
y1 <- matrix(sample(1:100,9), nrow=3, ncol=3) 
y2 <- matrix(sample(1:100,9), nrow=3, ncol=3)

print(y1)

#      [,1] [,2] [,3]
#[1,]   71   18   42
#[2,]   33    9   45
#[3,]   72   36    8

print(y2)

#      [,1] [,2] [,3]
#[1,]   72   31   83
#[2,]   43   56   16
#[3,]   38   14    9

suma <- (y1+y2)

print(suma)

#     [,1] [,2] [,3]
#[1,]  143   49  125
#[2,]   76   65   61
#[3,]  110   50   17

#Crea dos matrices aleatorias, una de tamaño  2 × 3 y otra de tamaño   3 × 4. Luego, calcula su producto matricial.

matrizA <- matrix(sample(1:20, 6), nrow = 2, ncol = 3)
matrizB <- matrix(sample(1:20, 12), nrow = 3, ncol = 4)

print(matrizA)

#     [,1] [,2] [,3]
#[1,]    2   15   11
#[2,]   10    6   13

print(matrizB)

#      [,1] [,2] [,3] [,4]
#[1,]   16    2    5   11
#[2,]    8   10   15   19
#[3,]    3    9   14    1

producto <- matrizA %*% matrizB

print(producto)

#     [,1] [,2] [,3] [,4]
#[1,]  185  253  389  318
#[2,]  247  197  322  237

#Crea una matriz aleatoria de tamaño 4 × 3 y encuentra su matriz transpuesta.

matriz12 <- matrix(sample(1:50, 12), nrow = 4, ncol = 3)

print(matriz12)

#     [,1] [,2] [,3]
#[1,]   48    8   11
#[2,]   36   50   16
#[3,]    5   39   35
#[4,]   13    7   12

transpuesta <- t(matriz12)

print(transpuesta)

#     [,1] [,2] [,3] [,4]
#[1,]   48   36    5   13
#[2,]    8   50   39    7
#[3,]   11   16   35   12

#Genera una matriz aleatoria de tamaño  5 × 5 y extrae el tercer renglón y la segunda columna.

mat <- matrix(sample(1:50, 25), nrow = 5, ncol = 5)

print(mat)

#     [,1] [,2] [,3] [,4] [,5]
#[1,]    3   33   40   24   29
#[2,]   27   12   46   42   41
#[3,]   30   50    5   39   31
#[4,]   14   19   38    7    6
#[5,]    4   26   15   20   34

renglon3 <- mat[3, ]

print(renglon3)

#[1] 30 50  5 39 31

columna2 <- mat[ , 2]

print(columna2)

#[1] 33 12 50 19 26

#Comparación de Expresión Génica entre Condiciones

#Descripción: Supongamos que tienes una matriz de expresión génica con 6 genes y 4 condiciones experimentales.
#Crea una matriz llamada expresion_genica con 6 genes y 4 condiciones (rellena con datos aleatorios).

expresion_genica <- matrix(runif(24, 0, 100), nrow = 6, ncol = 4)

#Asigna nombres de genes a las filas y nombres de condiciones a las columnas.

rownames(expresion_genica) <- c("Gen1", "Gen2", "Gen3", "Gen4", "Gen5", "Gen6")
colnames(expresion_genica) <- c("Control", "Tratamiento1", "Tratamiento2", "Tratamiento3")

print(expresion_genica)

#       Control Tratamiento1 Tratamiento2 Tratamiento3
#Gen1  98.37906     66.40792     5.222978     90.60218
#Gen2  67.66779     43.70660     2.657450     28.70696
#Gen3  20.81695     72.45350    10.797637     29.29597
#Gen4  79.00493     95.41675    55.753028     53.50332
#Gen5  91.19212     69.25131    52.534909     41.42472
#Gen6  28.45353     17.89342     9.169008     36.27759

#Calcula el promedio de expresión génica para cada gen.

promedio_gen <-rowMeans(expresion_genica)

print(promedio_gen)

#  Gen1     Gen2     Gen3     Gen4     Gen5     Gen6 
# 65.15304 35.68470 33.34101 70.91951 63.60076 22.94839 


#Datos de crecimiento de cultivos bacterianos
#Descripción: Supón que tienes un data.frame con datos de crecimiento de cultivos bacterianos en diferentes condiciones.

#Crea un data.frame crecimiento con las siguientes columnas: Cepa, Medio, TasaCrecimiento, Temperatura.
Cepa <- paste0("Cepa", 1:10)

print(Cepa)
# [1] "Cepa1"  "Cepa2"  "Cepa3"  "Cepa4"  "Cepa5"  "Cepa6"  "Cepa7"  "Cepa8"  "Cepa9"  "Cepa10"

Medio <- sample(c("LB", "M9","TSB"),10, replace = TRUE) #tuve que modificar todo el comando porque estaba usando paste0

print(Medio)
# "LB"  "M9"  "LB"  "TSB" "M9"  "M9"  "LB"  "TSB" "TSB" "TSB"

Temperatura <- sample(c(25, 30, 37, 42), 10, replace = TRUE) #tampoco mesale :c, faltaba agregar el c antes de los números de la temperatura

print(Temperatura)
# 37 42 37 30 37 37 30 25 42 42

TasaCrecimiento <- runif(10, 0.1, 1.0)

print(TasaCrecimiento)

# 0.5659694 0.9891647 0.9602785 0.5962677 0.1606417 0.7103574 0.5027153 0.2762252 0.8857885 0.5209321

#Llena el data.frame con datos aleatorios para 10 cepas en 3 medios diferentes y 4 temperaturas distintas.

crecimiento <- data.frame(Cepa, Medio, TasaCrecimiento, Temperatura) #lo había intentado con replace=true y no me salía, lo tuve que quitar y al final sí lo corrió

print(crecimiento)

#     Cepa    Medio    TasaCrecimiento  Temperatura
# 1   Cepa1    LB       0.5659694          37
# 2   Cepa2    M9       0.9891647          42
# 3   Cepa3    LB       0.9602785          37
# 4   Cepa4   TSB       0.5962677          30
# 5   Cepa5    M9       0.1606417          37
# 6   Cepa6    M9       0.7103574          37
# 7   Cepa7    LB       0.5027153          30
# 8   Cepa8   TSB       0.2762252          25
# 9   Cepa9   TSB       0.8857885          42
# 10 Cepa10   TSB       0.5209321          42

#Encuentra la media y la desviación estándar de la tasa de crecimiento por cada medio.

med_med <- aggregate(TasaCrecimiento ~ Medio, data = crecimiento, mean)

print(med_med)

#   Medio TasaCrecimiento
# 1    LB       0.6763210
# 2    M9       0.6200546
# 3   TSB       0.5698034

sd_med <- aggregate(TasaCrecimiento ~ Medio, data = crecimiento, sd)

print(sd_med)

#   Medio TasaCrecimiento
# 1    LB       0.2479398
# 2    M9       0.4215787
# 3   TSB       0.2510800

#Datos de abundancia de secuencias
#Descripción: Supón que tienes un data.frame con datos de abundancia de secuencias de diferentes microorganismos en distintas muestras.

#Crea un data.frame secuencias con las columnas: Muestra, Microorganismo, Abundancia.
#Llena el data.frame con datos aleatorios para 8 microorganismos en 5 muestras diferentes.
#Encuentra la abundancia total y promedio por muestra.

Muestra <- rep(paste0("Muestra", 1:5), each = 8)

print(Muestra)

# "Muestra1" "Muestra1" "Muestra1" "Muestra1" "Muestra1" "Muestra1" "Muestra1" "Muestra1" "Muestra2" "Muestra2" "Muestra2" "Muestra2" "Muestra2" "Muestra2"
# "Muestra2" "Muestra2" "Muestra3" "Muestra3" "Muestra3" "Muestra3" "Muestra3" "Muestra3" "Muestra3" "Muestra3" "Muestra4" "Muestra4" "Muestra4" "Muestra4"
# "Muestra4" "Muestra4" "Muestra4" "Muestra4" "Muestra5" "Muestra5" "Muestra5" "Muestra5" "Muestra5" "Muestra5" "Muestra5" "Muestra5"

Micro <- rep(paste0("Micro", 1:8), times = 5)

print(Micro)

# "Micro1" "Micro2" "Micro3" "Micro4" "Micro5" "Micro6" "Micro7" "Micro8" "Micro1" "Micro2" "Micro3" "Micro4" "Micro5" "Micro6" "Micro7" "Micro8" "Micro1"
# "Micro2" "Micro3" "Micro4" "Micro5" "Micro6" "Micro7" "Micro8" "Micro1" "Micro2" "Micro3" "Micro4" "Micro5" "Micro6" "Micro7" "Micro8" "Micro1" "Micro2"
# "Micro3" "Micro4" "Micro5" "Micro6" "Micro7" "Micro8"


Abund <- sample(50:500, 40, replace = TRUE)

print(Abund)

# 364 469 212  75  69 449 353 146 410 370 140 372  98 118 492 404 398 376 438 323 480 131  67 492 281 363 500 270 306 207 301 441 225 293  73 497 345 347 176
# 390

seq <- data.frame(Muestra, Micro, Abund)

print(seq)

#    Muestra  Micro  Abund
# 1  Muestra1 Micro1   364
# 2  Muestra1 Micro2   469
# 3  Muestra1 Micro3   212
# 4  Muestra1 Micro4    75
# 5  Muestra1 Micro5    69
# 6  Muestra1 Micro6   449
# 7  Muestra1 Micro7   353
# 8  Muestra1 Micro8   146
# 9  Muestra2 Micro1   410
#10 Muestra2 Micro2   370
#11 Muestra2 Micro3   140
#12 Muestra2 Micro4   372
#13 Muestra2 Micro5    98
#14 Muestra2 Micro6   118
#15 Muestra2 Micro7   492
#16 Muestra2 Micro8   404
#17 Muestra3 Micro1   398
#18 Muestra3 Micro2   376
#19 Muestra3 Micro3   438
#20 Muestra3 Micro4   323
#21 Muestra3 Micro5   480
#22 Muestra3 Micro6   131
#23 Muestra3 Micro7    67
#24 Muestra3 Micro8   492
#25 Muestra4 Micro1   281
#26 Muestra4 Micro2   363
#27 Muestra4 Micro3   500
#28 Muestra4 Micro4   270
#29 Muestra4 Micro5   306
#30 Muestra4 Micro6   207
#31 Muestra4 Micro7   301
#32 Muestra4 Micro8   441
#33 Muestra5 Micro1   225
#34 Muestra5 Micro2   293
#35 Muestra5 Micro3    73
#36 Muestra5 Micro4   497#
#37 Muestra5 Micro5   345
#38 Muestra5 Micro6   347
#39 Muestra5 Micro7   176
#40 Muestra5 Micro8   390

ab_total <- aggregate(Abund ~ Muestra, data = seq, sum)

print(ab_total)

#    Muestra Abund
# 1 Muestra1  2137
# 2 Muestra2  2404
# 3 Muestra3  2705
# 4 Muestra4  2669
# 5 Muestra5  2346

prom_muestra <- aggregate(Abund ~ Muestra, data = seq, mean)

print(prom_muestra)

#    Muestra   Abund
# 1 Muestra1 267.125
# 2 Muestra2 300.500
# 3 Muestra3 338.125
# 4 Muestra4 333.625
# 5 Muestra5 293.250

