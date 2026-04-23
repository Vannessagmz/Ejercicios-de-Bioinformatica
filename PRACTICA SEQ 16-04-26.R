#bioconductor - repositorio para bioinformatica en RStudio

if (!require("BiocManager", quietly = TRUE))
  +     install.packages("BiocManager")

BiocManager::install(version = "3.22")

BiocManager::install("Biostrings")

#BiocManager::install() y se ingresa el nombre del paquete que se quiere instalar
#Instalar Biostrings desde Bioconductor

library(Biostrings)

DNA1 <- DNAString ("ACCTGTTGACCCCAGGT")
DNA1 #ESTO ES PARA MOSTRAR LA SECUENCIA EN LA CONSOLA A COLORES POR LETRA

DNA2 <- DNAStringSet(c("ACTG", "CCTG", "AAAT"))
DNA2 #ESTO ES PARA CUANDO HAY VARIAS SECUENCIAS

IUPAC_CODE_MAP
#   A      C      G      T      M      R      W      S      Y      K      V      H      D      B      N 
#   "A"    "C"    "G"    "T"   "AC"   "AG"   "AT"   "CG"   "CT"   "GT"  "ACG"  "ACT"  "AGT"  "CGT" "ACGT"
DNA1[1:5]
# 5-letter DNAString object
# seq: ACCTG
DNA2 [[1]][1:2]
# 2-letter DNAString object
# seq: AC

names(DNA2) <- c("seq1", "seq2", "seq3") #para asignar nombres a las secuencias
DNA2

#DNAStringSet object of length 3:
#       width seq                       names               
#[1]     4 ACTG                         seq1
#[2]     4 CCTG                         seq2
#[3]     4 AAAT                         seq3

#WIDTH DA EL TAMAÑO DE LA SECUENCIA
width(DNA2)
# [1] 4 4 4

#REV DA LA SECUENCIA AL REVÉS
rev(DNA2)

# DNAStringSet object of length 3:
#        width seq                      names               
#[1]     4 AAAT                         seq3
#[2]     4 CCTG                         seq2
#[3]     4 ACTG                         seq1

translate(DNA1) #DA LOS AMINOACIDOS CORRESPONDIENTES AL TRIPLETE
#8-letter AAString object
# seq: TC*PQVVT (ASTERISCO ES UN CODON DE PARO)
translate(DNA2)
#AAStringSet object of length 3:
#     width seq         names               
#[1]     1 T             seq1
#[2]     1 P             seq2
#[3]     1 K             seq3
reverseComplement(DNA2)
#DNAStringSet object of length 3:
#         width seq                     names               
# [1]     4 CAGT                        seq1
# [2]     4 CAGG                        seq2
# [3]     4 ATTT                        seq3
alphabetFrequency(DNA1)
#A C G T M R W S Y K V H D B N - + . (MAS MENOS Y PUNTO SON LAS DELECIONES)
#6 8 5 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 

letterFrequency(DNA1, "A")
#A 
#6 
library(BSgenome)
install(BSgenome)
install.packages("BSgenome")
