#IF CONDICIONAL

mayor_de_edad<-18 #inicializar la variable

edad<-20

if(edad >=mayor_de_edad){
  print("Eres mayor de edad")
}

x<-5+4
print(x)

minimo<-20000
dinero<-15000

if(dinero>=minimo){
  print("¿Cómo está Cancún?")
  print("La vida es buena")
  sobrante<-dinero-minimo
  print(paste("Me queda $", sobrante))
}
print("Acá sigue")

#Elabora un programa que compare tu estatura con tu ídolx y determine si eres más altx.

estatura_vanne<-1.64

estatura_shohei<-1.93

if(estatura_vanne > estatura_shohei){
  print("eres más alta") #no sé por qué me sale que sí soy más alta:(, lol me faltaban los corchetes
}

if(estatura_vanne > estatura_shohei){
  print("Eres más alta")
} else if(estatura_vanne < estatura_shohei){
  print("Tu ídolo es más alto")
} else{
  print("Miden lo mismo")
} #ahora sí ya me salió
#¿Cómo harías una condición que considere que te gusta el mole y el pozole?
mole<-"sí me gusta"
pozole<-"sí me gusta"

if(mole=="sí me gusta" & pozole== "sí me gusta")
  print ("ambos me gustan")

if(mole=="sí me gusta" |pozole== "sí me gusta")
  print ("ambos me gustan")
#aqui tengo que corregir el codigo para agregar los corchetes

if(mole == "sí me gusta" & pozole == "sí me gusta"){
  print("Me gustan ambos")
} else if(mole == "sí me gusta" | pozole == "sí me gusta"){
  print("Me gusta uno de los dos")
} else{
  print("No me gusta ninguno")
}
#Elabora un programa que con tu fecha de cumpleaños te diga en qué estación del año naciste.

#Elabora un programa que con tu fecha de cumpleaños te diga en qué estación del año naciste.

cumpleanos <- "febrero"

if(cumpleanos == "diciembre" | cumpleanos == "enero" | cumpleanos == "febrero"){
  print("CUMPLES EN INVIERNO")
}

if(cumpleanos == "marzo" | cumpleanos == "abril" | cumpleanos == "mayo"){
  print("CUMPLES EN PRIMAVERA")
}

if(cumpleanos == "junio" | cumpleanos == "julio" | cumpleanos == "agosto"){
  print("CUMPLES EN VERANO")
}

if(cumpleanos == "septiembre" | cumpleanos == "octubre" | cumpleanos == "noviembre"){
  print("CUMPLES EN OTOÑO")
}
