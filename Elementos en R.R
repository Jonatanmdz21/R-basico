#####. Elementos de R. ####

## como declarar objetos 

 7 -> x

x

###tipos de variables 

x= 3 #numerico

class(x)     

x='Jonatan Mendoza'   #caracter

class(x)

x



x="2019-12-25"  #fecha ??


class(x)

x= as.Date(x) # cambio de clase a fecha 


class(x)  

x

## tipos de objetos 

#variables 

HolaMundo=2

y=2

#vectores 

edad= c(12, 34, 23, 22, 45, 32)

edad 

nombres= c('Mario', 'Pedro','victoria', 'Maria', 'Lisa', 'Jose')

nombres


#extraccion de valor en vector 

edad[4]

nombres[2:3]

SnVk=nombres[-3]

SnVk


##matrices 

mymatrix <- matrix(data=1:20, nrow=4,
                   ncol=5, byrow=T)

mymatrix[4, 1]


##arreglos 

miarray <- array(data=letters[1:24], dim=c(3, 4, 2))

miarray

miarray[1, 4, 1]



## data frames *** 



##funciones 

suma= function(a,b,c){
  a+b+c
}

suma(13,33,5)

basicas= function(i,m){
  print (paste(sprintf("la suma de %i + %i es: ", i, m), i+m))
  print (paste(sprintf("la resta de %i-%i es: ", i, m), i-m))
  print (paste(sprintf("la multiplicacion de %ix%i es: ", i, m), i*m))
  print (paste(sprintf("la divicion de %i/%i es: ", i, m), i/m))
}

basicas(10,2)

rm(mymatrixAndrea)
