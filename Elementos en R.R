#####Elementos de R####

hcahcja
###tipos de variables 

x=3  #numerico

class(x)     

x='abc'   #caracter

class(x)

x



x="2019-12-25"  #fecha ??


class(x)

x= as.Date(x)


class(x)  


## tipos de objetos 

#variables 

x=2

#vectores 

edad= c(12,34, 16, 22)

edad 

nombres=c('Juan', 'Pedro', 'Maria', 'Lisa')

nombres


#extraccion de valor en vector 

edad[2]

nombres[1:3]

nombres[-2]


##matrices 

mimatriz <- matrix(data=1:20, nrow=4, ncol=5, byrow=FALSE)

mimatriz[2, 3]


##arreglos 

miarray <- array(data=letters[1:24], dim=c(3, 4, 2))


miarray[1, 3, 2]



## data frames *** 



##funciones 

suma= function(a,b){
  a+b
}

suma(2,3)

basicas= function(i,m){
  print (paste(sprintf("la suma de %i+%i es: ", i, m), i+m))
  print (paste(sprintf("la resta de %i-%i es: ", i, m), i-m))
  print (paste(sprintf("la multiplicacion de %ix%i es: ", i, m), i*m))
  print (paste(sprintf("la divicion de %i/%i es: ", i, m), i/m))
}

basicas(5,4)
