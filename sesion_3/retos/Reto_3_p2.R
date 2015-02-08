## Algoritmo que dado un umbral por el usuario, dados 10 numeros por el usuario, cuente cuantos de esos numeros supera el umbral indicado.

numeros <- scan (n=10) # Se eligen 10 números elegidos por el usuario

umbral <- scan (n=1) #Umbral elegido por el usuario

contar <- 0 #En esta variable se cuentan los valores que van superando el umbral indicado anteriormente

for (valores in numeros){ # Para cada valor de "numeros" se hace todo lo que hay dentro del bucle
  if (valores > umbral){ #Si el valor es mas grande que el umbral hacer lo siguiente:
    contar <- contar + 1 # Se suma 1 a contar que es 0 inicialmente. De esta forma se van sumando en contar los valores que superan el umbral
  }
}
print (contar) #Muestra el resultado de "contar"
