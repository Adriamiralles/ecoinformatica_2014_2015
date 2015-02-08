## Algoritmo que multiplique 5 numeros indicados por el usuario.

numeros <- scan (n=5) # Esta función nos da 5 números que elige el usuario.

multi <- 1 # Variable para hacer que los valores del scan se vayan multiplicando

for (valores in numeros)  {   #Para los valores de numeros haz lo que hay dentro del bucle
  multi <- multi * valores    #En este paso se va multiplicando multi por los numeros elegidos por el usuario y se van acumulando los valores multiplicados en el multi hasta que lo se multiplican todos
}

print (multi) # Muéstrame el resultado de multi
