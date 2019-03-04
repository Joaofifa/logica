1. Rubí Rojas Tania Michelle
   # cuenta: 315121719

2. Ruiz Melo Jean Paul
   # cuenta: 314126546

Obervaciones:
1. Al trabajar sobre los elementos y las aristas al mismo tiempo, hace que se 
   tiene que usar funciones auxiliares, si no se puede complicar mucho el 
   algoritmo.
2. La gramática para el lenguaje proposicional ha sido cambiado únicamente para
   las proposiciones booleanas, es decir, TTrue ahora es Top y FFalse ahora es 
   Bot. Esto para mayor comodidad y entendimiento.
   Hay que tenerlo en cuenta sobre todo en la función inter, ya que recibe un 
   valor que debe der Top (True) o Bot (False).

3. Se agregó Show en el deriving() del lenguaje proposicional ya que mandaba un
   error de compilación porque haskell bebé no podía mostrar la salida en la 
   terminal.

4. Todas las definiciones de lógica proposicional se obtuvieron de las
   siguientes notas de clase del profesor Favio.

   (i) https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnx1bmFtbGMxNjJ8Z3g6M2QwODFjYzU5MGNjMjhkNQ
   (ii) https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnx1bmFtbGMxNjJ8Z3g6NDVmZjViYWU1OTQzMWM1MA
   
   En particular, para la función fnc, se siguió el algoritmo planteado en (i)
   para la implementación de la función mencionada.
   También, para la función correcto, se siguió el algoritmo planteado en (ii)
   para la implementación de la función mencionada.

5. Respecto a la función correcto. Al momento de ejecutarlo, mada un error que 
   no logramos identificar, y por ende, corregir. Nos parece que todo el 
   algoritmo es correcto, pero la falla puede venir de la función modelo.
   Lo que hicimos en esta función es utilizar la función consecuencia lógica
   como auxiliar para saber si el al argumento es correcto. Para ello,
   usamos una función que nos dice si el argumento es una contradicción, es 
   decir, que para toda interpretación I de I(gamma) se tiene que I(gamma) = 0.
   Para saber esto, utilizamos la función modelo, que nos debería de dar la 
   lista con todos los modelos de I (entonces si modelo nos regresa la vacía
   es que es insatisfacible). Aquí nos confundió mucho el cómo estaba definido
   Estado, suponemos que aquí es el fallo de la función correcto. 

   Nota: Personalmente me gustaría saber cómo es que pudimos haber corregido 
   este error, así que si nos pudieras mandar un correo con dicha observación
   te lo agradeceríamos infinitamente.
