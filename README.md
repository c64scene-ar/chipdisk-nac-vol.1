# Como compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`


# Internals

## Directorio

- `src`: todo lo que se compila o se incluye: source code, assets listos para ser incluidos, SIDs comprimidos (.exo)
- `res`: los assets en formato "original". Ej: archivos .vcharproj, .sid, etc.
- `bin`: el binario compilado

## Memoria

Ver el archivo [chipdisk.cfg](chipdisk.cfg) que muestra como se usa la memoria. 
Hay 2 lugares para agregar código. "CODE", y "MORECODE". Tratar de que todo entre ahí para evitar sacarle espacio a posibles nuevos SIDs, y además vamos a necesitar memoria para los botones.

De donde sacar memoria en caso que se necesite más:

- Fonts: Estan reservados 2k para los fonts, pero probablemente no se necesite tanto.
- Comprimir los botones apretaods.
- Usuar la memoria de $0400 a $0800 (1k) para poner código. La idea seria que ese código este temporalmente en $1000 y luego se copia a $0400
- El espacio de $1000 a $2800 esta reservado para tocar los sids. pero se puede usar cosas temporales (y luego moverlas a otro lado) antes de tocar el primer sid.
- El buffer de $1000 a $2800 tiene que ser tan grande como el tamaño del sid más grande a tocar. Ejemplo: Si el .sid más grande ocupa 4k, entonces ese tamaño se puede reducir de $1000 a $2000, liberando memoria (2k)

## SIDs comprimidos.

Los SIDs estan comprimidos con exomizer dentro del binario. Para generar los archivos .exo, usar este script: [sid_to_exo.py](https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_to_exo.py)


# Que falta


## Necesario

- Los temas! Definir que temás van. Creo que solo hay 4 o 5 de temas de los temas oficiales (el resto de los temas que estan, son de prueba). Y ver si se pueden poner más de 8 temas.
- Que detecte final de cancion y le de play a la siguiente. Detectar final de cancion se puede hacer que al cabo de N segundos toque la siguiente canción. Se usa una suerte de "ticks por canciíon".
- Que use los graficos de los botones apretados. Puede llegar a consumir mucha memoria. Hay que tener precaución al implementar esto.
- Que anime la ruedita con sprites: se necesitan nuevos sprites
- Que el contador este en el lugar correcto. Probablemente se pueda hacer con sprites. Se puede hacer con bitmap también pero el gráfico para el contador debería ser un poco más grande
- Known bugs:
   - se esta pisando la sombra del cassette cuando se plotea los nombres. Hay que hacer un _save_ de esa celda y luego _restorearla_
- Fonts: Asi no usamos el default.

## Opcional

- Easter Egg: definir que tiene que hacer, y hacerlo
- Soporte de teclado. En vez que las teclas muevan el cursor como si fuera un mouse, que cicle entre posiciones predefinidas como "Play", "Rew", ...
- Fade In al comienzo. Usar la memoria de $1000 a $2800 para eso, para no quitar memoria del player


