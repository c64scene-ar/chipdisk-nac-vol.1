# Chipdisk

Se recomienda correrlo en una Commodore 64 de norma PAL-B y SID 8580.

## Cómo compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`

## Tracks originales

En el directorio [res/orig](res/orig) se encuentran los archivos originales de
algunos de los .SIDs presentes en este chipdisk.  Las extensiones corresponden
a los siguientes trackers:

- `.ct`: CheeseCutter
- `.sng`: GoatTracker
- `.swm`: SID-Wizard

## Internals

### Directorio

- `src`: todo lo que se compila o se incluye: source code, assets listos para
  ser incluidos, SIDs comprimidos (.exo)
- `res`: los assets en formato "original". Ej: archivos .vcharproj, .sid, etc.
- `bin`: el binario compilado

### Memoria

Ver el archivo [chipdisk.cfg](chipdisk.cfg) que muestra cómo se usa la memoria.
Hay 2 lugares para agregar código. "CODE", y "MORECODE". Tratar de que todo
entre ahí para evitar sacarle espacio a posibles nuevos SIDs, y además vamos a
necesitar memoria para los botones.

De dónde sacar memoria en caso que se necesite más:

- Fonts: Estan reservados 2k para los fonts, pero probablemente no se necesite
  tanto.
- Comprimir los botones apretaods.
- Usar la memoria de $0400 a $0800 (1k) para poner código. La idea seria que
  ese código este temporalmente en $1000 y luego se copia a $0400
- El espacio de $1000 a $2800 esta reservado para tocar los sids. pero se puede
  usar cosas temporales (y luego moverlas a otro lado) antes de tocar el primer
  sid.
- El buffer de $1000 a $2800 tiene que ser tan grande como el tamaño del sid
  más grande a tocar. Ejemplo: Si el .sid más grande ocupa 4k, entonces ese
  tamaño se puede reducir de $1000 a $2000, liberando memoria (2k)

### SIDs comprimidos

Los SIDs estan comprimidos con exomizer dentro del binario. Para generar los
archivos .exo, usar este script:
[sid_to_exo.py](https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_to_exo.py)


## Licencia

Se distribuye el código fuente bajo la licencia Apache 2.
Ver [LICENSE](LICENSE) para más información.
