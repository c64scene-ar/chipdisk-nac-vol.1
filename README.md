# Chipdisk Nacional Volumen 1

Se recomienda correrlo en una Commodore 64 con SID 8580. Funciona en PAL-B, NTSC y PAL-N (Drean).

## Cómo compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`

## Tracks originales

En el directorio [res/sids](res/sids) se encuentran los archivos los SIDs.

## Internals

### Directorio

- `src`: todo lo que se compila o se incluye: source code, assets listos para
  ser incluidos, SIDs comprimidos (.exo)
- `res`: los assets en formato "original". Ej: archivos .vcharproj, .sid, etc.
- `bin`: el binario compilado

### Memoria

Más info sobre esto próximamente. Mientrastanto ver los archivos `.cfg` para darse
una idea de como esta organizada la memoria.

### SIDs comprimidos

Los SIDs estan comprimidos con exomizer dentro del binario. Para generar los
archivos .exo, usar este script:
[sid_to_exo.py](https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_to_exo.py)


## Licencia

Se distribuye el código fuente bajo la licencia Apache 2.
Ver [LICENSE](LICENSE) para más información.
