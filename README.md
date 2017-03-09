# Chipdisk Nacional Volumen 1

Se recomienda correrlo en una Commodore 64 con SID 8580. Funciona en PAL-B, NTSC y PAL-N (Drean).

## Cómo compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`

## Tracks originales

Los sids y los fuentes de los sids se encuentran aca:

- Fuentes de los sids (archivos SidWizard, CheeseTracker, etc.): [res/sids-source](res/sids-source)
- sids: [res/sids](res/sids)

## Internals

### Directorio

- `src`: todo lo que se compila o se incluye: source code, assets listos para
  ser incluidos, SIDs comprimidos (.exo)
- `res`: los assets en formato "original". Ej: archivos .vcharproj, .sid, etc.
- `bin`: el binario compilado


## Cómo esta hecho

Aca esta documentado como esta hecho el Chipdisk internamente. Comentamos
los distintos truquitos que usamos y más.

- [chipdisk_internals.es.rst](chipdisk_internals.es.rst)

## Licencia

Se distribuye el código fuente bajo la licencia Apache 2.
Ver [LICENSE](LICENSE) para más información.


