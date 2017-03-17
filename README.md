# Chipdisk Nacional Volumen 1 (english)

Recommended setup:

- Commodore 64 with SID 8580
- Works with PAL, NTSC, PAL-N (Drean)

## How to compile it

- Download [cc65](http://cc65.github.io/cc65/) and add it to the path
- Download [exomizer](http://hem.bredband.net/magli143/exo/) and add it to the path
- Download [VICE](http://vice-emu.sourceforge.net/) and add it to the path

and then do `make`

        $ git clone https://github.com/c64scene-ar/chipdisk-nac-vol.1.git
        $ cd chipdisk-nac-vol.1.git
        $ make

## Original Tracks

The sids and its sources are here:

- Sid sources (SidWizard and CheeseCutter files): [res/sids-source](res/sids-source)
- Sids: [res/sids](res/sids)

## Internals

### Directory

- `src`: includes source code + assets needed by the source code
- `res`: includes the data in its original format: .vcharproj, sids, etc.
- `bin`: the compiled binary


## Documentation

- [chipdisk_internals.en.rst](chipdisk_internals.en.rst)

## License

[Apache v2 License](LICENSE)


# Chipdisk Nacional Volumen 1 (español)

Se recomienda correrlo en una Commodore 64 con SID 8580. Funciona en PAL-B, NTSC y PAL-N (Drean).

## Cómo compilarlo

- Bajarse [cc65](http://cc65.github.io/cc65/) y ponerlo en el path
- Bajarse [exomizer](http://hem.bredband.net/magli143/exo/) y ponerlo en el path
- Bajarse [VICE](http://vice-emu.sourceforge.net/) y ponerlo en el path

Y luego darle `make`

        $ git clone https://github.com/c64scene-ar/chipdisk-nac-vol.1.git
        $ cd chipdisk-nac-vol.1.git
        $ make

## Tracks originales

Los sids y los fuentes de los sids se encuentran aca:

- Fuentes de los sids (archivos SidWizard, CheeseCutter, etc.): [res/sids-source](res/sids-source)
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


