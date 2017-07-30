Cursito intermedio de asm para la c64 - Parte I: Haciendo un Chipdisk
=====================================================================

:Versión: 0.1.1 (`ir a última versión <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/chipdisk_internals.es.rst>`__)
:Autor: `riq <http://retro.moe>`__ / `Pungas de Villa Martelli <http://pungas.space>`__

.. contents:: Contenidos
   :depth: 2

Introducción
============

Hola. Primero bajarse el Chipdisk para darse una idea de lo que abarca
el cursito:

-  `chipdisk-nac.d64 <https://github.com/c64scene-ar/chipdisk-nac-vol.1/raw/master/bin/chipdisk-nac.d64>`__

Listo, empecemos. Tocar un sid en una Commodore 64 es muy sencillo:

.. code:: asm

    setup:
        sei                 ; prohibir interrupciones

        lda #<irq_vector    ; setear vector IRQ para ser llamado
        sta $0314           ; una vez por cada refresh de pantalla
        lda #>irq_vector
        sta $0315           ; el vector $314/$315 apunta a la rutina raster IRQ

        lda #$00
        jsr $1000           ; inicializar sid para que toque canción 0
                            ; ya que un sid puede tener más de una canción

        cli                 ; habilitar interrupciones nuevamente
        rts

    irq_vector:
        asl $d019           ; ACK interrupción de raster

        jsr $1003           ; llamar a tocar el sid

        jmp $ea31           ; salir de la interrupción

...y listo. Cada sid sabe tocarse solo, ya que un sid es código + data.
La llamada ``jsr $1003`` hace toda la magia, y ese código esta dentro
del sid.

Lo complicado de hacer un Chipdisk, no es tocar el sid, sino todo lo
demás. Veamos el porqué.


Organizando la memoria
======================

VIC, bancos y demás
-------------------

    .. note:: Supongo que ya sabes como usar sprites y modos gráficos. Si aún
      no lo sabes, ir a `Bringing Sprites in good Shape <http://dustlayer.com/vic-ii/2013/4/28/vic-ii-for-beginners-part-5-bringing-sprites-in-shape>`__
      y a `Screen Modes Cheaper by the Dozen <http://dustlayer.com/vic-ii/2013/4/26/vic-ii-for-beginners-screen-modes-cheaper-by-the-dozen>`__

Empecemos por lo básico. Hay 64k RAM disponibles, pero cuando prendemos la compu
dice que hay ``38911 BASIC BYTES FREE``. Eso significa que si vamos a usar BASIC,
solo hay 38k RAM libres.

.. figure:: https://lh3.googleusercontent.com/q9Fndsw89AVrXaPtPwr9FUPH42cbtExt4vuyi_VpAFCXG_W_7nMhPqZ2-CAfSbFaERt0IK-9eqAlY2nJrM4FKwZ--hEpjcbTzlCrcIKTXJ5ESBGulrjjiN3KsF-1bcztXnww_a0
   :alt: memory\_free

   *Hay 38k RAM libres para usar desde BASIC, pero 64k RAM desde asm*

Pero como no vamos a usar BASIC, lo *apagamos* y nos libera 8k RAM.
Y si seguimos apagando todo, como el KERNAL y demás, entonces ahí vamos a tener
los 64k RAM libres.

Para prender/apagar estas cosas se usa la dirección `$01`_

En el Chipdisk usamos dos configuraciones:

-  Todo apagado menos ``$d000-$dfff`` (area reservada para IO:
   VIC/SID/CIA): se usa siempre, salvo cuando se descomprimen los
   sids
-  Todo apagado (64k RAM libres): se usa cuando se descomprimen los
   sids, ya que el area de ``$d000-$dfff`` es usada por los sids
   comprimidos

Se usa así:

.. code:: asm

        lda #37                 ; valor por defecto de la C64
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: BASIC
                                ; C000-CFFF: RAM
                                ; D000-DFFF: IO (VIC,SID/CIA)
                                ; E000-FFFF: KERNAL

        lda #$35                ; usada por el Chipdisk normalmente
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: RAM
                                ; C000-CFFF: RAM
                                ; D000-DFFF: IO (VIC,SID/CIA)
                                ; E000-FFFF: RAM

        lda #$34                ; usada por el Chipdisk cuando descomprime
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: RAM
                                ; C000-CFFF: RAM
                                ; D000-DFFF: RAM
                                ; E000-FFFF: RAM

Hay varias posibles combinaciones. Ir `acá para más info <http://unusedino.de/ec64/technical/aay/c64/zp01.htm>`__

La otra cosa, es que el VIC (la *GPU* de la compu) necesita de la memoria RAM.
Si queremos dibujar un gráfico bitmap, ponemos el gráfico en la memoria RAM y
el VIC lo lee de ahí (de la RAM). Así que la RAM se comparte entre la CPU (el 6510)
y la GPU (el VIC).

Pero hay un limitación: El VIC solo puede ver 16k a la vez de los 64k RAM.
Hay 4 bancos de 16k cada uno (``64k / 16k == 4``) de los cuales el VIC puede
leer la data.

- Banco 0: ``$0000 - $3fff``
- Banco 1: ``$4000 - $7fff``
- Banco 2: ``$8000 - $bfff``
- Banco 3: ``$c000 - $ffff``

Esto significa que un gráfico bitmap no puede estar mitad en un banco y mitad
en otro. Tiene que estar entero en un solo banco.

Pero eso no es todo. No puede estar en cualquier parte del banco. Hay lugares
especiales para poner el bitmap, el charset y la screen RAM.

Y a eso se le suman otras limitaciones, pero vamos de poco.  Para decirle al
VIC que banco usar, se hace a travez del registro `$dd00`_ del CIA 2, así:

.. code:: asm

        lda $dd00                       ; CIA 2
        and #$%11111100                 ; máscara de los 2 primeros bits
        ora #2                          ; 3 para Banco 0
                                        ; 2 para Banco 1
                                        ; 1 para Banco 2
                                        ; 0 para Banco 3
        sta $dd00

Y para decirle al VIC donde esta el bitmap, charset y screen+sprite ptr., se hace a
travez del registro `$d018`_ del VIC.

.. figure:: https://lh3.googleusercontent.com/hRPBQeC8azhb1h5fmaBBfaLfqA_zQgGvFEI56Dyq-lIpAOzCbQCwsoGiynGc2Zr-XBcLJXGbmnfPsdbK_xwWAjw48-Fs2Lknnx9TGaHGj2ttM5oPYOmZVxhVLdP-YzqILJCZwTk
   :alt: internals de cada banco

   *Memoria interna de cada banco*

Pero eso no es todo. Los bancos 0 y 2 (``$0000-$3fff`` y ``$8000-$bfff``) tienen
mapeados entre ``$1000-$1fff`` y ``$9000-$9fff`` respectivamente al charset
default (mayúsculas y minúsculas). Eso significa que no podemos usar esas
direcciones para que el VIC vea data... salvo para que vea el charset default.

.. figure:: https://lh3.googleusercontent.com/hgGTs3AF3tFO6FuL3F1aWGujcLNspxEFnY6JARm53sRvWik8hTKNJAPDgMFbzeoJCu_LPDy7Tyaz7tjrMUO9tHwwiHQXw74_W87_uIbPpQR_cZCVCE8oRHikpQ2WrGpDp_DC46A
   :alt: bancos del VIC

   *Los cuatro bancos disponibles*

Y el VIC *ve* el charset por default en esas direcciones porque el charset
tiene que estar en algún lugar. Pero si esta en la RAM va a ocupar memoria RAM,
entonces de los 38k libres para el BASIC, habría ahora 4k menos. Y supongo que
para que no suceda eso, lo ingenieros de C= decidieron mapearle al VIC el
charset en esas direcciones.


Resumiendo:

-  Hay 4 bancos posibles donde poner la data para el VIC
-  Los valores del VIC son módulo ``$4000``
-  En las direcciones ``$1000-$1fff`` y ``$9000-$9fff``, el VIC **ve** el charset por default
-  Se usa `$dd00`_ para cambiar de banco. Y `$d018`_ para decirle al SID de donde sacar la data


Los sids, Exomizer, y demás
---------------------------

¿Cuanta RAM necesitamos para el Chipdisk? Hagamos cuentas.
El Chipdisk esta compuesto por 3 módulos:

-  Intro: Mitad gráfico multi-color + mitad pantalla PETSCII + charset +
   código
-  Player: 9 canciones (sids) + sonido para ruido blanco + gráfico
   bitmap + charset + código
-  Easter Egg: 1 canción (sid) + gráfico PETSCII + texto scroll + código

|intro|\ |player|\ |easteregg|

Y solo el módulo del Player ocupa:

-  Los 9 sids: ~53k
-  Gráfico bitmap: 9k (8k bitmap + 1k colores)
-  Ruido blanco (usando entre tema y tema): ~1,8k
-  Imágenes de botones apretados (bitmap + colores): ~1,7k
-  Charset (usado en letras oblicuas): 1k
-  Sprites (cursores, rueditas, contador): ~1k

Y nos da un total de: ~65k, sin contar código, ni Intro, ni Easter Egg.
¿Cómo hacemos para meter todo en 64k de memoria y sin acceder al disco?

La respuesta es: Se comprime todo lo que se pueda comprimir, y se descomprime
cuando se necesita.

-  Los 9 sids comprimidas [#]_ usando Exomizer_ ocupan: ~28k

Pero para que un sid se pueda "tocar" un sid hay que descomprimirlo en algún
lugar. Para eso se necesita RAM libre. Entonces necesitamos un buffer tan
grande como el sid más grande.

En nuestro caso, el sid que más ocupa es *Prófugos* con 9k. Algo
bastante inusual para un sid (generalmente no ocupan más de 4k), pero
por los instrumentos y demás, no se puede achicar más que eso sin
perder calidad de sonido.

Entonces necesitamos un total de 37k (28k + 9k) para los sids. Algo
mucho mejor que los 53k original (¡16k menos!).

El buffer de 9k empieza en la dirección ``$1000``. Puede empezar en cualquier
otro lado, pero como los sids por defecto corren en ``$1000``, seguimos
usando ``$1000``. Así que de ``$1000`` a ``$3328`` (8952 bytes) están
reservados para descomprimir los sids.

    .. note:: ¿Y saben por qué casi todos los sids empiezan en ``$1000``? Vean la sección
      anterior para saberlo.

Los sids comprimidos están a partir de ``$7cb0``. Cuanto más arriba
mejor, así libera lugar para el gráfico bitmap (ver más abajo).

Hasta ahora la memoria esta así:

::

    $0000 - $0fff: Libre (4k)
    $1000 - $32f7: Buffer reservado para tocar un sid (~9k)
    $32f8 - $7caf: Libre (18k)
    $7cb0 - $fbdf: Sids comprimidos (28k)
    $fbe0 - $ffff: Libre (1k)

Gráfico Bitmap y demás
----------------------

Ahora hay que poner al gráfico algún lugar. Un buen lugar es ponerlo en el Banco 1.
Usar de ``$4000-6000`` para el bitmap, y de ``$6000-$6400`` para los colores.
Y si agregamos los sprites, sid de ruido blanco y demás, queda así:

::

    $0000 - $0fff: Libre (4k)
    $1000 - $32f7: Buffer para tocar el sid más grande (~9k)
    $32f8 - $3fff: Libre (~3k)
    $4000 - $5fff: Gráfico bitmap (8k)
    $6000 - $63ff: Gráfico color (Screen RAM) (1k)
    $6400 - $68ff: Sprites (~1k)
    $6900 - $6cff: Charset (1k)
    $6d00 - $73ff: Sid de Ruido blanco (1.7k)
    $7400 - $7caf: Imagenes de botones apretados + buffer temporal (~2k)
    $7cb0 - $fbdf: Sids comprimidos (28k)
    $fbe0 - $ffff: Libre (1k)

Sobran 9k para poner el código del Player. Pero recordemos que en esos
9k también tiene que estar el Easter Egg. Eso complica las cosas
bastante. Poner la Intro no ocupa lugar en los 9k. Luego explicaré
porqué.


Código: El Player
=================

El código del player se puede dividir en:

-  Sprites: Animar rueditas y demás
-  Descomprimir sid, modificarlo para tocarlo en NTSC/Drean
-  Actualizar nombre de canción / autor
-  Leer eventos: mouse (port #1), joystick (port #2) o teclado
-  Animar botones apretados
-  Patchear gráfico bitmap con sprites
-  Actualizar número de canción

Sprites: Animar rueditas y demás
--------------------------------

.. figure:: https://lh3.googleusercontent.com/5gtsDGNPpV8eU6wD3jYBJnJmpG23iXHaXga_NbVDUpKQa5gCSbN_2_bmCAaJP7DLaaiBOauma2cJHrBYQmMnXsYUB7erJ2c4bUCdkFAcQjPgYyEPZCc2bpb9_db66AQ0pKdo9rM
   :alt: sprites

   *Sprites usados por el player*

Dentro del player se usan sprites en los distintos lugares:

- Animación de las rueditas: un sprite para cada ruedita
- Puntero: 2 sprites "overlaid"
- Boton de encendido: 1 sprite
- Contador para canciones: 1 sprite
- Arreglar "artifacs" del bitmap: 2 sprites

En total se usan 8 sprites, así que no hay necesidad de multiplexar los sprites.

.. figure:: https://lh3.googleusercontent.com/rZIaCnwOg7xCputC0GH9FF4xdUOl5-yW4c4ZgZpemclrt9qH6rbTglj91-NXl4tuC8aXvuheJiEiugWB-iP5o9uN4XW1W6TPFYzAdonBz4e9-et4Yc2VdBIXSaNn9MF7H4yGeWk
   :alt: donde están los sprites

   *Ubicación de los sprites*

La animación de las rueditas es trivial. Se cambia el sprite frame cada tantos
segundos. Veamos como se hace:

.. code:: asm

    SPRITE_DATA_ADDR = $6400
    SPRITE0_POINTER = <((SPRITE_DATA_ADDR .MOD $4000) / 64)     ; equivalente a 144
    TOTAL_FRAMES = 5

    do_anim_cassette:
            dec delay
            bne end                         ; fin del delay ?

            lda #3
            sta delay                       ; restaura el delay

            dec $63f8 + 6                   ; $63f8 + 6 es el "sprite pointer" del sprite 6
            lda $63f8 + 6                   ; lo compara con el primer frame - 1
            cmp #(SPRITE0_POINTER - 1)
            bne :+
            lda #(SPRITE0_POINTER + TOTAL_FRAMES - 1) ; si es así, setea el frame otra vez desde el final
    :       sta $63f8 + 6                   ; actualiza sprite pointer del sprite #6
            sta $63f8 + 7                   ; y lo mismo para el sprite #7
    end:
            rts
    delay:
            .byte 1

Y los sprites pointers están desde ``$63f8`` a ``$63ff`` ya que se esta usando
el Banco 1 (``$4000-$7fff``) y le dijimos al VIC que la Screen va a estar en
``$6000``.

Un truquito útil para que los sprites se vean mejor es dibujar un sprite
standard sobre otro sprite (standard o multi-color).

La idea es esta:

.. figure:: https://lh3.googleusercontent.com/T1TmdjKnu_7BrDTvQr3L1Sre2jmwlM-KTsnBpCuEjK9g7esu5pQyd1gXsVoUOR2_L4w4jsZKX7w_RkhfgsCdztt1wWJbuu1zkJ9X8DpM7Xp8CxEJY_hX-YqFkdBxQDrxObXxi1Y
   :alt: overlay sprites

   *Overlaid sprites*

Esta idea se usa mucho. Jueguitos como el Bruce Lee (y cientos de otros) la usan.
El único inconveniente es que usa 2 sprites en vez de uno.

Otro truquito que usamos, es arreglar "bugs" del bitmap con sprites. Recuerden
que las celdas del bitmap no pueden tener más de 2 colores. Y para solucionar
algunos pixeles que se ven mal, los tapamos con sprites.

Y eso es todo respecto a los Sprites del Player.


Descomprimir sids y modificarlos...
-----------------------------------

Los sids están comprimidos con Exomizer_. Así que se usa la rutina de
descompresión del Exomizer [#]_. Lo interesante de esta rutina, es que es "multi
tarea". Es decir, mientras descomprime se pueden hacer otras cosas. En nuestro
caso, cuando descomprimimos el sid, lo que hacemos es animar las rueditas del
casette:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; get_crunched_byte()
    ; Esta subrutina es llamada por el decruncher. X, Y y Carry se tiene que preservar.
    ; Actualiza el puntero del decruncher y anima las rueditas del casette
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    get_crunched_byte:
            lda _crunched_byte_lo           ; _crunched_byte_lo & _crunched_byte_hi
            bne @byte_skip_hi               ; son usados por el decruncher (exomizer)
            dec _crunched_byte_hi           ; para saber que byte tiene que Descomprimir
                                            ; cada vez que se llama a esta rutina
                                            ; lo que hay que hacer es decrementar en uno
                                            ; al puntero
    @byte_skip_hi:

            dec delay                       ; y de paso, lo que hacemos es animar las rueditas
            bne @cont                       ; del casette.
                                            ; "delay" es un timer para animar las rueditas
                                            ; a la velocidad correcta

            lda wheel_delay_counter         ; resetea el delay
            sta delay

            php                             ; guarda Status (Carry y otros) porque
                                            ; el decruncher necesita que estos valores
                                            ; no se modifiquen

            lda is_rewinding                ; si se pasa a la canción anterior, entonces
            beq @anim_ff                    ; animar ruedita para atras, sino para adelante
            inc $63f8 + 6                   ; $63f8 + 6 y + 7 son guardan los sprite frames
            lda $63f8 + 6                   ; de los sprites de las rueditas
            cmp #(SPRITE0_POINTER + TOTAL_FRAMES)
            bne :+
            lda #SPRITE0_POINTER
    :       sta $63f8 + 6                   ; actualiza los punteroes del sprite 6
            sta $63f8 + 7                   ; y del sprite 7
            jmp @done_anim
    @anim_ff:
            dec $63f8 + 6                   ; acá es lo mismo, pero con animación
            lda $63f8 + 6                   ; en "fast forward" (la otra era "rewind")
            cmp #(SPRITE0_POINTER - 1)
            bne :+
            lda #(SPRITE0_POINTER + TOTAL_FRAMES - 1)
    :       sta $63f8 + 6                   ; actualiza los punteros del sprite 6
            sta $63f8 + 7                   ; y 7
    @done_anim:
            plp                             ; restaura Status (Carry y otros)

    @cont:
            dec _crunched_byte_lo
    _crunched_byte_lo = * + 1
    _crunched_byte_hi = * + 2
            lda $caca                       ; self-modyfing. tiene que contener el último
                                            ; byte + 1 de lo que se quiere descomprimir
                                            ; antes de llamar a esta rutina
            rts
    delay:
            .byte 5

Una vez descomprimido el sid, hay que modificar la tabla de frecuencias
para que suene igual tanto en PAL, NTSC y Drean (PAL-N).

Para eso, no hay que hacer otra cosa que ir sid por sid y fijarse donde
esta la tabla de frecuencias de cada uno.

Las tablas de frecuencias generalmente tienen 96 valores:

-  8 octavas
-  de 12 semi-tonos cada una

Cada semi-tono ocupa 2 bytes, así que generalmente los sids almacenan
las tablas de la siguiente manera:

.. code:: asm

    ; PAL freq table
    freq_table_lo:
    ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
    .byte $17,$27,$39,$4b,$5f,$74,$8a,$a1,$ba,$d4,$f0,$0e  ; 1
    .byte $2d,$4e,$71,$96,$be,$e8,$14,$43,$74,$a9,$e1,$1c  ; 2
    .byte $5a,$9c,$e2,$2d,$7c,$cf,$28,$85,$e8,$52,$c1,$37  ; 3
    .byte $b4,$39,$c5,$5a,$f7,$9e,$4f,$0a,$d1,$a3,$82,$6e  ; 4
    .byte $68,$71,$8a,$b3,$ee,$3c,$9e,$15,$a2,$46,$04,$dc  ; 5
    .byte $d0,$e2,$14,$67,$dd,$79,$3c,$29,$44,$8d,$08,$b8  ; 6
    .byte $a1,$c5,$28,$cd,$ba,$f1,$78,$53,$87,$1a,$10,$71  ; 7
    .byte $42,$89,$4f,$9b,$74,$e2,$f0,$a6,$0e,$33,$20,$ff  ; 8

    freq_table_hi:
    ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02  ; 1
    .byte $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04  ; 2
    .byte $04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07,$08  ; 3
    .byte $08,$09,$09,$0a,$0a,$0b,$0c,$0d,$0d,$0e,$0f,$10  ; 4
    .byte $11,$12,$13,$14,$15,$17,$18,$1a,$1b,$1d,$1f,$20  ; 5
    .byte $22,$24,$27,$29,$2b,$2e,$31,$34,$37,$3a,$3e,$41  ; 6
    .byte $45,$49,$4e,$52,$57,$5c,$62,$68,$6e,$75,$7c,$83  ; 7
    .byte $8b,$93,$9c,$a5,$af,$b9,$c4,$d0,$dd,$ea,$f8,$ff  ; 8

Entonces lo que hay que hacer es buscar esas tablas (o similares) en los
sids, y reemplazarlas en runtime por una de NTSC.

    .. note:: No todas las tablas son iguales, pero si son muy
      parecidas. Por ejemplo, un "La" en la 8va octava puede que aparezca como
      ``$f820``, y en otras como ``$f830``, u algún otro valor. Pero el oído humano no
      las diferenciaría.

Lo mejor es buscar por ``$01, $01, $01, $01, $02, $02, $02`` y ver si
tiene pinta de ser la tabla "hi". Y luego ir 96 bytes para arriba o
abajo y ver si ahí esta la tabla "low".

.. figure:: https://lh3.googleusercontent.com/VqNAXgS2DOrbG7bJ729Fz3VWCjzkvTjH_DhtBnZeuL0iIszlmQdtWAnS8qEdBi5FX-fcFL9wfe7hAp0UHkWfmKDCQab5GokBc4vsL6IVRIDMWQdDdezC5bm7I9m2D5d8P8Lph08
   :alt: buscando tabla

   *Buscando la tabla de frecuencias en un sid*

Y una vez que se encuentren los valores, se reemplazan por los valores
de una NTSC. Acá no hay que hacer más que un simple bucle para copiar
las tablas. Ej:

.. code:: asm

        ; actualiza tabla de frecuencias
        ldx #95
    @l0:
        lda ntsc_freq_table_hi,x
        sta dst_hi,x

        lda ntsc_freq_table_lo,x
        sta dst_lo,x
        bpl @l0

    ntsc_freq_table_lo:
    .byte $0c,$1c,$2d,$3f,$52,$66,$7b,$92,$aa,$c3,$de,$fa  ; 1
    .byte $18,$38,$5a,$7e,$a4,$cc,$f7,$24,$54,$86,$bc,$f5  ; 2
    .byte $31,$71,$b4,$fc,$48,$98,$ed,$48,$a7,$0c,$78,$e9  ; 3
    .byte $62,$e2,$69,$f8,$90,$30,$db,$8f,$4e,$19,$f0,$d3  ; 4
    .byte $c4,$c3,$d1,$f0,$1f,$61,$b6,$1e,$9d,$32,$df,$a6  ; 5
    .byte $88,$86,$a3,$e0,$3f,$c2,$6b,$3d,$3a,$64,$be,$4c  ; 6
    .byte $0f,$0c,$46,$bf,$7d,$84,$d6,$7a,$73,$c8,$7d,$97  ; 7
    .byte $1e,$18,$8b,$7f,$fb,$07,$ac,$f4,$e7,$8f,$f9,$2f  ; 8

    ntsc_freq_table_hi:
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ; 1
    .byte $02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03  ; 2
    .byte $04,$04,$04,$04,$05,$05,$05,$06,$06,$07,$07,$07  ; 3
    .byte $08,$08,$09,$09,$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f  ; 4
    .byte $10,$11,$12,$13,$15,$16,$17,$19,$1a,$1c,$1d,$1f  ; 5
    .byte $21,$23,$25,$27,$2a,$2c,$2f,$32,$35,$38,$3b,$3f  ; 6
    .byte $43,$47,$4b,$4f,$54,$59,$5e,$64,$6a,$70,$77,$7e  ; 7
    .byte $86,$8e,$96,$9f,$a8,$b3,$bd,$c8,$d4,$e1,$ee,$fd  ; 8

Interrupciones, Timers y Raster
-------------------------------

La otra cosa a tener en cuenta, es la velocidad con la que se toca el
sid. Muchos trackers generan sids que se tocan a 50.125Hz (velocidad
PAL). Es lo ideal. Pero no todos son así. Así que hay que doble chequear
eso (ej: SidTracker64).

Y para hacer que algo funcione a cierta velocidad en la C64, hay dos
maneras:

-  Con interrupciones raster
-  Y/o con interrupciones de timer

Básicamente las interrupciones son "callbacks" que nos llaman cuando
sucede algo. Estos "callbacks" son programables: se puede activar o
desactivar.

Raster
~~~~~~

La interrupción por raster es la más común. Uno le dice a la C64 que lo
llame cuando el raster esta en cierto rasterline.

Por ejemplo, si yo quisiera que el borde de la pantalla fuese negro en
la parte de arriba, y blanco en la de abajo, se usan dos interrupciones
de raster encadenadas. Es así:

.. code:: asm

    setup_irq:
        sei
        ldx #<raster_top        ; dirección de nuestro callback (IRQ)
        ldy #>raster_top
        stx $0314               ; IRQ vector lo
        sty $0315               ; IRQ vector hi

        lda #0
        sta $d012               ; disparar raster cuando rasterline sea 0

        lda #1
        sta $d01a               ; habilitar interrupción por raster

        cli
        rts

    raster_top:
        asl $d019               ; ACK interrupción de raster

        lda #0                  ; poner borde
        sta $d020               ; con color negro (0=negro)

        lda #100                ; encadenar 2nd callback
        sta $d012               ; que se dispare cuando rasterline sea 100

        ldx #<raster_bottom
        ldy #>raster_bottom
        stx $0314
        sty $0315

        jmp $ea81               ; salir de la interrupción

    raster_bottom:
        asl $d019               ; ACK interrupción de raster

        lda #1                  ; poner borde
        sta $d020               ; con color blanco (1=blanco)

        lda #0                  ; encadenar al primer callback
        sta $d012               ; que se dispare cuando rasterline sea 0

        ldx #<raster_top
        ldy #>raster_top
        stx $0314
        sty $0315

        jmp $ea81               ; salir de la interrupción

Y así uno puede encadenar varias interrupciones de raster. Lo importante
acá es:

-  el vector `$0314/$0315`_ contiene la dirección del callback (IRQ)
-  ACK (limpiar/aceptar) `$d019`_ cuando nos llamen en la interrupción
-  habilitar interrupción de raster con `$d01a`_
-  usar `$d012`_ para decir en que rasterline se tiene que disparar la interrupción
-  salir de la interrupción con un ``jmp`` a `$ea81`_ o `$ea31`_
-  el color del borde se cambia con `$d020`_. usar `$d021`_ para color de fondo de pantalla

Timers
~~~~~~

Las interrupciones por timer funcionan muy similar a las del raster.
Pero en vez de llamarnos cuando el rasterline tiene cierto valor, nos
llaman cuando transcurren cierta cantidad de ciclos de CPU.

La manera de usarlas es muy similar. ej:

.. code:: asm

    setup_irq:
        sei
        ldx #<timer_top        ; dirección de nuestro callback (IRQ)
        ldy #>timer_top
        stx $0314               ; IRQ vector lo
        sty $0315               ; IRQ vector hi

        ldx #$c7                ; CIA 1 - Timer A que se dispare
        ldy #$4c                ; luego de $4cc8 ciclos (se pone uno menos.)
                                ; ej: usar $4cc7 para contar $4cc8 ciclos
        stx $dc04
        sty $dc05

        lda #$81
        sta $dc0d               ; prender interrupciones del CIA 1

        lda #$11
        sta $dc0e               ; prender timer A

        cli
        rts

    timer_top:
        lda $dc0d               ; ACK interrupción de Timer

        jsr $1003               ; tocar música

        jmp $ea81               ; salir de la interrupción

-  `$dc0e`_ se usa para activar el Timer A. Puede ser "single-shot" o "continue"
-  `$dc0d`_ se usa para habilitar interrupciones del CIA 1
-  `$dc04`_ / `$dc05`_ se usa para decirle cuantos ciclos tiene que contar
   antes de disparar el callback (IRQ)

Y así es como se usan las interrupciones. De hecho se pueden usar
interrupciones de raster y de timer a la vez. Ambas comparten el mismo
callback. Para diferenciar si es de raster o timer se puede hacer algo
así:

.. code:: asm

    irq:
            asl $d019                       ; ACK interrupción de raster
            bcs raster                      ; y Carry estará prendido si la interrupción
                                            ; fue de raster

            lda $dc0d                       ; ACK interrupción de timer
            jsr $1003                       ; ej: tocar música con el timer interrupt
            jmp end

    raster:
            jsr animar_scroll               ; ej: animar scroll con el raster interrupt

    end:
            jmp $ea81

Timers para el Sid
------------------

Ahora que ya sabemos usar los timers, veamos como se usan para tocar un
sid a la velocidad correcta tanto en las distintas plataformas.

Asumiendo que el sid fue generado para PAL, la formulita para convertir
a NTSC es:

-  ``((velocidad_del_timer + 1) * 1022727 / 985248) - 1``

Y para convertir a Drean es similar:

-  ``((velocidad_del_timer + 1) * 1023440 / 985248) - 1``

..

    .. note:: ``985248``, ``1022727``, ``1023440`` son la velocidades del 6510
      en una PAL, NTSC, Drean respectivamente (``0.985248`` Mhz, ``1.022727``
      Mhz, ``1.023440`` Mhz). Como ven, la más rápida de todas es la Drean, y
      la más lenta es la PAL.

Para saber la velocidad del timer, hay que fijarse en el código del sid
y ver si modifica los valores del timer CIA. Por ejemplo, si ven algo
así:

.. code:: asm

        ldx #$c7            ; store $4cc7 in Timer A - CIA 1
        ldy #$4c            ; $4cc7 is on tick per refresh in PAL
        stx $dc04           ; Timer A lo
        sty $dc05           ; Timer A hi

Si el sid esta usando ``$4cc7`` en el timer (un 'tick' por refresco de
pantalla en PAL), entonces el nuevo valor del timer para NTSC será:

-  ``($4cc7 + 1) * 1022727 / 985248 - 1 = $4fb2``

El ``+1`` es porque el timer espera "cantidad de ciclos - 1".

.. code:: asm

        ldx #$b2            ; store $4fb2 in Timer A - CIA 1
        ldy #$4f            ; $4fb2 sets correct speed for NTSC
        stx $dc04           ; Timer A lo
        sty $dc05           ; Timer A hi

Y el valor para Drean es: ``$4fc1``.

Como ven las velocidades de Drean y NTSC son muy parecidas. De hecho las
tablas de frecuencias son muy parecidas entre sí también.

En el caso del Player, y dado que no teníamos memoria libre, Drean y
NTSC usan las mismas tabla de frecuencias.

Detectando entre PAL, NTSC y Drean
----------------------------------

La otra cosa importante es como detectar si una máquina es Drean, NTSC o
PAL.

El truquito es el siguiente. Cada una de estas máquinas tiene un
resolución de pantalla diferente:

-  PAL: 312 x 63
-  NTSC: 263 x 65
-  Drean: 312 x 65

Eso esta medido en ciclos de CPU. En una PAL, refrescar toda la pantalla
se tardan 312 x 63 = 19,656 ($4cc8) ciclos. ¿Les suena el número
``$4cc8``?. Es el que usamos en el timer para tocar la música a
velocidad PAL (``$4cc8 - 1``, ya que en los timers hay que restar 1 al
valor que uno quiere). Eso significa que si yo pongo un timer a
``$4cc7``, en una PAL va a llamarse una vez por refresco de pantalla.

La otra cosa a saber, es que uno puede saber en que rasterline se
encuentra el raster. Por las dudas, el raster es el haz de luz que barre
la pantalla de izquierda a derecha, de arriba hacia abajo.

Uniendo estas dos cosas, uno puede saber si la máquina es PAL, Drean o
NTSC.

El truquito es así:

-  Espero a que el raster este en la linea 0 (hay que leer `$d012`_)
-  Una vez que esta ahí, disparo el timer CIA con ``$4cc7``
-  Cuando el timer me llame, habrá dado justo una vuelta entera y `$d012`_
   valdrá 0. Al menos en PAL

¿Pero que valor debería tener para una NTSC?

La NTSC tiene una resolución de 263 \* 65. O sea, 17095 ciclos se
necesitan para recorrerla entera. Si el timer esta seteado para 19656
ciclos, entonces hay un "overflow" de:

-  19656 - 17095 = 2561 ciclos

Y como el NTSC tiene 65 ciclos por linea, si divido ese valor por 65, me
da:

-  2561 ciclos / 65 ciclos = 39.4.

O sea, que el raster, luego de 19656 ciclos, habrá dado una vuelta
entera y estará en el rasterline 39. Y una cuenta similar hay que hacer
para Drean (ejercicio para el lector).

El código que lo detecta PAL/NTSC/Drean es el siguiente:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; char ut_detect_pal_paln_ntsc(void)
    ;------------------------------------------------------------------------------;
    ; Cuenta cuantas rasterlines se dibujan en 312*63 (19656) ciclos
    ; 312*63-1 es usado en el Timer del CIA, porque es lo que espero el Timer (uno menos)
    ;
    ; En PAL,      (312 * 63)  19656/63 = 312  -> 312 % 312   (00, $00)
    ; En PAL-N,    (312 * 65)  19656/65 = 302  -> 302 % 312   (46, $2e)
    ; En NTSC,     (263 * 65)  19656/65 = 302  -> 302 % 263   (39, $27)
    ; En NTSC Old, (262 * 64)  19656/64 = 307  -> 307 % 262   (45, $2d)
    ;
    ; Return values:
    ;   $01 --> PAL
    ;   $2F --> PAL-N (Drean)
    ;   $28 --> NTSC
    ;   $2e --> NTSC-OLD
    ;
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ut_detect_pal_paln_ntsc:
            sei                             ; inhabilitar interrupciones

            lda #0
            sta $d011                       ; apagar pantalla para inhabilitar badlines

    :       lda $d012                       ; esperar a que el raster llegue a la rasterline 0 (más estable)
    :       cmp $d012
            beq :-
            bmi :--

            lda #$00
            sta $dc0e                       ; parar Timer A

            lda #$00
            sta $d01a                       ; inhabilitar raster IRQ
            lda #$7f
            sta $dc0d                       ; inhabilitar Timer en CIA 1
            sta $dd0d                       ; y CIA 2


            lda #$00
            sta sync

            ldx #<(312*63-1)                ; poner timer para PAL
            ldy #>(312*63-1)
            stx $dc04                       ; Timer A lo
            sty $dc05                       ; Timer A hi

            lda #%00001001                  ; one-shot
            sta $dc0e

            ldx #<timer_irq
            ldy #>timer_irq
            stx $fffe                       ; como el BASIC/KERNAL están apagados
            sty $ffff                       ; se usa $fffe/$ffff en vez de $0314/$0315

            asl $d019                       ; ACK interrupción de raster
            lda $dc0d                       ; ACK interrupción Timer en CIA 1
            lda $dd0d                       ; y CIA 2

            lda #$81
            sta $dc0d                       ; habilitar interrupción timer A
            cli                             ; CIA 1

    :       lda sync
            beq :-

            lda #$1b                        ; habilitar pantalla otra vez
            sta $d011
            lda ZP_VIC_VIDEO_TYPE           ; valor de retorno
            rts

    timer_irq:
            pha                             ; guardar "A"

            lda $dc0d                       ; ACK interrupción de Timer

            lda $d012
            sta ZP_VIC_VIDEO_TYPE

            inc sync
            cli

            pla                             ; restaurar "A"
            rti                             ; restaurar "PC" y "Status"

    sync:  .byte $00

Con esto ya deberíamos poder tocar sids en cualquier máquina de manera
correcta.

Actualizar nombre de canción / autor
------------------------------------

Quizás la parte las tediosa de todo el Player es actualizar los
nombres de la canción y autor. Veamos porque:

El modo bitmap funciona por celdas. La pantalla esta dividida en:

-  40 x 25 celdas
-  Cada celda es de 8x8 pixels (8 bytes)
-  Cada celda no puede tener más de 2 colores

.. figure:: https://lh3.googleusercontent.com/W9abCQZfIhLIFlxyodyd5BhMr0JioeCj9SSTgwhjkqfB0KH1J8PEta4SsS_tq7w8GiEXaOY0WFuobe1ngDv3vBwjgLs3MJMa5cpFkBjdFfbnC8AP6umui1-s8R0H8urtX1WG7_c
   :alt: cells

   *En modo Bitmap Standard las celdas no pueden tener más de 2 colores a la vez*

El gráfico en total usa 16 colores. Pero si prestan atención, cada celda
no tiene más de 2 colores a la vez. Este modo gráfico existe para
ahorrar memoria. Por ejemplo, si uno pudiera elegir 16 colores (4 bits)
por pixel, entonces el gráfico ocuparía:

-  (320 \* 200 \* 4 bits) / 8 = 32000 bytes.

Algo muy caro para una computadora de 64k RAM. Sumado además a que el
VIC no puede ver más de 16k a al vez. Sumado a que en si uno usa
BASIC solo tiene 38k libres. O sea, este modo gráfico no existe en la
C64.

Al usar celdas, el color de "foreground" y "background" se almacena en
un buffer de 40 x 25. Cada byte representa el color de la celda: los 4
bits altos son "foreground", y los 4 bits bajos, son el "background". De
esta manera, una gráfico bitmap + color ocupa:

-  ((320 \* 200 \* 1 bit) / 8) + (40 \* 25) = 9000 bytes.

Y 9000 bytes es algo aceptable para una máquina de 64k RAM.

Para prender un pixel en x,y y ponerle color se hace esto:

.. code:: c

    // pseudo código
    void set_pixel(int x, int y)
    {
            // x va de 0 a 319
            // y va de 0 a 199

            // conseguir la celda correspondiente
            int cell_offset = 40 * (y / 8) + (x / 8);

            // dentro de esa celda, buscar el byte correspondiente
            int byte_offset = y % 8;

            // dentro de ese byte, buscar el bit correspondiente
            int bit_offset = x % 8;

            bitmap[cell_offset + byte_offset] |= bit_offset;
    }

    void set_cell_color(int x, int y, int foreground, int background)
    {
            // x va de 0 a 39
            // y va de 0 a 24

            offset = y * 40 + x;
            color = (foreground << 4 | background);

            screen_ram[offset] = color;
    }

Ahora que sabemos prender (y apagar) un pixel, lo que tenemos que hacer
es que las letras se dibujen en "diagonal". Si vemos el gráfico
nuevamente vemos que tiene una inclinación de:

-  vertical: de 1 x 1. recta: ``Y = -X``. Pendiente de -1
-  horizontal: de 2 x 1. recta: ``Y = X/2``. Pendiente de 0.5

.. figure:: https://lh3.googleusercontent.com/TpaSLAM6xyEgB80FWG8R8QsEKmNvBfuTrYpy8bwkECpVF4dtFZs3NqCkKw98dC-PzjtZMu3-ZKEC5Fs3wsyI1aatB9z0r5MyStkOsJOU0gj2SNlNIld4ztQdSXXq6SipWNktL2k
   :alt: inclinación

   *Inclinación que se quiere buscar*

Básicamente, lo que queremos lograr es algo así:

.. figure:: https://lh3.googleusercontent.com/j-TXraycC52OgY3wO-9OTl2wf6X0q1F3jmr5ygvRwJ-NFfd99OicecuzuUa1viUYF3nWsCighJtpFf0QXqXyTpcNY0HWgakFwZ43-jjrcvfx5UYty7IL4T-hMvk6cjprPMxf5LU
   :alt: resultado

   *Ejemplo de como tiene que ser la inclinación de las letras*

El algoritmo a dibujar las letras sería algo así:

.. code:: c

    //pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // empezar desde la celda 14 horizontal
        int offset_pixel_y = 3 * 8;     // empezar desde la celda 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char(name[i], x, y);
            x += 8;                     // siguiente char empieza: 8 pixels a la derecha
            y += 4;                     // y 4 pixels más abajo
        }
    }

Pero lo difícil es implementar ``plot_char()``. Si no tuviésemos que
inclinar el char, la solución sería más o menos así:

.. code:: c

    // pseudo code
    void plot_char_normal(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // cada char ocupa 8 bytes.

        for (int y=0; y<8; y++)
        {
            for (int x=0; x<8; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x, offset_y + y);
                else
                    clear_pixel(offset_x + x, offset_y + y);
            }
        }
    }

Pero lo que queremos hacer es imprimirlo inclinado. La solución es
similar, pero cada tanto tenemos que bajar e ir a la izquierda:

.. code:: c

    // pseudo code
    void plot_char_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // cada char ocupa 8 bytes.

        // fix_x/fix_y son las que van a dar el efecto de inclinación
        int fix_x = 0;
        int fix_y = 0;

        // iterar por sobre todos los pixels del char
        for (int y=0; y<8; y++)
        {
            for (int x=0; x<8; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y + fix_y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y + fix_y);

                // bajar un pixel (Y) por cada dos pixels horizontales (X)
                fix_y = x/2;
            }
            // la siguiente fila tiene que empezar un pixel a la izquierda
            fix_x--;
        }
    }

Con ese algoritmo podemos imprimir cosas de este estilo:

.. figure:: https://lh3.googleusercontent.com/_egTNJbWjoF0tImd_bbporzfdvE9Vp74q3gIM2ezwOWU4GRYUeLZzWeGJMk6vM4vPHnGC_Tqqtxmiz5HQMHSBRoiAtADyQtZyapK1bQFKFCJA1nl2iIoChVXAujdJ6LSvSq5AHg
   :alt: inclinada\_fat

   *Las letras tienen pixeles vacíos en el medio*

Pero **no** es lo que queremos ya que:

-  Ocupa mucho lugar de pantalla, no van a entrar los nombres de las
   canciones
-  Hay pixeles vacíos en el medio de las letras, no se ve prolijo

¿Y por qué hay pixeles vacíos? La respuesta esta en ver esta rotación:

.. figure:: https://lh3.googleusercontent.com/K4ylCjj6GgzdI9DEhTjikkcc14C_bnQEHCBk1OvXtOh3ReUK28f0vTnyGnyu6Q1x67mLLNw5qUuec_CtAWUztv-5wFeDvf7LKpq2-KDqtn_qw93OUAQmhNGKJU0pKg8QpQc6N-U
   :alt: rotado

   *El porqué de los pixeles vacíos*

El algoritmo hace lo que le dijimos que haga, pero no es lo que
queremos. Lo primero a hacer, es usar fonts de 4x8 (y no de 8x8) para
que no ocupe tanto espacio de pantalla. Lo segundo es arreglar los
pixeles vacíos.

Una posible solución para evitar los pixeles vacíos es que el algoritmo
incline los chars de manera horizontal, y no vertical. Algo así como
esto:

.. figure:: https://lh3.googleusercontent.com/gcnEulu7AuMlM2TmwusHLe5-iS3UqUVeTJnHFhKT9d_9JjqdCG7_nFijuyWpQKHzGVeTGfXlbbF-mOi_Y-TRxyuTs1H-xy-BUqfz55rMitmiSJApwRI5M_BTRTzDR47oRk1_iw8
   :alt: rotado2

   *Alternativa para evitar los pixeles vacíos*

Y cuatro letras se verían así:

.. figure:: https://lh3.googleusercontent.com/ViP4RjGdqlvh1B55Q4laIg2S95S6DivApYRuGMOKpK3LnukRebGh410rSkSc5hLb12fu24FMeHuDILaAozN-UK7WX6QgCGqFZZXcKAQ6rC2idlGnCbqJY4Sr9_MPiUCWKScE4Q0
   :alt: rotado3

   *Los pixeles vacíos estan al final de cada letra*

Lo que estamos haciendo, es que los pixeles vacíos estén como
"separadores" de los caracteres, y no en el medio de cada caracter. Con
esto en mente, el nuevo algoritmo es así:

.. code:: c

    // pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // empezar desde la celda 14 horizontal
        int offset_pixel_y = 3 * 8;     // empezar desde la celda 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char_semi_inclinado(name[i], x, y);
            x += 4;                     // siguiente char empieza: 4 pixels a la derecha
            y += 2;                     // y 2 pixels más abajo
        }
    }

    void plot_char_semi_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // cada char ocupa 8 bytes.

        // fix_x da efecto de inclinación en X
        int fix_x = 0;

        // iterar por sobre todos los pixels del char
        for (int y=0; y<8; y++)
        {
            // de 0 a 4, ya que el char ahora ocupa la mitad
            for (int x=0; x<4; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y);
            }
            // la siguiente fila tiene que empezar un pixel a la izquierda
            fix_x--;
        }
    }

Lo que hay que hacer ahora, es tener un charset [#]_ que cuando se incline
solo horizontalmente, se vea como queremos. Por ejemplo, un charset como
este:

.. figure:: https://lh3.googleusercontent.com/bEDUkJFBU44Uc6vjfmyCPDHVO3jrSTvW0SQzBSoYsQkwuZ7Q1ij8Gl0K6SBfm0LyD8yg6ZaEHsOsJqAgpd2g0CUZUZ1Wvowg72MaX9JjW7GZ058yNLQrtgURQ7NyFOe7RhYbwmI
   :alt: charset

   *Charset completo con letras listas para ser inclinadas*

Y así se ven algunas de las letras inclinadas:

.. figure:: https://lh3.googleusercontent.com/K2eFlXjp7iAn72AjmoREX7GsKBPSxmnSi6s02-fFhtfw0JZhdNG1EnyGPJG_KEYPS6T5pBR3ZhmEaeTsH-7dyogYnlm-J7oFN6gjcYB9k_VeY0UJs8Yy0cES7uGD_NMaLhMFTxk
   :alt: charset\_rotado

   *Ejemplo de como se ven 'a', 'b', 'c' y 'd'*

Pero aún queda por solucionar las letras anchas como ``m``, ``M``, ``W``
y ``w``. Eso se soluciona usando dos chars para esas letras y haciendo
que las letras ocupen 8x8 y no 4x8. Sería así:

.. figure:: https://lh3.googleusercontent.com/5fnDgzMLnIjb6wNdSE-WdqTxR1lvl42si2gr57JpF_fXMd5J7g0SrG6yuCjTV9TLjMq-gJOvHk4kTEIIPvhGVzybZgPbSUz9PtkdIty4QYurb_gF6rGc40XLvrDFzeZJlAuP1Wc
   :alt: m\_rotada

   *Componiendo la M*

Entonces, el algoritmo final es:

-  Se usa un charset de 8x8. Pero la mayoría de las letras son de 4x8.
   La parte derecha de la mayoría de las letras esta vacía
-  Se copian los 8x8 pixels de las letras usando el algoritmo de
   ``semi_inclinación``
-  Ciertas letras como la ``m`` y ``w`` van a usar dos caracteres. Ej:
   ``mama`` es escrito como ``m&am&a``, ya que el char ``&`` tendrá la
   parte que le falta a la ``m``

Entonces, el código queda bastante sencillo, lo cual es bueno (menos
bugs), pero se pone más esfuerzo en la data. Pero es 10 veces mejor
tener código sencillo y data compleja, que al revés.

Algoritmo final para imprimir la letras inclinadas:

.. code:: c

    // pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // empezar desde la celda 14 horizontal
        int offset_pixel_y = 3 * 8;     // empezar desde la celda 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char_semi_inclinado(name[i], x, y);
            x += 4;                     // siguiente char empieza: 4 pixels a la derecha
            y += 2;                     // y 2 pixels más abajo
        }
    }

    void plot_char_semi_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // cada char ocupa 8 bytes.

        // fix_x da efecto de inclinación en X
        int fix_x = 0;

        // iterar por sobre todos los pixels del char
        for (int y=0; y<8; y++)
        {
            // de 0 a 8. Se copia el char entero
            for (int x=0; x<8; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y);
            }
            // la siguiente fila tiene que empezar un pixel a la izquierda
            fix_x--;
        }
    }

Versión Optimizada
~~~~~~~~~~~~~~~~~~

El algoritmo anterior funciona bien, pero el problema es que usa mucho
las multiplicaciones en ``set_pixel()`` [#]_, y recordemos que el 6510
no tiene instrucciones de multiplicación.

Entonces el Player usa una versión un tanto más complicada para mejorar la
performance. Tiene en cuenta lo siguiente:

-  Los caracteres solo pueden empezar en los siguientes offsets
   relativos a las celdas: (0,0), (4,2), (0,4), (4,6)
-  Un caracter necesita dos celdas para imprimirse. Y estas celdas son
   contiguas.
-  El siguiente caracter a imprimir estará, como mucho, a una celda de
   distancia tanto en X como en Y
-  Hay funciones especificas para dibujar los posibles 4 offsets
   ``plot_char_0()``, ..., ``plot_char_3()``
-  Hay funciones especificas para dibujar cada una de las 8 filas:
   ``plot_row_0()``, ..., ``plot_row_7()``
-  Hay tres punteros globales:

   -  ``$f6/$f7`` offset del charset que apunta al caracter a imprimir
   -  ``$f8/f9``, y ``$fa/$fb`` que apuntan a la celda actual, y a la
      siguiente celda del bitmap

Con eso en mente, no hace falta calcular el offset de los pixeles por
cada pixel y eso ahorra CPU ya que no hay multiplicaciones de por medio.
Aunque agrega complejidad.

Así es como funciona el algoritmo optimizado (pseudo código):

.. code:: c

    // pseudo código

    // global: apunta al principio del bitmap
    #define ORIGIN_CELL_X = 14;
    #define ORIGIN_CELL_Y = 3;

    // en el código en assembler, estas dos variables están representadas
    // por `$f8/$f9` y `$fa/$fb`
    int g_bitmap_offset_0, g_bitmap_offset_1;

    void plot_name(char* name)
    {
        int l = strlen(name);
        int idx = 0;

        // inicializar bitmap offset con el origen de la celda
        g_bitmap_offset_0 = ORIGIN_CELL_Y * 40 + ORIGIN_CELL_X * 8;
        g_bitmap_offset_1 = ORIGIN_CELL_Y * 40 + (ORIGIN_CELL_X + 1) * 8;
        char c;

        while (no_se_hayan_impreso_todos_los_chars) {

            c = fetch_next_char();
            plot_char_0(c);     // print first char (offset 0,0)

            c = fetch_next_char();
            plot_char_1(c);     // print second char (offset 4,2)

            bitmap_next_x();    // celda_x++ (actualiza g_bitmap_offsets)

            c = fetch_next_char();
            plot_char_2(c);     // print third char (offset 0,4)

            c = fetch_next_char();
            plot_char_3(c);     // imprime cuarto char (offset 4,6)

            bitmap_next_x();    // celda_x++ (actualiza g_bitmap_offsets)
            bitmap_next_y();    // celda_y++ (actualiza g_bitmap_offsets)
        }
    }

    // prints char at offset 0,0
    void plot_char_0(char* char_data)
    {
        plot_row_0(char_data[0]);

        bitmap_prev_x();        // celda_x-- (actualiza g_bitmap_offsets)

        plot_row_1(char_data[1]);
        plot_row_2(char_data[2]);
        plot_row_3(char_data[3]);
        plot_row_4(char_data[4]);
        plot_row_5(char_data[5]);
        plot_row_6(char_data[6]);
        plot_row_7(char_data[7]);

        // restore pointer
        bitmap_next_x();
    }

    // prints char at offset 4,2
    void plot_char_1(char* char_data)
    {
        plot_row_2(char_data[0]);
        plot_row_3(char_data[1]);
        plot_row_4(char_data[2]);
        plot_row_5(char_data[3]);
        plot_row_6(char_data[4]);

        bitmap_prev_x();        // celda_x-- (actualiza g_bitmap_offsets)

        plot_row_7(char_data[5]);

        bitmap_next_y();        // celda_y++ (actualiza g_bitmap_offsets)

        plot_row_0(char_data[6]);
        plot_row_1(char_data[7]);

        // restore pointers
        bitmap_next_x();
        bitmap_prev_y();
    }

    void plot_char_2(char* char_data)
    {
        // y así sucesivamente hasta el plot_char_3()
        ...
    }

    void plot_row_0(char c)
    {
        g_bitmap[g_bitmap_offset_0] = c;
    }

    void plot_row_1(char c)
    {
        rotate_left(c, 1);              // caracter es rotado un lugar a la izquierda

        // actualizo celda izquierda
        char value_izq = g_bitmap[g_bitmap_offset_0];
        value_izq &= 0b11111110;        // apago el 1er bit LSB
        value_izq |= (c & 0b00000001);  // pongo lo que este en el 1er bit LSB del char
        g_bitmap[g_bitmap_offset_0] = value_izq;

        // actualizo celda derecha
        char value_der = g_bitmap[g_bitmap_offset_1];
        value_der &= 0b00000001;        // apago los 7 primeros bit MSB
        value_der |= (c & 0b11111110);  // pongo lo que este en los primeros 7 bit MSB del char
        g_bitmap[g_bitmap_offset_1] = value_der;
    }

    void plot_row_2(char c)
    {
        rotate_left(c, 2);              // caracter es rotado dos lugares a la izquierda

        // actualizo celda izquierda
        char value_izq = g_bitmap[g_bitmap_offset_0];
        value_izq &= 0b11111100;        // apago los dos bit LSB
        value_izq |= (c & 0b00000011);  // pongo lo que este en los dos bit LSB del char
        g_bitmap[g_bitmap_offset_0] = value_izq;

        // actualizo celda derecha
        char value_der = g_bitmap[g_bitmap_offset_1];
        value_der &= 0b00000011;        // apago los 6 primeros bit MSB
        value_der |= (c & 0b11111100);  // pongo lo que este en los primeros 6 bit MSB del char
        g_bitmap[g_bitmap_offset_1] = value_der;
    }

    void plot_row_3(char c)
    {
        // y así sucesivamente hasta el plot_row_7()
        ...
    }

Y eso mismo (más/menos algunos cambios) es como esta hecho el Player,
pero en assembler. Con esto se logra que no se hagan multiplicaciones.

Para los que quieran ver el código en completo en assembler, esta acá:

-  `plotter en assembler <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/src/chipdisk.s#L1313>`__

Algunos truquitos que usamos en el plotter:

Truquito: Macros y demás
^^^^^^^^^^^^^^^^^^^^^^^^

Usamos macros y algunos *Pseudo Functions* [#]_ como ``.IDENT``, ``.CONCAT``
para simplificar el código. Veamos como se usan:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Macros
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; entry:
    ;   number_of_rows: cuantas filas a imprimir
    ;   char_y_offset: offset del char a imprimir
    ;   cell_y_offset: offset de la celda Y
    ;   cell_x_offset: offset de la celda X. Usada para llamar a plot_row_xxx
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    .macro PLOT_ROWS number_of_rows, char_y_offset, cell_y_offset, cell_x_offset
            .repeat number_of_rows, YY
                    ldy #char_y_offset + YY
                    lda ($f6),y                 ; $f6 apunta a la data del charset
                    ldy #cell_y_offset + YY
                    jsr .IDENT(.CONCAT("plot_row_", .STRING(cell_x_offset + YY)))
            .endrepeat
    .endmacro


    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; entry:
    ;       A = byte to plot
    ;       Y = bitmap offset
    ;       MUST NOT modify X
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    .macro PLOT_BYTE addr, mask
    .scope
            and #mask
            sta ora_addr
            lda (addr),y
            and # <(.BITNOT mask)
    ora_addr = *+1
            ora #0                          ; self modifying
            sta (addr),y
    .endscope
    .endmacro

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Funciones
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_char_0
    ; entry:
    ;       $f6,$f7: address of char from charset (8 bytes)
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_char_0:
            PLOT_ROWS 8, 0, 0, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset
            rts

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_char_1
    ; entry:
    ;       $f6,$f7: address of char from charset (8 bytes)
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_char_1:
            PLOT_ROWS 4, 0, 2, 4            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

            jsr bitmap_prev_x

            PLOT_ROWS 2, 4, 6, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

            jsr bitmap_next_y

            PLOT_ROWS 2, 6, 0, 2            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

            jsr bitmap_next_x               ; restore
            jsr bitmap_prev_y               ; restore

            rts

    plot_char_2:
            ; y así hasta el plot_char_3
            ...

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_row_0
    ; entry:
    ;       A = byte to plot
    ;       Y = bitmap offset
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_row_0:
            sta ($f8),y                 ; no tiene que rotar nada
            rts                         ; así que lo imprime directamente

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_row_1
    ; entry:
    ;       A = byte to plot
    ;       Y = bitmap offset
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_row_2:
            .repeat 1                       ; rota el caracter 1 posición
                    asl                     ; a la izquierda
                    adc #0
            .endrepeat

            tax                             ; save for next value
            PLOT_BYTE $f8, %00000001

            txa
            PLOT_BYTE $fa, %11111110

            rts

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_row_2
    ; entry:
    ;       A = byte to plot
    ;       Y = bitmap offset
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_row_2:
            .repeat 2                       ; rota el caracter 2 posiciones
                    asl                     ; a la izquierda
                    adc #0                  ; el "adc" pone a la derecha lo que salió
                                            ; por la izquierda
            .endrepeat

            tax                             ; save for next value
            PLOT_BYTE $f8, %00000011

            txa
            PLOT_BYTE $fa, %11111100

            rts

    plot_row_3:
            ; y así hasta el plot_row_7
            ...

Truquito: Rotar In-Place
^^^^^^^^^^^^^^^^^^^^^^^^

El truquito que usamos de rotar "in-place" [#]_ es simpático:

.. code:: asm

            asl                     ; rota todo un bit a la izquierda. "C" tiene el valor del bit 7.
            adc #0                  ; y el bit 0 tiene el valor del "C"

Truquito: Unrolled-loops
^^^^^^^^^^^^^^^^^^^^^^^^

Los *unrolled loops* se usan mucho dentro de juegos/demos/intros que quieren
lograr velocidad (a cambio de espacio en RAM):

Un *loop* normal es así:

.. code:: asm

                lda #$20                ; pone un $20 de $0400 a $04ff
                ldx #0
        l0:     sta $0400,x             ; tarda 5 ciclos, ocupa 3 bytes
                dex                     ; tarda 2 ciclos, ocupa 1 byte
                bne l0                  ; tarda 2 ciclos, ocupa 2 bytes

El loop se repite 256 veces, así el *loop* tarda (5 + 2 + 2) * 256 = 2304 ciclos
y ocupa 6 bytes.

Una manera de hacerlo mucho más rápido es con un *unrolled loop*. Así:

.. code:: asm

        lda #$20                        ; pone un $20 de $0400 a $04ff
        sta $0400                       ; tarda 4 ciclos, ocupa 3 bytes
        sta $0401                       ; tarda 4 ciclos, ocupa 3 bytes
        sta $0402                       ; tarda 4 ciclos, ocupa 3 bytes
        ...
        sta $04fe                       ; tarda 4 ciclos, ocupa 3 bytes
        sta $04ff                       ; tarda 4 ciclos, ocupa 3 bytes

De esta manera el *unrolled loop* tarda 4 * 256 = 1024 ciclos, pero ocupa
256 * 3 = 768 bytes.

Una manera más mantenible de escribir *unrolled loops* es, al menos con
cc65_, es así:

.. code:: asm

        lda #$20
        .repeat 256, XX
                sta $0400 + XX
        .endrepeat

Y van a ver que dentro del código del Chipdisk se usa mucho. Solo buscar por
``.repeat`` para que vean la cantidad de veces que se usa. Pero para ser honestos
no estoy seguro que el Chipdisk requiera de tantos *unrolled loops*.


Truquito: Sumar 320
^^^^^^^^^^^^^^^^^^^

La otra cosa a rescatar, es como funciona el ``bitmap_next_y()``. Lo que
hace es sumar ``320`` al puntero ``$f8/$f9``. Y como ``320 = 256 + 64``,
lo hace sumando 64 a ``$f8`` e incrementando en uno ``$f9``.

.. code:: asm

    bitmap_next_y:
            clc                             ; limpiar Carry para la suma
            lda $f8                         ;
            adc #64                         ; suma 64 a $f8 y guarda el carry
            sta $f8                         ; guarda el valor en $f8

            lda $f9                         ; incrementa $f9 en 1 + carry
            adc #1
            sta $f9                         ; guarda en valor en $f9


Leyendo mouse, joystick y teclado
---------------------------------

El Player soporta 3 métodos para controlar la "flechita":

-  Joystick en el port #2
-  Mouse en el port #1
-  Teclado

Joystick
~~~~~~~~

Leer el joystick es relativamente sencillo en la C64. Los valores del
joystick 1 están en `$dc01`_ y los del joystick 2 están en `$dc00`_

.. code:: asm

        ldx $dc00                       ; "X" tiene el valor del joystick #2
        ldy $dc01                       ; "Y" tiene el valor del joystick #1

Los posibles valores son:

+-----------+---------------------------------+
|$dc00/$dc01| Significado                     |
+===========+=================================+
| Bit  4    | Joystick Boton: 0 = Activo      |
+-----------+---------------------------------+
| Bit  3    | Joystick Derecha: 0 = Activo    |
+-----------+---------------------------------+
| Bit  2    | Joystick Izquierda: 0 = Activo  |
+-----------+---------------------------------+
| Bit  1    | Joystick Abajo: 0 = Activo      |
+-----------+---------------------------------+
| Bit  0    | Joystick Arriba: 0 = Activo     |
+-----------+---------------------------------+

Importante: 0 significa que esta prendido, y 1 apagado. Si uno quiere
chequear si el botón del Joystick 2 esta apretado, el código es:

.. code:: asm

        lda $dc00                       ; leer estado de Joystick 2
        and #%00010000                  ; solo me interesa el estado del botón
        beq boton_apretado              ; si es 0, entonces el botón esta apretado

Y algo similar para el Joystick 1, pero con `$dc01`_ en vez de `$dc00`_.

Teclado
~~~~~~~

El teclado es un poco más complicado... o no, depende de lo que uno
necesite. Hay una función del KERNAL que devuelve la tecla apretada: `$ffe4`_

.. code:: asm

        jsr $ffe4                       ; devuelve en A en byte leido del teclado

Y usar el KERNAL para esto esta más que bien para la mayoría de los
casos. El Player, sin embargo, usa el otra opción que es leyendo el
"hardware" directamente, y funciona así:

-  El teclado de la Commodore 64 tiene 64 teclas (sin contar RESTORE)
-  Y las teclas están ordenadas en una matriz de 8 x 8 (8 \* 8 = 64)
-  `$dc01`_ contiene los valores de las columnas
-  y `$dc00`_ contiene los valores del las filas

O sea, que uno puede saber que teclas están apretadas al leer la
siguiente matriz:

+---------------+--------------------------------------------------------------------------------+
|Matriz 8x8     |                                      $DC01                                     |
|del teclado    +---------+---------+---------+---------+---------+----------+---------+---------+
|               |  Bit 7  |  Bit 6  |  Bit 5  |  Bit 4  |  Bit 3  |  Bit 2   |  Bit 1  |  Bit 0  |
+=====+=========+=========+=========+=========+=========+=========+==========+=========+=========+
|     |**Bit 7**| RUN/STOP|    Q    |    C=   |  SPACE  |    2    |    CTRL  |    ←    |    1    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 6**|    /    |    ↑    |    =    | SHIFT-R | CLR/HOME|     ;    |    \*   |    £    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 5**|    ,    |    @    |    :    |    .    |   \-    |     L    |    P    |    \+   |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|$DC00|**Bit 4**|    N    |    O    |    K    |    M    |    0    |     J    |    I    |    9    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 3**|    V    |    U    |    H    |    B    |    8    |     G    |    Y    |    7    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 2**|    X    |    T    |    F    |    C    |    6    |     D    |    R    |    5    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 1**| SHIFT-L |    E    |    S    |    Z    |    4    |     A    |    W    |    3    |
|     +---------+---------+---------+---------+---------+---------+----------+---------+---------+
|     |**Bit 0**| UP/DOWN |    F5   |    F3   |    F1   |    F7   |LEFT/RIGHT|  RETURN | INST/DEL|
+-----+---------+---------+---------+---------+---------+---------+----------+---------+---------+

Si queremos saber si la tecla ``Q`` fue apretada entonces hay que hacer lo siguiente:

.. code:: asm

        lda #%01111111              ; fila 7
        sta $dc00
        lda $dc01
        and #%01000000              ; columna 6
        beq tecla_apretada          ; si vale 0, entonces fue apretada

Al igual que el joystick, un valor 0 indica que fue apretada, y un 1 que no.

    .. note:: Los joysticks y el teclado comparten el mismo controlador (CIA)
      por lo que diferenciar entre un movimiento de joystick y teclas apretadas a
      veces se complica. Notaran que ambos usan tanto `$dc00`_ como  `$dc01`_ para
      leer los datos.

Si queremos saber si el *cursor izquierda* esta apretado, entonces hay que
chequear si las teclas *Shift* y *cursor izquierda/derecha* están apretadas.
Para detectar eso, en el Player hacemos esto:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; read_keyboard
    ;
    ; chequea si cursor derecha o izquierda fueron apretado
    ;
    ; A = 0 nada fue apretado
    ; A = 1 cursor derecha fue apretado
    ; A = 2 cursor izquierda fue apretado
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    read_keyboard:
            ; IMPORTANTE: los bits están invertidos en el CIA (0 = on, 1 = off)

            NoKey    = 0
            LeftKey  = 1
            RightKey = 2


            ; Chequear por shift izquierdo
            lda #%11111101    ; file 2
            sta $dc00
            lda CIA1_PRB
            and #%10000000    ; col 7
            beq :+

            ; Chequear for shift derecho
            lda #%10111111    ; file 6
            sta $dc00
            lda CIA1_PRB
            and #%00010000    ; col 4
            beq :+
            lda #$ff          ; shift no apretados
    :       sta shift_on

            ; chequear por cursor izq/derecha
            lda #%11111110    ; file 0
            sta $dc00
            lda CIA1_PRB
            and #%00000100    ; col 2
            cmp keydown
            bne newkey
            lda #NoKey        ; No se apretó nada
            rts
    newkey:
            sta keydown
            lda keydown
            beq :+
            lda #NoKey        ; key up
            rts
    :       lda shift_on
            beq left
            lda #RightKey
            rts
    left:   lda #LeftKey
            rts

    keydown:
        .byte %00000100
    shift_on:
        .byte $ff  ; $ff = false, $00 = true

Mouse
~~~~~

El Player funciona con el mouse también. No es muy común usar mouse en
la C64, pero si uno tiene un Commodore 1351, lo puede usar. Controlar el
mouse no es tan complicado, pero tiene lo suyo.

Lo primero que hay que hacer, es decirle a la CIA que el puerto 1 (ó 2) se va
a usar para el mouse. Y luego se lee el *delta x* desde `$d419`_ y el *delta y*
desde `$d41a`_ (que son registros del chip de sonido).

El mouse se activa con `$dc00`_.

.. code:: asm

        lda #%01000000                  ; habilitar mouse
        sta $dc00                       ; en puerto 1

        ; luego de usar el mouse, se desactiva de la siguiente manera

        lda #%00111111                  ; habilitar joystick
        sta $dc00                       ; en puerto 1


Y esta es la rutina que usa el Player: lee los deltas, y chequea si el boton
fue apretado

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; read_mouse
    ;       exit    x = delta x movement
    ;               y = delta y movement
    ;               C = 0 if button pressed
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    read_mouse:
            lda $d419                       ; lee delta X (pot x)
            ldy opotx
            jsr mouse_move_check            ; calcular delta
            sty opotx
            sta ret_x_value

            lda $d41a                       ; leer delta Y (pot y)
            ldy opoty
            jsr mouse_move_check            ; calcular delta
            sty opoty

            eor #$ff                        ; delta esta invertido... arreglarlo
            tay
            iny

            sec                             ; C = 1 (significa botón no apretado)

    ret_x_value = * + 1
            ldx #00                         ; self modifying

            lda $dc01                       ; leer botón joy #1 : bit 4
            asl
            asl
            asl
            asl                             ; C = 0 (significa botón apretado)
            rts

    opotx: .byte $00
    opoty: .byte $00

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; mouse_move_check
    ; Taken from here:
    ; https://github.com/cc65/cc65/blob/master/libsrc/c64/mou/c64-1351.s
    ;
    ;       entry   y = old value of pot register
    ;               a = current value of pot register
    ;       exit    y = value to use for old value
    ;               x,a = delta value for position
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    mouse_move_check:
            sty     old_value
            sta     new_value
            ldx     #$00

            sec
            sbc     old_value               ; a = mod64 (new - old)
            and     #%01111111
            cmp     #%01000000              ; if (a > 0)
            bcs     @L1                     ;
            lsr     a                       ;   a /= 2;
            beq     @L2                     ;   if (a != 0)
            ldy     new_value               ;     y = NewValue
            rts                             ;   return

    @L1:    ora     #%11000000              ; else or in high order bits
            cmp     #$ff                    ; if (a != -1)
            beq     @L2
            sec
            ror     a                       ;   a /= 2
            dex                             ;   high byte = -1 (X = $FF)
            ldy     new_value
            rts

    @L2:    txa                             ; A = $00
            rts

    old_value: .byte 0
    new_value: .byte 0

Y para entender mejor como se habilita/inhabilita el mouse/joystick
así es como funciona el ``main_loop()`` del Player:

.. code:: asm

    main_loop:
        ...

        lda #%01000000                  ; habilitar mouse
        sta $dc00                       ; (inhabilita joystick)

        jsr read_mouse
        jsr process_mouse

        jsr read_keyboard
        jsr process_keyboard

        lda #%00111111                  ; habilitar joystick
        sta $dc00                       ; (inhabilita el mouse)

        jsr read_joystick
        jsr process_joystick

        ...
        jmp main_loop




Animar botones apretados
------------------------

Acá no estamos haciendo nada raro. Simplemente reemplazamos un pedazo del bitmap
por otro.

.. figure:: https://lh3.googleusercontent.com/gGQcvRrOcIv8tWfcliz_qTAveG2UALJxt9JYd-3JjOKYBzqM9FBiZ0U6nZMknEQt-87LYgH-H_OVP-V_HlMEr4W93M4H1WHOXkL2atCm5TePAqrK2s8CGaXHBg6apUN75M1xnzA
   :alt: celda de 7x7

   *Se copia un bloque de 7x7 celdas*

El algoritmo es más o menos así:

1.  El botón que esta apretado (si es que hay alguno) se reemplaza por el contenido del buffer temporal
2.  Se copia al buffer temporal el contenido del boton a apretar
3.  Se copia el contenido del botón apretado a destino

Lo que se copia es un bloque de 7x7 de cada botón. Se copia tanto el bitmap
como el color. Cada botón ocupa:

- bitmap: 7 * 7 * 8 (8 bytes por celda) + color: 7 * 7 = 441 bytes

Son 4 botones lo que animamos: *Play*, *FF*, *Rew* y *Stop*, y usamos un buffer temporal.
Así que en total usamos 441 * 5 (2205) bytes de data para esto.

El código en assembler esta hecho con macros:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; BUTTON_IMAGE_COPY
    ;
    ; Copy button (7x7 block) bitmap and colormap to Screen RAM and Color RAM
    ; respectively, from source address.  Source address must point to the start of
    ; the bitmap data, and its colormap must follow.
    ;
    ; If from_screen is not blank, data from screen is copied to src.
    ;
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    .macro BUTTON_IMAGE_COPY   src, pos_x, pos_y, from_screen
            Width  = 7
            Height = 7

            ScreenRAM = $4000
            ScreenSrc  = src
            ScreenDest = ScreenRAM + (pos_y * 40 * 8) + (pos_x * 8)

            ColorRAM  = $6000
            ColorSrc  = src + (Width * Height * 8)
            ColorDest = ColorRAM + (pos_y * 40) + pos_x

    .repeat Height, YY
            ;; Copy bitmap
            ldx #(Width*8-1)
    .ifblank from_screen
    :       lda ScreenSrc  + (YY * (Width * 8)), x
            sta ScreenDest + (YY * (40 * 8)), x
    .else
    :       lda ScreenDest + (YY * (40 * 8)), x
            sta ScreenSrc  + (YY * (Width * 8)), x
    .endif
            dex
            bpl :-

            ;; Copy color attributes
            ldx #(Width-1)
    .ifblank from_screen
    :       lda ColorSrc  + (YY * Width), x
            sta ColorDest + (YY * 40), x
    .else
    :       lda ColorDest + (YY * 40), x
            sta ColorSrc  + (YY * Width), x
    .endif
            dex
            bpl :-
    .endrepeat

            rts
    .endmacro

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ;; play
    button_play_plot:
            BUTTON_IMAGE_COPY  img_button_play, 0, 14
    button_play_save:
            BUTTON_IMAGE_COPY  tmp_img_button, 0, 14, 1
    button_play_restore:
            BUTTON_IMAGE_COPY  tmp_img_button,  0, 14

    ;; rew
    button_rew_plot:
            BUTTON_IMAGE_COPY  img_button_rew, 3, 16
    button_rew_save:
            BUTTON_IMAGE_COPY  tmp_img_button, 3, 16, 1
    button_rew_restore:
            BUTTON_IMAGE_COPY  tmp_img_button, 3, 16

    ;; ff
    button_ff_plot:
            BUTTON_IMAGE_COPY  img_button_ff,  7, 18
    button_ff_save:
            BUTTON_IMAGE_COPY  tmp_img_button,  7, 18, 1
    button_ff_restore:
            BUTTON_IMAGE_COPY  tmp_img_button, 7, 18

    ;; stop
    button_stop_plot:
            BUTTON_IMAGE_COPY  img_button_stop, 10, 18
    button_stop_save:
            BUTTON_IMAGE_COPY  tmp_img_button, 10, 18, 1
    button_stop_restore:
            BUTTON_IMAGE_COPY  tmp_img_button,  10, 18


Código: El Easter Egg
=====================

.. figure:: https://lh3.googleusercontent.com/Zp52TSOw_i2SzQ9zJhI0Fl28joPzCKIpYGy4v52h4r2AWZVsnXTGAJAh9dxEPs7vhTIv4x0CdGgt55xQcAhK7HoTrVOjsxdmW_cNiF4Yi9BfiLpB43dJ_Gsuoetg5CH5qNnaex8
   :alt: easter egg

El Easter Egg (huevo de pascua) esta hecho con:

-  Usa modo texto (PETSCII puro) para el sol y sus animaciones
-  7 sprites extendidos en X e Y para el scroll
-  Abre el borde vertical para por los sprites debajo del sol
-  Toca un sid que tiene que sonar bien en PAL/NTSC/Drean

Borde Vertical
--------------

Una manera de abrir el borde vertical es más o menos así:

1. Se cambia modo 24 filas cuando el VIC esta dibujando la fila 25 (entre los rasterlines ``$f2`` y ``$fa``)
2. Se cambia a modo 25 filas una vez que el raster pasó la fila 25.

y eso se tiene que hacer en cada frame.

Ejemplo:

.. code:: asm

    loop:

            lda #$f9                        ; raster line llegó a $f9?
    :       cmp $d012                       ; sino, esperarlo
            bne :-

            lda $d011                       ; cambiar a modo 24-filas
            and #%11110111                  ;
            sta $d011

            lda #$fc                        ; esperar a la rasterline $fc
    :       cmp $d012
            bne :-

            lda $d011                       ; cambiar a modo 25-filas
            ora #%00001000                  ; nuevamente
            sta $d011

            jmp loop


Esa es la lógica en general. Pero lo que hay que cambiar es como esperar a la
rasterline ``$f9`` sin que se consuma todos los ciclos. La manera más sencilla es
con una interrupción de raster... algo así:


.. code:: asm

    setup_irq:
            sei
            lda #$f9                        ; disparar IRQ en rasterline $f9
            sta $d012

            ldx #<irq_vector
            ldy #>irq_vector
            stx $fffe                       ; como BASIC/KERNAL estan apagados se usa
            sty $ffff                       ; $fffe/$ffff en vez de $0314/$0315
            cli
            rts


    irq_vector:
            pha                             ; guarda "A"

            asl $d019                       ; ACK interrupción raster

            lda $d011                       ; cambiar a modo 24-filas
            and #%11110111                  ;
            sta $d011

            lda #$fc                        ; esperar a la rasterline $fc
    :       cmp $d012
            bne :-

            lda $d011                       ; cambiar a modo 25-filas
            ora #%00001000                  ; nuevamente
            sta $d011

            pla                             ; restaura "A"
            rti                             ; restaurar "PC" y "Status"

Eso funciona en el 99% de los casos. Pero recordemos que tenemos que tocar
el sid para que funcione bien en PAL, NTSC y Drean. Y para el sid
tenemos que usar un timer con la velocidad correcta que puede ser que
sea distinta la velocidad del raster irq.

Supongamos que estamos corriendo el programita en una NTSC (ver *Detectando entre...* para más info):

-  Vamos a tener un timer que se dispara cada ``$4fb3`` (20403) ciclos para tocar el sid
-  Y además el raster IRQ se dispara cada ``$42c7`` (263*65 = 17095) ciclos para abrir el borde

.. figure:: https://lh3.googleusercontent.com/D50glqRSR3V8MMi-aXe41TiXWk9tHjyTKkTcrhQmUZFfdPHs07WbWRPhok07di0ydzyAkn16MeOLsQzOdxVipXaSjv6diR9pmNJHB2MCG-yg0kSJ8HcqRBvIPInhU3t30N34yXc
   :alt: collision in interrupts

   *Colisión entre Raster IRQ y Timer IRQ en NTSC. ¿Cual se ejecuta primero?*

Es posible que cada tanto, el borde no se abra porque la interrupción del sid
se ejecuta justo cuando se tenía que llamar a la interrupción del raster. En
la animación de arriba la barra blanca que "baja" muestra cuando se ejecuta
el Timer IRQ y su duración. Y la barrita chiquita de abajo muestra el Raster
IRQ. Como se puede ver, a veces "chocan" y no se sabe cual se ejecuta.


Interrupciones NMI
------------------

Una manera de lograr que el borde se abra siempre, es usar la interrupción NMI
(Non-Maskable Interrupt) para disparar el código del borde. La interrupción NMI tiene
prioridad sobre las demás interrupciones. Si la interrupción de Raster se
esta ejecutando cuando la NMI se tiene que ejecutar, la Interrupción NMI
interrumpe la interrupción Raster. Pero nadie puede interrumpir a una interrupción
NMI.

La interrupción NMI se puede disparar con los siguientes eventos:

-  Apretando la tecla Restore
-  Hardware
-  Con el Timer A del CIA 2: `$dd0d`_ y demás amigos

Y en nuestro caso, vamos a usar el Timer A del CIA 2. Es así:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; init_nmi
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    init_nmi:
                                            ; setup NMI (abrir borde)
            ldx #<nmi_openborder
            ldy #>nmi_openborder
            stx $fffa                       ; usa el vector de NMI ($fffa/$fffb)
            sty $fffb                       ; y no el de IRQ ($fffe/$ffff)

            lda #$0                         ; parar timer A CIA 2
            sta $dd0e


                                            ; PAL,      (312 * 63) $4cc8 - 1
                                            ; PAL-N,    (312 * 65) $4f38 - 1
                                            ; NTSC,     (263 * 65) $42c7 - 1
                                            ; NTSC Old, (262 * 64) $4180 - 1

            ldx #<$4cc7                     ; default: PAL
            ldy #>$4cc7

            lda ZP_VIC_VIDEO_TYPE           ; $01 --> PAL
                                            ; $2F --> PAL-N (Drean)
                                            ; $28 --> NTSC
                                            ; $2e --> NTSC-Old
            cmp #$01
            beq @done

            cmp #$2f
            beq @paln

            cmp #$28
            beq @ntsc
            bne @ntsc_old

    @paln:
            ldx #<$4f37n                    ; ciclos para PAL-N (Drean)
            ldy #>$4f37
            bne @done

    @ntsc:
            ldx #<$42c6                     ; ciclos para NTSC
            ldy #>$42c6
            bne @done

    @ntsc_old:
            ldx #<$417f                     ; ciclos para NTSC-Old
            ldy #>$417f                     ; fall-through

    @done:
            stx $dd04                       ; Timer A: low-cycle-count
            sty $dd05                       ; Timer A: high-cycle-count

            lda #%10000001                  ; habilitar interrupción timer A
            sta $dd0d                       ; en CIA 2

    :       lda $d012                       ; esperar a que el rasterline llegue
    :       cmp $d012                       ; a $f9, que es donde queremos abrir
            beq :-                          ; el borde
            cmp #$f9
            bne :--

            lda #%10010001                  ; ¡y habilitar el timer A!
            sta $dd0e

            rts

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; nmi_openborder
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    nmi_openborder:
            pha                             ; guardar "A"

            lda $dd0d                       ; ACK la interrupción del Timer CIA 2

            lda $d011                       ; abrir borde vertical
            and #%11110111                  ; cambiando a 24-filas
            sta $d011

            lda #$fc                        ; Esperar a que el rasterline llegue a $fc
    :       cmp $d012
            bne :-

            lda $d011                       ; y cambiar nuevamente a 25-filas
            ora #%00001000
            sta $d011

            pla                             ; restaurar "A"
            rti                             ; restaurar "PC" y "Status"


Y de esa manera el borde siempre se va a abrir, sin importar que la interrupción
IRQ este activada.

Scroll con Sprites
------------------

El Scroll se hace con 7 sprites expandidos tanto en X como Y, abarcando todo
el largo de la pantalla. El largo de la pantalla son 320 pixeles. Y con 7 sprites
expandidos en X cubrimos: 7 * 24 * 2 = 336 pixeles.

.. figure:: https://lh3.googleusercontent.com/wqwavZCFHLGy1xzLNMvtDXbfbzDTqjBEZ4rUNuq4R1GR8N-UK4Olh63-YYColFjcexYR_2PnoquipJDkYuf4NDGbcb2hMgCHbeJPDlB2-LriVoEkVfC0c5gpH3xhUwLuBrEBc8Q
   :alt: scroll con sprites

   *Scroll con 7 sprites*

El scroll no se puede hacer con caracteres, porque se hace debajo de la linea
25. Y lo único que se puede por ahí son sprites.

El truquito es muy sencillo:

1.  Se ponen 7 sprites expandidos en X, uno al lado del otro
2.  Al principio los sprites están "vacíos"
3.  Se calcula el ``C`` (*carry*) para actualizar el sprite de más a la derecha
4.  Se hace ``rol`` de cada fila del sprite. Y ``carry`` se usa para la columna anterior de la misma fila

Es similar un scroll de texto normal, pero en vez de scrollear caracteres,
se scrollean bits de sprites. El código es este:


.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; animate_scroll
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    animate_scroll:
            ; usa $fa-$ff como variables temporales
            lda #0
            sta $fa                         ; variable temporal

            ldx #<CHARSET_ADDR              ; dirección donde esta el charset
            ldy #>CHARSET_ADDR
            stx $fc
            sty $fd                         ; $fc/$fd son los punteros al charset

    load_scroll_addr = * + 1
            lda SCROLL_TEXT                 ; self-modifying
            cmp #$ff                        ; si "char == $ff" es fin de scroll
            bne next
            ldx #0                          ; resetear scroll, ya que se vuelve a empezar
            stx ZP_BIT_INDEX
            ldx #<SCROLL_TEXT
            ldy #>SCROLL_TEXT
            stx load_scroll_addr
            sty load_scroll_addr+1
            lda SCROLL_TEXT

    next:                                   ; A tiene el char a dibujar
            clc                             ; char_idx * 8, ya que cada char
            asl                             ; ocupa 8 bytes en el charset
            rol $fa
            asl
            rol $fa
            asl
            rol $fa

            tay                             ; char_def = ($fc),y

            clc
            lda $fd
            adc $fa                         ; A = charset[char_idx * 8]
            sta $fd


            ; scroll 8 bytes de arriba
            ; YY = filas del sprite. 8 en total
            ; SS = numero de sprite. 7 en total
            .repeat 8, YY                   ; "unrolled loop" para que se más rápido
                    lda ($fc),y
                    ldx ZP_BIT_INDEX        ; Se actualiza el "C" (carry) con el valor
    :               asl                     ; necesario para actualizar el sprite
                    dex                     ; de más a la derecha
                    bpl :-

                    .repeat 7, SS           ; cada sprite tiene 3 "columnas". scrollear cada una de las 3
                                            ; empezando por la la más a la derecha
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 2
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 1
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 0
                    .endrepeat
                    iny                     ; byte of the char
            .endrepeat


            ldx ZP_BIT_INDEX                ; bit index va de 0 a 7
            inx                             ; se incrementa una vez por "scroll"
            cpx #8                          ; cuando llega a 8 (overflow)
            bne l1                          ; se lee el siguiente char del texto
                                            ; del scroll
            ldx #0
            clc
            inc load_scroll_addr            ; siguiente char a leer del texto
            bne l1                          ; del scroll
            inc load_scroll_addr+1
    l1:
            stx ZP_BIT_INDEX

            rts

..

    .. note:: Ojo que estamos ``rol`` eando 163 (7 * 8 * 3) bytes por frame, un total
      de 978 (163 * 6) ciclos de CPU. No es muchísimo, pero es mucho más de lo que se usa en
      un scroll de texto normal. Si usaramos los 24 pixeles del sprite,
      sería el triple de costoso. ¡Ojo!

.. figure:: https://lh3.googleusercontent.com/7j8O3TKZuljEjbSTtlfsd1xLsErRXOsI8W147As4KsvjfNXetZUhP8-BFk3AjiWW1tA7FcGjHrGRQrjOtvjbo38lfcLyaRo1GWP7p_RCFIshxOm3Gb7pOOTug6eVFLZeQ4zcagY
   :alt: rasterbars

   *Consumo de rasterbars. A:Scroll, B:Música, C:Abrir borde*

Descomprimiendo el Easter Egg
-----------------------------

Una vez que el Easter Egg se activa hay que descomprimirlo. El problemita
acá es que el Easter Egg no entra en un "chunk" continuo. Esta partido en 3
partes:

-  código comprimido: ``$0118 - $07ff``
-  sid comprimido: En algún lugar cerca de ``$e000``
-  texto de scroll comprimido: En algún lugar cerca de ``$f800``

Lo interesante acá es el código comprimido que usa parte del stack de la c64.
Y para evitar que el stack pise al código comprimido, le decimos al stack
que el "tope" del stack es ``$0117`` con las siguientes instrucciones:

.. code:: asm

        ldx #$17                        ; solo se usan 24 bytes para el stack
        txs                             ; el resto esta reservado para el easter egg


La memoria del Player con el Easter Egg comprimido queda así:

::

    ; Memoria del Player con Easter Egg comprimido
    $0000 - $00ff: Zero Page: 256 bytes usados para variables temporales
    $0100 - $0117: 24 bytes usados para stack
    $0118 - $07ff: Código Easter Egg comprimido
    $0800 - $0fff: Código Player (1/3)
    $1000 - $32f7: Buffer para tocar el sid más grande (~9k)
    $32f8 - $3fff: Código Player (2/3)
    $4000 - $5fff: Gráfico bitmap (8k)
    $6000 - $63ff: Gráfico color (Screen RAM) (1k)
    $6400 - $68ff: Sprites (~1k)
    $6900 - $6cff: Charset (1k)
    $6d00 - $73ff: Sid de Ruido blanco (1.7k)
    $7400 - $7caf: Imagenes de botones apretados + buffer temporal (~2k)
    $7cb0 - $fbdf: Sids comprimidos, incluyendo el Sid del Easter Egg (28k)
    $7be0 - $7ccf: Código Player (3/3)
    $fcd0 - $fff0: Texto Scroll Easter Egg comprimido (~800 bytes)

Y cuando se dispara el Easter Egg, se descomprimen los tres pedazos del Easter
Egg en el destino correcto.

Código: La Intro
================

.. figure:: https://lh3.googleusercontent.com/STIEW1KCcW65Y0U0NMOHebjsrQzkk4IuxsbqR6kVTvzx0O16ZmJYTJ_S0ttv1L5bIv0_Qsg5oGzb3pnVAiJbxBqNMg8HX658PNziScLQB1R3csABQSgB5Pt8nsC-N03Nmv9v_DU
   :alt: intro

La intro es bastante simple:

-  La parte de arriba usa un gráfico bitmap multi-color
-  La parte de abajo usa modo texto + charset custom
-  Los textos están coloreados con "rasterbars" invertidas

Rasterbars
----------

Quizás lo más interesante es como lograr el "color invertido". Es sencillo:

1.  Se hace el efecto típico de las rasterbars

.. figure:: https://lh3.googleusercontent.com/IAw3ncdKryj8scLeYa5HpEK-CUChYbFRoTjvT5oNYwmsccQMj6iN1JCmd9xO3aoCUFqJGQEk-rtKjmMECm1hrvyjRILJowwIbQwXc1XvQAJ6AC4Gkj5bnRxty-6gH_WWgHeTIIY
   :alt: intro-rasterbar

2.  Se pone texto encima

.. figure:: https://lh3.googleusercontent.com/rXTTYVOZKkL5GbZRE01CcRoiRdhjZYQ4rcNd_Y5jlTk9AJvzMIkiMycnhdnJGTlWuqmfsjTELB5Zf_8s43yPObuXVAIGB68sOdWQk43HHUk6KosqHifntMtqmrA9wrKJaAk-FcA

3.  Se usa un charset invertido. Es decir, los bits que están en 0 pasan a 1, y viceversa

.. figure:: https://lh3.googleusercontent.com/0d8Az60NjW3UsVP1Nsc2LGawrjcVQOURv58cpHb4eCFgS9rnyOpUjj92dVuUEUQ6urUC22a7aLwiF2o9Yx_vJmnhvRi-vsdz7kN07dr1dfvCDdY_YgxC1YaqhTcnapGVyfoE6RI


Intro-linker
------------

La Intro, una vez que finaliza, descomprime el Player. Para eso hay que tener
en cuenta dos cosas:

-  El código de descompresión no se tiene que "pisar" mientras descomprime
-  La data a descomprimir no se tiene que pisar mientras se descomprime

Para lo primero, lo que se hace, es poner el código de descompresión en una
dirección que no se va a usar, como ``$0400`` (dirección de Screen). Y para
evitar que sea vea "feo", pintamos todo de negro:


.. figure:: https://lh3.googleusercontent.com/Az0sLlckuc5AZ11CAEMfEHt5Qhytwjo5pF8VoXPMUOrXPdhah23WTuGCQ5OHHjImepzFYRMuDoMV6Pj_keYu7i5InAxd5shUcByNSwLibPkMDoTOBi9edcjgBEsKe4IZkIZ9m5U
   :alt: intro-linker negro

   *Todo negro. No se ve el código de descompresión*

Pero si el fondo no fuese negro, se vería lo siguiente:

.. figure:: https://lh3.googleusercontent.com/4HefvIHOzC2oM23jOjZGUgnR6ChVb4Jj8hm-2_w8MpHTAWKjvFdWt0YDl-KQwV4ox6kVlyNSxbpfqvrtk7KuOesAi8XnnFHebSxmmXL5gk1r5m-ouYBrsAqvgg-X3DKianQIfQs
   :alt: intro-linker transparente

   *Lo que se vería si la pantalla no tuviese fondo negro*

Para lo segundo, es sencillo también, pero se complicó en el Chipdisk.
El Player + Easter Egg comprimidos ocupan 40585 bytes, y cuando de descomprimen
ocupan 63427 bytes. La C64 solo tiene 64k RAM, así que la rutina de descompresión
va a pisar la data comprimida en algún momento. La idea es que la pise solo una
vez que haya usado la data comprimida.

Y la rutina de descompresión del Exomizer_ va de "atrás
para adelante". Es decir, empieza a descomprimir el último byte primero.

::

        Memoria antes de descomprimir el Player + Easter Egg:
        +----+---------------+---------------------------------------------+---------------+
        |    |$0400-$07ff    | $0800 - $afff                               | $b000 - $fff0 |
        |    |Descompresor   | Código Player+Easter Egg comprimidos        | Intro         |
        +----+---------------+---------------------------------------------+---------------+

        Memoria luego de descomprimir:
        +---------------------+------------------------------------------------------------+
        |                     | $0820 - $fff0                                              |
        |                     | Código Player+Easter Egg                                   |
        +---------------------+------------------------------------------------------------+

Entonces para evitar que pisa data no usada, se hace algo así:

-  La dirección del último byte comprimido esta en ``$afff``, y ese byte
   descomprimido va a parar a ``$fff0``. Así que no "pisa" la data comprimida.

-  Y la dirección del primer byte comprimido esta en ``$0800``, y ese byte
   descomprimido va a parar a ``$0820``. Pisa la data comprimida, pero una vez
   que ya fue usada.

**Reglita**: Tanto la dirección del primer byte de origen como la del último
tienen que ser menores que las direcciones del primer y último byte de destino
respectivamente: ``$0800 < $0820`` y ``$afff < $fff0``.

Raster IRQ estable
------------------

Cuando uno usa los interrupciones de raster, el callback no siempre es llamado
donde uno quiere. Si uno hace: ``lda #$80; sta $d012``, el
raster nos va a llamar cuando el rasterline este en ``$80``, pero ¿en que parte
de la rasterline ``$80``? A veces nos llama en la mitad de la linea, y otras
veces más adelante o atrás.

Y eso hace que un simple efecto como el rasterbar se vea "inestable"...
con una linea de color partida por la mitad, o cosas por el estilo.

Bueno, en la Intro pasa algo similar cuando cambiamos la pantalla de modo
bitmap a modo texto. A veces aparece/desaparece una linea negra en el rasterline
que las divide.

.. figure:: https://lh3.googleusercontent.com/kvWKJs7IEaFXfR8dKVI21ans9NSVY9WMXZ_qr9MuM6ugq7TCIiGyzSkDb-YCMWw_15bN_1TJ-J0FerIf2D8K1j_f37xjTixXUFIP6Bl8E-F89jFnaIJj51qrAsTdTUJSmI_VwCk
   :alt: artifact

   *Arriba de la P y demás letras aparece/desaparece una linea negra. Ese es el "artifact"*

Y eso se soluciona con un *raster IRQ estable*. Lo que hace es que el callback
siempre es llamado en el mismo ciclo del rasterline. Después uno lo puede ajustar
poniendo unos ``nop`` s adicionales. Hay distintas técnicas para lograr el
raster estable. El Chipdisk usa la técnica llamada "doble IRQ".

El código es así:

.. code:: asm

        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        ; STABILIZE_RASTER
        ; Rutina de raster estable usando Doble-IRQ
        ; Sacada de from: http://codebase64.org/doku.php?id=base:stable_raster_routine
        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .macro STABILIZE_RASTER
                ; El Raster IRQ es disparado en el ciclo 0 del actual rasterline ($d012)
                ; Pero la CPU tiene que terminar el opcode que se esta ejecutando antes de llamar a la IRQ
                ; Así que puede haber un retraso de 0 a 7 ciclos, dependiendo del opcode
                ; Después hay un retraso de unos 7 ciclos llamando a la "Interrupt Handler" (Push SR/PC to stack++)
                ; Y luego se consumen otros 13 ciclos más para guardar los registros (pha, txa, pha, tya, pha)

                ; ciclos consumidos hasta aca: 20~27
                lda #<@irq_stable   ; +2, 2
                ldx #>@irq_stable   ; +2, 4
                sta $fffe       ; +4, 8
                stx $ffff       ; +4, 12
                inc $d012       ; +6, 18
                asl $d019       ; +6, 24
                tsx             ; +2, 26
                cli             ; +2, 28

               .repeat 10
                        ; La próximo IRQ va a ser llamada mientras se ejecutan estos nops
                        nop         ; +2 * 10, 48.
                .endrepeat
                ; cycle count: 68~75. La nueva raster IRQ ya fue llamado en este punto

        @irq_stable:
                ; cycle count: 7~8 .7 cycles for the interrupt handler + 0~1 cycle Jitter for the NOP
                txs         ; +2, 9~10

                ; 42 cycles
                ldx #$08        ; +2, 11~12
                dex             ; +2 * 8, 27~28
                bne *-1         ; +3 * 7, +2, 50~51
                bit $00         ; +3, 53~54

                lda $d012       ; +4, 57~58
                cmp $d012       ; +4, 61~62
                beq *+2         ; +2/+3, 64
        .endmacro


        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        ; raster IRQ que divide la pantalla entre modo bitmap y modo texto
        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        irq_rasterbar:
                pha                             ; guarda A, X, Y
                txa
                pha
                tya
                pha

                STABILIZE_RASTER                ; llama a la macro "STABILIZE_RASTER"
                                                ; que es la que hace toda la magia

                sei

                ldx #0                          ; rutina que genera los "raster bars"
        @l0:
                lda $d012
        @l1:    cmp $d012
                beq @l1                         ; espera a nueva linea de rasterline
                lda palette,x                   ; y cuando pasa eso cambia el color
                sta $d021                       ; $d021 para generar una nueva linea
                inx
                cpx #TOTAL_PALETTE
                bne @l0

                asl $d019                       ; ACK la interrupción raster

                lda #%00001000                  ; no scroll, multi-color apagado, 40-cols
                sta $d016

                lda #%11101100                  ; screen en 0x3800, charset en $3000
                sta $d018

                lda #%00011011                  ; bitmap mode apagado. modo texto.
                sta $d011

                ldx #<irq_bitmap                ; apuntar al raster IRQ que prende
                ldy #>irq_bitmap                ; el modo bitmap
                stx $fffe
                sty $ffff

                lda #20
                sta $d012                       ; próximo raster IRQ en la rasterline $20

                pla                             ; restaura A, X, Y
                tay
                pla
                tax
                pla
                rti                             ; restaura previo PC, status

La técnica Doble IRQ funciona bastante bien pero con ciertas limitaciones:

-  Consume ciclos adicionales
-  No se puede llamar en las *bad lines* [#]_

Fuentes del Chipdisk y demás
============================

Cómo compilar el Chipdisk
-------------------------

El código completo del Chipdisk esta acá:

- https://github.com/c64scene-ar/chipdisk-nac-vol.1

Para compilarlo se necesita:

-  cc65_ (probado con v2.15)
-  Exomizer_ (probado con v2.0.9)
-  VICE_ (opcional, usado para generar el .d64)
-  ``make``

Poner todo en el path, clonar el repositorio, y hacer:

.. code:: bash

        $ git clone https://github.com/c64scene-ar/chipdisk-nac-vol.1.git
        $ cd chipdisk-nac-vol.1
        $ make

Licencia
--------

`Apache v2 <https://www.apache.org/licenses/LICENSE-2.0>`__


Comentarios adicionales
-----------------------

El código **no** fue escrito a modo de ejemplo. Es código real
escrito para el Chipdisk que presentamos en la Datastorm 2017.
Eso significa que tiene todos los problemas de "código real".

-  Esta basado en el código del Chipdisk *Arriba Las Manos* que presentamos en DeCrunch 2016
-  Lo requerimientos fueron cambiando. El código también. Puede que haya
   código que no se use, o código que ya no tenga más sentido tener.
-  Se usaron demasiadas macros/unrolled-loops. Quizás hubiera sido mejor usar menos
   para tener lugar adicional para un posible nuevo tema.
-  Hay pocos comentarios
-  El Easter Egg tiene algunos bugs en el scroll. Con tiempo los arreglaremos
-  Y puede que haya otros bugs también.

Preguntas y demás
=================

¿Tenés preguntas? ¿Querés colaborar con PVM? Estamos acá:

-  http://pungas.space
-  En IRC. `EFnet <http://www.efnet.org/>`__ . Canal #pvm
-  `Twitter <https://twitter.com/pungas64>`__
-  `Facebook <https://www.facebook.com/PVM1996/>`__

Referencias
===========

.. [#] La herramienta que se usa para comprimir sids es esta: `sid_to_exo.py <https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_to_exo.py>`__
.. [#] La rutina de descompresión esta en el .zip del Exomizer_, pero también la pueden ver acá: `exodecrunch.s <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/src/exodecrunch.s>`__
.. [#] La gran idea de hacer un charset especial para simplificar el rendereo es de Alakran
.. [#] O como bien recomienda Acid, se podría optimizar solamente ``set_pixel()`` con tablas para evitar la multiplicación.
.. [#] Más ca65 Pseudo Functions: https://cc65.github.io/doc/ca65.html#s10
.. [#] Más truquitos de como optimizar el 6502 están acá: `6502 assembly optimisations <https://wiki.nesdev.com/w/index.php/6502_assembly_optimisations>`__ y acá `Synthetic instructions <https://wiki.nesdev.com/w/index.php/Synthetic_instructions>`__. Y también acá: `CodeBase64 <http://codebase64.org/>`__
.. [#] Para más información sobre Bad Lines ir a `Beyond the Screen: Rasters and Cycles <http://dustlayer.com/vic-ii/2013/4/25/vic-ii-for-beginners-beyond-the-screen-rasters-cycle>`__


.. |intro| image:: https://lh3.googleusercontent.com/8428M9tm2gsXy5q5725Y_ddPiO6XLxNfV3E2jt5q8hp_-G65M-klPna7nJjk82BdEBR9_o72mYsDN3gf9o_OgABJ6cuH_D1nl-VJnfRtxrtabcdKGNIpkLQZDWX2Wx9qhprp8XU
.. |player| image:: https://lh3.googleusercontent.com/skZURzPSvCWF27RkDVUQr9UtxuAcp2G8yxv1TS5qQg5Cp1rt_TFXyMPCxAoveqRSgikO6g7O6ZatJJm_S3m-U3u5d4O-ge4XWSnnDQmQHCBu_b7YjKY4LD_I_woCqrSpOseXNLU
.. |easteregg| image:: https://lh3.googleusercontent.com/qKBOrZy7L8NlH0d3pOxV4EkrlJzqY16sPv6xrO_R2eeGcLt_2MV9BR-UakHxEjecRKL_4s9gXM7sfQDs8D0C5f_OvUZaZUc75SH_a_7d8dGKABBjjH4TIABonNONQ4Iye7fciI0

.. _Exomizer: https://bitbucket.org/magli143/exomizer/wiki/Home
.. _cc65: https://github.com/cc65/cc65
.. _VICE: http://vice-emu.sourceforge.net/
.. _$01: http://unusedino.de/ec64/technical/aay/c64/zp01.htm
.. _$0314/$0315: http://unusedino.de/ec64/technical/aay/c64/zp0314.htm
.. _$d011: http://unusedino.de/ec64/technical/aay/c64/vic17.htm
.. _$d012: http://unusedino.de/ec64/technical/aay/c64/vic18.htm
.. _$d016: http://unusedino.de/ec64/technical/aay/c64/vic22.htm
.. _$d018: http://unusedino.de/ec64/technical/aay/c64/vic24.htm
.. _$d019: http://unusedino.de/ec64/technical/aay/c64/vic25.htm
.. _$d01a: http://unusedino.de/ec64/technical/aay/c64/vic26.htm
.. _$d020: http://unusedino.de/ec64/technical/aay/c64/vic32.htm
.. _$d021: http://unusedino.de/ec64/technical/aay/c64/vic33.htm
.. _$d022: http://unusedino.de/ec64/technical/aay/c64/vic34.htm
.. _$d023: http://unusedino.de/ec64/technical/aay/c64/vic35.htm
.. _$d419: http://unusedino.de/ec64/technical/aay/c64/sid25.htm
.. _$d41a: http://unusedino.de/ec64/technical/aay/c64/sid26.htm
.. _$dc00: http://unusedino.de/ec64/technical/aay/c64/cia10.htm
.. _$dc01: http://unusedino.de/ec64/technical/aay/c64/cia11.htm
.. _$dc04: http://unusedino.de/ec64/technical/aay/c64/cia14.htm
.. _$dc05: http://unusedino.de/ec64/technical/aay/c64/cia15.htm
.. _$dc0e: http://unusedino.de/ec64/technical/aay/c64/cia114.htm
.. _$dc0d: http://unusedino.de/ec64/technical/aay/c64/cia113.htm
.. _$dd00: http://unusedino.de/ec64/technical/aay/c64/cia20.htm
.. _$dd0d: http://unusedino.de/ec64/technical/aay/c64/cia213.htm
.. _$ea31: http://unusedino.de/ec64/technical/aay/c64/romea31.htm
.. _$ea81: http://unusedino.de/ec64/technical/aay/c64/romea81.htm
.. _$ffe4: http://unusedino.de/ec64/technical/aay/c64/romffe4.htm

