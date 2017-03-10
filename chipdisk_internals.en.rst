Intermediate asm tutorial for the c64: Making a Chipdisk
========================================================

:Version: 0.1 (`get the latest version <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/chipdisk_internals.es.rst>`__)
:Author: `riq <http://retro.moe>`__ / `Pungas of Villa Martelli <http://pungas.space>`__
:Translation: `Mark Seelye a.k.a. Burning Horizon <mseelye@yahoo.com>`__

.. contents:: Contents
   :depth: 2

Introduction
============

Hi. First download the Chipdisk to get an idea of ​​what it covers
The link:

- `chipdisk-nac.d64 <https://github.com/c64scene-ar/chipdisk-nac-vol.1/raw/master/bin/chipdisk-nac.d64>`__

Okay, let's start. Playing a sid on a Commodore 64 is very simple:

.. code:: asm

    setup:
        sei                 ; Disable interrupts

        lda #<irq_vector    ; Set IRQ vector to be called
        sta $0314           ; Once per screen refresh
        lda #>irq_vector
        sta $0315           ; The $ 314 / $ 315 vector points to the IRQ raster routine

        lda #$00
        jsr $1000           ; Initialize sid to play song 0
                            ; because a sid can have more than one song

        cli                 ; Enable interrupts again
        rts

    irq_vector:
        asl $d019           ; ACK raster interrupt

        jsr $1003           ; Call the sid

        jmp $ea31           ; Exit interrupt

...and done. Each sid knows how to play all by itself, since a sid is code + data.
The call `` jsr $ 1003`` does all the magic, and that code is inside of sid.

The complicated thing about making a Chipdisk, is not touching the sid, but everything
else. Let's see why.



Memory Organization
===================

VIC, Banks and others
---------------------

*Note*: I guess you already know how to use sprites and graphics modes. If you still
do not know, go to `Bringing Sprites in good Shape <http://dustlayer.com/vic-ii/2013/4/28/vic-ii-for-beginners-part-5-bringing-sprites-in-shape>`__
and `Screen Modes Cheaper by the Dozen <http://dustlayer.com/vic-ii/2013/4/26/vic-ii-for-beginners-screen-modes-cheaper-by-the-dozen>`__

Let's start with the basics. There are 64k RAM available, but when we turn on the computer
it says there are ``38911 BASIC BYTES FREE``. That means that if we are going to use BASIC,
there is only 38k RAM free.

.. figure:: https://lh3.googleusercontent.com/q9Fndsw89AVrXaPtPwr9FUPH42cbtExt4vuyi_VpAFCXG_W_7nMhPqZ2-CAfSbFaERt0IK-9eqAlY2nJrM4FKwZ--hEpjcbTzlCrcIKTXJ5ESBGulrjjiN3KsF-1bcztXnww_a0
   :alt: memory\_free

   *There is 38k free RAM to use from BASIC, but 64k RAM from asm*

But since we are not going to use BASIC, we *turn it off* and it releases to us 8k RAM.
And if we continue to turn everything off, like the KERNAL and so on, then there we will have
The full 64k of free RAM.

To turn these things on / off, use the address `$01`_

In the Chipdisk we use two configurations:

-  All RAM except ``$d000 - $dfff`` (area reserved for IO:
   VIC / SID / CIA): it is always used, except with
   the sids decompressed
-  All RAM (64k free RAM): Used when the sids are decompressed

It is used like this:

.. code:: asm

        lda #37                 ; Default value of C64
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: BASIC
                                ; C000-CFFF: RAM
                                ; D000-DFFF: IO (VIC,SID,CIA)
                                ; E000-FFFF: KERNAL

        lda #$35                ; Used by the Chipdisk normally
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: RAM
                                ; C000-CFFF: RAM
                                ; D000-DFFF: IO (VIC,SID,CIA)
                                ; E000-FFFF: RAM

        lda #$34                ; Used by the Chipdisk when it decompresses
        sta $01                 ; 0000-9FFF: RAM
                                ; A000-BFFF: RAM
                                ; C000-CFFF: RAM
                                ; D000-DFFF: RAM
                                ; E000-FFFF: RAM

There are several possible combinations. Go here for more info `<http://unusedino.de/ec64/technical/aay/c64/zp01.htm>`__

The other thing, is that the VIC (the *GPU*) needs the RAM as well.
If we want to draw a bitmap graphic, we put the graphic in RAM and
The VIC reads it from there (from RAM). So the RAM is shared between the CPU (the 6510)
and the GPU (the VIC).

But there is a limitation: The VIC can only see 16k at a time of the 64k RAM.
There are 4 banks of 16k each (`` 64k / 16k == 4``) of which the VIC can
read the data.

- Bank 0: ``$0000 - $3fff``
- Bank 1: ``$4000 - $7fff``
- Bank 2: ``$8000 - $bfff``
- Bank 3: ``$c000 - $ffff``

This means that a bitmap graphic can not be half in one bank and half
in another. It has to all be in one bank.

That is not all. It can not be anywhere in the bank. There are places
special to put bitmaps, charset and screen RAM.

And that adds to other limitations not discussed here. To tell the
VIC which bank to use is done through the registry `$dd00`_ of CIA 2, like this:

.. code:: asm

        lda $dd00                       ; CIA 2
        and #$%11111100                 ; Mask the first 2 bits
        ora #2                          ; 3 for Bank 0
                                        ; 2 for Bank 1
                                        ; 1 for Bank 2
                                        ; 0 for Bank 3
        sta $dd00

To tell the VIC where to find the bitmap, charset and screen + sprite ptr. is made
through the registry `$d018`_ of the VIC.

.. figure:: https://lh3.googleusercontent.com/hRPBQeC8azhb1h5fmaBBfaLfqA_zQgGvFEI56Dyq-lIpAOzCbQCwsoGiynGc2Zr-XBcLJXGbmnfPsdbK_xwWAjw48-Fs2Lknnx9TGaHGj2ttM5oPYOmZVxhVLdP-YzqILJCZwTk
   :alt: Internals of each bank

   *Internal memory of each bank*

But that is not all. Banks 0 and 2 (``$0000- $3fff`` and ``$8000- $bfff``) have
mapped between ``$1000- $1fff`` and ``$9000- $9fff`` respectively to the charset
default (uppercase and lowercase). That means we can not use those
addresses for the VIC to view data ... except to see the default charset.

.. figure:: https://lh3.googleusercontent.com/hgGTs3AF3tFO6FuL3F1aWGujcLNspxEFnY6JARm53sRvWik8hTKNJAPDgMFbzeoJCu_LPDy7Tyaz7tjrMUO9tHwwiHQXw74_W87_uIbPpQR_cZCVCE8oRHikpQ2WrGpDp_DC46A
   :alt: banks of the VIC

   *The four banks available*

The VIC *sees* the charset by default in those locations because the charset
has to be somewhere. But if it is in RAM it will occupy RAM, and
then the free 38k for BASIC would now have 4k less. I suppose so that does
not happen the engineers of C= decided to map the VIC the charset in those
locations.


Summary:

- There are 4 possible banks where to put the data for the VIC
- VIC values are modulo ``$4000``
- In the locations ``$1000- $1fff`` and ``$9000- $9fff``, the VIC **sees** the charset by default
- `$dd00`_ is used to change banks. And `$d018`_ is used to tell SID where to get the data


Sids, Exomizer, and others
---------------------------

How much RAM do we need for Chipdisk? Let's figure it out.
The Chipdisk is composed of 3 modules:

-  Intro: Half graphic multi-color + half screen PETSCII + charset +
   code
-  Player: 9 songs (sids) + sound for white noise + graphic
   Bitmap + charset + code
-  Easter Egg: 1 song (sid) + PETSCII graphic + scroll text + code

|intro|\ |player|\ |easteregg|

The Player module alone occupies:

- The 9 sids: ~ 53k
- Bitmap graphic: 9k (8k bitmap + 1k colors)
- White noise (used between songs): ~ 1.8k
- Images of buttons (bitmap + colors): ~ 1,7k
- Charset (used in oblique letters): 1k
- Sprites (cursors, casters, counter): ~ 1k

That gives us a total of: ~ 65k, not counting code, nor the intro and easter egg.
How do we put everything in 64k of memory and without accessing the disk?

The answer is: Compresses everything that can be compressed, and decompresses
when needed.

- The 9 compressed sids [#]_ using Exomizer_ occupy: ~ 28k

But before a sid can be accesed it must be decompressed somewhere.
For that you need free RAM. So we need a buffer as big as the
biggest sid.

In our case the sid that occupies most is *Prófugos* with 9k. Something
quite unusual for a sid (they usually do not occupy more than 4k), for
instruments and data, you can not reduce more than that without
losing sound quality.

Then we need a total of 37k (28k + 9k) for sids. This is
much better than the original 53k (16k less!).

The 9k buffer starts at the address ``$1000``. You can start at any
place, but by default sids run in ``$1000``, so we follow
using ``$1000``. So from ``$1000`` to ``$3328`` (8952 bytes) is
reserved to decompress the sids.

*Note*: Do you know why almost all sids start at ``$1000``? See section
above to know.

The compressed sids start from ``$7cb0``. The higher up
the better, thus freeing up place for the bitmap graphic (see below).

So far the memory is like this:

::

    $0000 - $0fff: Free (4k)
    $1000 - $32f7: Reserved buffer to play a sid (~9k)
    $32f8 - $7caf: Free (18k)
    $7cb0 - $fbdf: Compressed Sids (28k)
    $fbe0 - $ffff: Free (1k)

Bitmap graphic and others
-------------------------

Now you have to put the graphics some place. A good place to put it in Bank 1.
Use ``$4000-6000`` for the bitmap, and ``$6000- $6400`` for colors.
If we add the sprites, sid of white noise and so on, it looks like this:

::

    $0000 - $0fff: Free (4k)
    $1000 - $32f7: Buffer to decompress at least the largest sid (~9k)
    $32f8 - $3fff: Free (~3k)
    $4000 - $5fff: Bitmap graphic (8k)
    $6000 - $63ff: Colors (Screen RAM) (1k)
    $6400 - $68ff: Sprites (~1k)
    $6900 - $6cff: Charset (1k)
    $6d00 - $73ff: White Noise Sid (1.7k)
    $7400 - $7caf: Pressed button images and temporary buffer (~2k)
    $7cb0 - $fbdf: Compressed Sids (28k)
    $fbe0 - $ffff: Free (1k)

There is 9k left to put the player code. But remember that in those
9k also has to be the Easter Egg. This complicates things a bit.
Putting the intro does not take place in the 9k, I'll explain later
why.


Code: The Player
=================

The player code can be divided into:

- Sprites: Animated casters and more
- Unzip sid, modify it to play on NTSC / Drean
- Update song / author name
- Read events: mouse (port # 1), joystick (port # 2) or keyboard
- Animate pressed buttons
- Patching bitmap graphic with sprites
- Update song number

Sprites: Animated casters and more
----------------------------------

.. figure:: https://lh3.googleusercontent.com/5gtsDGNPpV8eU6wD3jYBJnJmpG23iXHaXga_NbVDUpKQa5gCSbN_2_bmCAaJP7DLaaiBOauma2cJHrBYQmMnXsYUB7erJ2c4bUCdkFAcQjPgYyEPZCc2bpb9_db66AQ0pKdo9rM
   :alt: sprites

   *Sprites used by the player*

Inside the player sprites are used in different places:

- Animation of the wheels: one sprite for each wheel
- Pointer: 2 sprites "overlayed"
- Power button: 1 sprite
- Counter for songs: 1 sprite
- Fix "artifacs" of the bitmap: 2 sprites

In total 8 sprites are used, so there is no need to multiplex the sprites.

.. figure:: https://lh3.googleusercontent.com/rZIaCnwOg7xCputC0GH9FF4xdUOl5-yW4c4ZgZpemclrt9qH6rbTglj91-NXl4tuC8aXvuheJiEiugWB-iP5o9uN4XW1W6TPFYzAdonBz4e9-et4Yc2VdBIXSaNn9MF7H4yGeWk
   :alt: Sprite locations

   *Location of the sprites*

The animation of the casters is trivial. You change the frame sprite every so many
seconds. Let's see how it is done:

.. code:: asm

    SPRITE_DATA_ADDR = $6400
    SPRITE0_POINTER = <((SPRITE_DATA_ADDR .MOD $4000) / 64)     ; Equivalent to 144
    TOTAL_FRAMES = 5

    do_anim_cassette:
            dec delay
            bne end                         ; End of delay?

            lda #3
            sta delay                       ; Restore the delay

            dec $63f8 + 6                   ; $63f8 + 6 is the "sprite pointer" for sprite 6
            lda $63f8 + 6                   ; Compares it to the first frame - 1
            cmp #(SPRITE0_POINTER - 1)
            bne :+
            lda #(SPRITE0_POINTER + TOTAL_FRAMES - 1) ; If so, set the frame again from the end
    :       sta $63f8 + 6                   ; Update sprite sprite pointer # 6
            sta $63f8 + 7                   ; And the same for the sprite # 7
    end:
            rts
    delay:
            .byte 1

And the sprites pointers are from ``$63f8`` to ``$63ff`` since it is being used
Bank 1 (``$4000-$7fff``) and we told the VIC that the Screen will be in
``$6000``.

A useful trick for sprites to look better is to draw a standard sprite
onto another sprite (standard or multi-color).

This is how that idea works:

.. figure:: https://lh3.googleusercontent.com/T1TmdjKnu_7BrDTvQr3L1Sre2jmwlM-KTsnBpCuEjK9g7esu5pQyd1gXsVoUOR2_L4w4jsZKX7w_RkhfgsCdztt1wWJbuu1zkJ9X8DpM7Xp8CxEJY_hX-YqFkdBxQDrxObXxi1Y
   :alt: overlay sprites

   *Overlayed sprites*

This idea is used a lot. Games like Bruce Lee (and hundreds of others) use it.
The only drawback is that it uses 2 sprites instead of one.

Another trick we use is to fix bitmap bugs with sprites. Remember
That the cells in the bitmap can not have more than 2 colors. And to solve
some pixels that look bad, we cover them with sprites.

And that's all about the Player Sprites.


Decompress sids and modify them ...
-----------------------------------

The sids are compressed with Exomizer_. The decompression routine we use is from
Exomizer [#]_. The interesting thing about this routine is that it is "multi
tasking". In other words, while decompressing, other things can be done. In our
case, while we are decompressing the sid, we will be animating the
cassette:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; get_crunched_byte()
    ; This subroutine is called by the decruncher. X, Y and Carry must be preserved.
    ; Update the decruncher pointer and animate casette casters
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    get_crunched_byte:
            lda _crunched_byte_lo           ; _crunched_byte_lo & _crunched_byte_hi
            bne @byte_skip_hi               ; are used by the decruncher (exomizer)
            dec _crunched_byte_hi           ; to know which byte has to decompress
                                            ; every time this routine is called
                                            ; you must decrease the pointer by one
                                            ;
    @byte_skip_hi:

            dec delay                       ; Occasianally we want to advance the wheels
            bne @cont                       ; of the cassette.
                                            ; "Delay" is a timer to animate the wheels
                                            ; at the correct speed

            lda wheel_delay_counter         ; Reset the delay
            sta delay

            php                             ; Saves Status (Carry and others) because
                                            ; The decruncher needs these values.
                                            ; Do not change this.

            lda is_rewinding                ; If you skip to the previous song, then
            beq @anim_ff                    ; animate the caster backwards instead of forwards.
            inc $63f8 + 6                   ; $63f8 + 6 and + 7 are the sprite frames
            lda $63f8 + 6                   ; of the sprites for the wheels
            cmp #(SPRITE0_POINTER + TOTAL_FRAMES)
            bne :+
            lda #SPRITE0_POINTER
    :       sta $63f8 + 6                   ; Update sprite pointer 6
            sta $63f8 + 7                   ; and sprite pointer 7
            jmp @done_anim
    @anim_ff:
            dec $63f8 + 6                   ; Here is the same, but with animation
            lda $63f8 + 6                   ; on "fast forward" (the other was "rewind")
            cmp #(SPRITE0_POINTER - 1)
            bne :+
            lda #(SPRITE0_POINTER + TOTAL_FRAMES - 1)
    :       sta $63f8 + 6                   ; Update the sprite pointers 6
            sta $63f8 + 7                   ; and 7
    @done_anim:
            plp                             ; Restore Status (Carry and others)

    @cont:
            dec _crunched_byte_lo
    _crunched_byte_lo = * + 1
    _crunched_byte_hi = * + 2
            lda $caca                       ; self-modyfing. Has to contain the last
                                            ; Byte + 1 of what you want to decompress
                                            ; Before calling this routine
            rts
    delay:
            .byte 5

Once the sid is decompressed, the frequency table must be modified
So it sounds the same in PAL, NTSC and Drean (PAL-N).

For that, you have to go to each sid and look where
the table of frequencies are for each one.

Frequency tables generally have 96 values:

- 8 octaves
- of 12 semi-tones each

Each half-tone occupies 2 bytes, so usually the sids store
The tables as follows:

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

So what you have to do is look for those tables (or similar) in the
Sids, and replace them in runtime with an NTSC table.

**IMPORTANT**: Not all tables are the same, but they are very
similar. For example, the note "A" in the 8th octave may appear as
$f820, and in others like $f830, or some other value. But the human ear
can not differentiate them.

It is best to search for ``$01, $01, $01, $01, $02, $02, $02`` and see if
it looks like the "hi" chart. Then go 96 bytes up  and see if there
is a "low" table.

.. figure:: https://lh3.googleusercontent.com/VqNAXgS2DOrbG7bJ729Fz3VWCjzkvTjH_DhtBnZeuL0iIszlmQdtWAnS8qEdBi5FX-fcFL9wfe7hAp0UHkWfmKDCQab5GokBc4vsL6IVRIDMWQdDdezC5bm7I9m2D5d8P8Lph08
   :alt: Lookup Table

   *Looking for the table of frequencies in a sid*

Once the values ​​are found, they are replaced by the NTSC values.
Here there is just a simple loop to copy the tables. Ex:


.. code:: asm

        ; Update frequency table
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

Interruptions, Timers and Raster
-------------------------------

The other thing to keep in mind is the speed of the the
Sid. Many trackers generate sids that play at 50.125Hz (PAL's
speed). It is ideal, but not all are like that. So double check
that (eg: SidTracker64).

To make something work at a certain speed on the C64, there are two
ways:

- With raster interrupts
- And / or with timer interrupts

Basically the interrupts are "callbacks" that call us when
something happens. These callbacks are programmable: you can activate
or deactivate.

Raster
~~~~~~

Raster interrupts are the most common. You tell the C64 that you
want a call when the raster is on a certain rasterline.

For example, if I wanted the edge of the screen to be black in
the top, and white on the bottom, two interrupts are used
in a chained raster. Like this:

.. code:: asm

    setup_irq:
        sei
        ldx #<raster_top        ; Address of our callback (IRQ)
        ldy #>raster_top
        stx $0314               ; IRQ vector lo
        sty $0315               ; IRQ vector hi

        lda #0
        sta $d012               ; Fire raster interrupt when rasterline is 0

        lda #1
        sta $d01a               ; Enable raster interrupt

        cli
        rts

    raster_top:
        asl $d019               ; ACK raster interrupt

        lda #0                  ; Update border
        sta $d020               ; color to black (0=black)

        lda #100                ; Chain the 2nd callback
        sta $d012               ; to be fired when rasterline is 100

        ldx #<raster_bottom
        ldy #>raster_bottom
        stx $0314
        sty $0315

        jmp $ea81               ; Exit interrupt

    raster_bottom:
        asl $d019               ; ACK raster interrupt

        lda #1                  ; update border
        sta $d020               ; color to white (1=white)

        lda #0                  ; Chain to the first callback
        sta $d012               ; that fires when rasterlineis 0

        ldx #<raster_top
        ldy #>raster_top
        stx $0314
        sty $0315

        jmp $ea81               ; Exit the interrupt

And so one can chain several raster interrupts. The important thing
here is:

- The `$0314/$0315`_ vector contains the callback address (IRQ)
- ACK (clean / accept) `$d019`_ when they call us on the interrupt
- Enable raster interrupt with `$d01a`_
- Use `$d012`_ to say on which rasterline the interrupt has to be triggered
- Exit the interrupt with a ``jmp`` to `$ea81`_ or `$ea31`_
- The border color is changed with `$d020`_. Use `$d021`_ for background color

Timers
~~~~~~

Interrupts with timers work very similar to the raster interrupts.
Instead of calling us when the rasterline has some value, we
get called when a certain number of CPU cycles pass.

The way of using them is very similar. Ex:

.. code:: asm

    setup_irq:
        sei
        ldx #<timer_top        ; Address of our callback (IRQ)
        ldy #>timer_top
        stx $0314               ; IRQ vector lo
        sty $0315               ; IRQ vector hi

        ldx #$c7                ; CIA 1 - Trigger timer
        ldy #$4c                ; in $4cc8 cycles (set to one less.)
                                ; Ex: use $4cc7 to count $4cc8 cycles
        stx $dc04
        sty $dc05

        lda #$81
        sta $dc0d               ; To turn on CIA1 interrupts

        lda #$11
        sta $dc0e               ; Hold timer A

        cli
        rts

    timer_top:
        lda $dc0d               ; ACK timer interrupt

        jsr $1003               ; Play music

        jmp $ea81               ; Exit interrupt

-  `$dc0e`_ is used to activate Timer A. It can be "single-shot" or "continuous"
-  `$dc0d`_ is used to enable CIA1 interrupts
-  `$dc04`_ / `$dc05`_ is used to tell you how many cycles to count
    before firing the callback (IRQ)

And that's how interrupts are used. In fact Raster and timer interrupts
can be used at the same time. Both share the same callback, so to
tell if it was a raster or timer interrupt you can do the following:


.. code:: asm

    irq:
            asl $d019                       ; ACK raster interrupt
            bcs raster                      ; Carry will be set if the interruption
                                            ; was a raster interrupt

            lda $dc0d                       ; ACK timer interrupt
            jsr $1003                       ; Ex: play music with the timer interrupt
            jmp end

    raster:
            jsr animate_scroll              ; Ex: Animate scroll with the raster interrupt

    end:
            jmp $ea81

Timers for the Sid
------------------

Now that we know how to use the timers, let's see how they are used to play a
sid at the correct speed on both platforms.

Assuming the sid was generated for PAL, the formula for converting
to NTSC is:

-  ``((speed_of_timer + 1) * 1022727/985248) - 1``

And to convert to Drean is similar:

-  `` ((speed_of_timer + 1) * 1023440/985248) - 1``

*Note*: ``985248``, ``1022727``, ``1023440`` are the speeds of the 6510
In a PAL, NTSC, Drean respectively (``0.985248`` Mhz, ``1.022727``
Mhz, "1.023440" Mhz). As you can see, the fastest of all is the Drean, and
The slowest is PAL.

To know the speed of the timer, it is necessary to notice in the code of the sid
and see if it modifies the values ​​of the CIA timer. For example, if you see something
like:

.. code:: asm

        ldx #$c7            ; Store $4cc7 in Timer A - CIA 1
        ldy #$4c            ; $4cc7 is on tick per refresh in PAL
        stx $dc04           ; Timer A lo
        sty $dc05           ; Timer A hi

If the sid is using ``$4cc7`` on the timer (a 'tick' of
screen in PAL), then the new timer value for NTSC will be:

-  ``($4cc7 + 1) * 1022727 / 985248 - 1 = $4fb2``

The ``+1`` is because the timer expects "number of cycles - 1".

.. code:: asm

        ldx #$b2            ; Store $ 4fb2 in Timer A - CIA 1
        ldy #$4f            ; $4fb2 sets correct speed for NTSC
        stx $dc04           ; Timer A lo
        sty $dc05           ; Timer A hi

The value for Drean is: ``$4fc1``.

As you can see the speeds of Drean and NTSC are very similar. In fact the
Frequency tables are very similar to each other as well.

In the case of the Player, and since we had no free memory, Drean and
NTSC use the same frequency table.

Detecting between PAL, NTSC and Drean
-------------------------------------

The other important thing is how to detect if a machine is Drean, NTSC or
PAL.

The trick is as follows. Each of these machines has a different screen
resolution:

- PAL: 312 x 63
- NTSC: 263 x 65
- Drean: 312 x 65

This is measured in CPU cycles. In a PAL machine, to refresh the entire screen
it takes 312 x 63 = 19,656 ($4cc8) cycles. Do you hear the number
``$4cc8``? It's the one we used on the timer to play music at
PAL speed (``$4cc8 - 1``, ​​since in the timers you subtract 1 to get
the desired value). That means if I set the timer to
``$4cc7``, on a PAL machine it will be called once per screen refresh.

The other thing to know is that one can read on which rasterline
the raster is on. The raster is the beam of light that sweeps
the screen from left to right, top to bottom.

By joining these two things, one can know if the machine is PAL, Drean or
NTSC.

The trick works like this:

- I wait for the raster to be on line 0 (read `$d012`_)
- Once it's there, I fire the CIA timer with ``$4cc7``
- When the timer calls me, it will have given just one whole loop and `$d012`_
  will be 0, for a PAL machine.

But what value should it have for an NTSC?

The NTSC has a resolution of 263 \* 65. That is 17095 cycles are
required to draw the entire screen. If the timer is set to 19656
cycles, then there is an overflow of:

- 19656 - 17095 = 2561 cycles

Since the NTSC has 65 cycles per line, if I divide that value by 65, I
get:

- 2561 cycles / 65 cycles = 39.4.

So, the raster after 19656 cycles will have drawn a full screen
and will be somewhere on rasterline 39. The formula is similar
for Drean (you, the reader, can try this).

The code that detects PAL / NTSC / Drean is as follows:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; char ut_detect_pal_paln_ntsc(void)
    ;------------------------------------------------------------------------------;
    ; Count how many rasterlines are drawn in 312 * 63 (19656) cycles
    ; 312 * 63-1 is used in the Timer of the CIA, because I expect the timer to be one less
    ;
    ; In PAL,      (312 * 63)  19656/63 = 312  -> 312 % 312   (00, $00)
    ; In PAL-N,    (312 * 65)  19656/65 = 302  -> 302 % 312   (46, $2e)
    ; In NTSC,     (263 * 65)  19656/65 = 302  -> 302 % 263   (39, $27)
    ; In NTSC Old, (262 * 64)  19656/64 = 307  -> 307 % 262   (45, $2d)
    ;
    ; Return values:
    ;   $01 --> PAL
    ;   $2F --> PAL-N (Drean)
    ;   $28 --> NTSC
    ;   $2e --> NTSC-OLD
    ;
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ut_detect_pal_paln_ntsc:
            sei                             ; Disable Interrupts

            lda #0
            sta $d011                       ; Turn off screen to disable badlines

    :       lda $d012                       ; Wait for the raster to reach rasterline 0 (more stable)
    :       cmp $d012
            beq :-
            bmi :--

            lda #$00
            sta $dc0e                       ; Stop Timer A

            lda #$00
            sta $d01a                       ; Disable raster IRQ
            lda #$7f
            sta $dc0d                       ; Disable Timer on CIA 1
            sta $dd0d                       ; and CIA 2


            lda #$00
            sta sync

            ldx #<(312*63-1)                ; Set timer for PAL
            ldy #>(312*63-1)
            stx $dc04                       ; Timer A lo
            sty $dc05                       ; Timer A hi

            lda #%00001001                  ; one-shot
            sta $dc0e

            ldx #<timer_irq
            ldy #>timer_irq
            stx $fffe                       ; When the BASIC/KERNAL are mapped out
            sty $ffff                       ; use $fffe/$ffff instead of $0314/$0315

            asl $d019                       ; ACK raster interrupt
            lda $dc0d                       ; ACK Timer CIA 1 interrupt
            lda $dd0d                       ; and CIA 2

            lda #$81
            sta $dc0d                       ; Enable timer interrupt on A
            cli                             ; CIA 1

    :       lda sync
            beq :-

            lda #$1b                        ; Enable screen again
            sta $d011
            lda ZP_VIC_VIDEO_TYPE           ; Load and return the return value
            rts

    timer_irq:
            pha                             ; Restore "A"

            lda $dc0d                       ; ACK Timer interrupt

            lda $d012
            sta ZP_VIC_VIDEO_TYPE

            inc sync
            cli

            pla                             ; Restore "A"
            rti                             ; Restore "PC" and "Status"

    sync:  .byte $00

With this we should be able to play sids on any machine at
a correct speed.

Update song / author name
-------------------------

Perhaps the most tedious part of all the Player is to update the
Names of the song and author. Let's see why:

Bitmap mode works by cells. The screen is divided into:

- 40 x 25 cells
- Each cell is 8x8 pixels (8 bytes)
- Each cell can not have more than 2 colors

.. figure:: https://lh3.googleusercontent.com/W9abCQZfIhLIFlxyodyd5BhMr0JioeCj9SSTgwhjkqfB0KH1J8PEta4SsS_tq7w8GiEXaOY0WFuobe1ngDv3vBwjgLs3MJMa5cpFkBjdFfbnC8AP6umui1-s8R0H8urtX1WG7_c
   :alt: cells

   *In Standard Bitmap mode cells can not have more than 2 colors at once*

The total graphic uses 16 colors. But if you pay attention, each cell
has no more than 2 colors at a time. This graphical mode exists to
save memory. For example, if one could choose 16 colors (4 bits)
per pixel, then the graph would occupy:

-  (320 \* 200 \* 4 bits) / 8 = 32000 bytes.

Something very expensive for a 64k RAM computer. In addition,
VIC can not see more than 16k at a time. Added to that if one uses
BASIC, then it only has 38k free. That is why his graphic mode
does not exist in the C64.

When using cells, the foreground and background color is stored in
A buffer of 40 x 25. Each byte represents the color of the cell: the 4
High bits are "foreground", and the 4 low bits are the "background".
With this a bitmap + color graphic occupies:

-  ((320 \* 200 \* 1 bit) / 8) + (40 \* 25) = 9000 bytes.

And 9000 bytes is somewhat acceptable for a 64k RAM machine.

To turn a pixel on at x,y and color it, works like this:

.. code:: c

    // pseudo code
    void set_pixel(int x, int y)
    {
            // x goes from 0 to 319
            // y goes from 0 to 199

            // get the corresponding cell
            int cell_offset = 40 * (y / 8) + (x / 8);

            // inside that cell, find the corresponding byte
            int byte_offset = y % 8;

            // within that byte, find the corresponding bit
            int bit_offset = x % 8;

            bitmap[cell_offset + byte_offset] |= bit_offset;
    }

    void set_cell_color(int x, int y, int foreground, int background)
    {
            // x goes from 0 to 39
            // y goes from 0 to 24

            offset = y * 40 + x;
            color = (foreground << 4 | background);

            screen_ram[offset] = color;
    }

Now that we know how to turn on (and turn off) a pixel, what we need to do
is drawn the letters diagonally. If we look at the graphic
We see that it has an inclination of:

-  vertical: of 1 x 1. straight: ``Y = -X``. Slope of -1
-  horizontal: of 2 x 1. straight: ``Y = X/2``. Slope of 0.5

.. figure:: https://lh3.googleusercontent.com/TpaSLAM6xyEgB80FWG8R8QsEKmNvBfuTrYpy8bwkECpVF4dtFZs3NqCkKw98dC-PzjtZMu3-ZKEC5Fs3wsyI1aatB9z0r5MyStkOsJOU0gj2SNlNIld4ztQdSXXq6SipWNktL2k
   :alt: Tilt

   *The slope that we want*

Basically, what we want to accomplish is something like this:

.. figure:: https://lh3.googleusercontent.com/j-TXraycC52OgY3wO-9OTl2wf6X0q1F3jmr5ygvRwJ-NFfd99OicecuzuUa1viUYF3nWsCighJtpFf0QXqXyTpcNY0HWgakFwZ43-jjrcvfx5UYty7IL4T-hMvk6cjprPMxf5LU
   :alt: result

   *Example of how it should be for the tilt of the letters*

The algorithm to draw the letters would look something like this:

.. code:: c

    // pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // start from cell 14 horizontal
        int offset_pixel_y = 3 * 8;     // start from cell 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char(name[i], x, y);
            x += 8;                     // next char starts: 8 pixels on the right
            y += 4;                     // and 4 pixels below
        }
    }

But the hard thing is to implement ``plot_char()``. If we did not have to
tilt the char, the solution would look something like this:

.. code:: c

    // pseudo code
    void plot_char_normal(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // each char occupies 8 bytes.

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

But what we want to do is print it with a slope. The solution is
similar, but every now and then we have to go down and then left:

.. code:: c

    // pseudo code
    void plot_char_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // each char occupies 8 bytes.

        // fix_x / fix_y are the ones that will give the tilt effect
        int fix_x = 0;
        int fix_y = 0;

        // iterate over all pixels of char
        for (int y=0; y<8; y++)
        {
            for (int x=0; x<8; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y + fix_y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y + fix_y);

                // Go down one pixel (Y) for every two horizontal pixels (X)
                fix_y = x/2;
            }
            // the next row has to start one pixel to the left
            fix_x--;
        }
    }

With this algorithm we can print things like this:

.. figure:: https://lh3.googleusercontent.com/_egTNJbWjoF0tImd_bbporzfdvE9Vp74q3gIM2ezwOWU4GRYUeLZzWeGJMk6vM4vPHnGC_Tqqtxmiz5HQMHSBRoiAtADyQtZyapK1bQFKFCJA1nl2iIoChVXAujdJ6LSvSq5AHg
   :alt: Sloping fat

   *Letters have empty pixels in the middle*

But that is **NOT** what we want because:

-  It occupies a lot of screen space, they will not enter the names of the
   songs
-  There are empty pixels in the middle of the letters

And why are there empty pixels? The answer is to see this rotation:

.. figure:: https://lh3.googleusercontent.com/K4ylCjj6GgzdI9DEhTjikkcc14C_bnQEHCBk1OvXtOh3ReUK28f0vTnyGnyu6Q1x67mLLNw5qUuec_CtAWUztv-5wFeDvf7LKpq2-KDqtn_qw93OUAQmhNGKJU0pKg8QpQc6N-U
   :alt: rotated

   *Why the empty pixels*

The algorithm does what we tell it to do, but it is not what
we want. The first thing to do, is to use fonts of 4x8 (and not of 8x8)
since it does not occupy as much screen space. The second is to fix the
empty pixels.

A possible solution to avoid empty pixels is to have the algorithm
tilt the chars horizontally rather than vertically. Something like
this:

.. figure:: https://lh3.googleusercontent.com/gcnEulu7AuMlM2TmwusHLe5-iS3UqUVeTJnHFhKT9d_9JjqdCG7_nFijuyWpQKHzGVeTGfXlbbF-mOi_Y-TRxyuTs1H-xy-BUqfz55rMitmiSJApwRI5M_BTRTzDR47oRk1_iw8
   :alt: rotated2

   *Alternative to avoid empty pixels*

And four letters would look like this:

.. figure:: https://lh3.googleusercontent.com/ViP4RjGdqlvh1B55Q4laIg2S95S6DivApYRuGMOKpK3LnukRebGh410rSkSc5hLb12fu24FMeHuDILaAozN-UK7WX6QgCGqFZZXcKAQ6rC2idlGnCbqJY4Sr9_MPiUCWKScE4Q0
   :alt: rotated3

   *Empty pixels are at the end of each letter*

What we want to do is have the empty pixels be like
"Separators" of the characters, and not be in the middle of
each character. With this in mind, the new algorithm looks like this:

.. code:: c

    // pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // start from cell 14 horizontal
        int offset_pixel_y = 3 * 8;     // start from cell 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char_semi_inclinado(name[i], x, y);
            x += 4;                     // next char starts: 4 pixels on the right
            y += 2;                     // and 2 pixels below
        }
    }

    void plot_char_semi_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // each char occupies 8 bytes.

        // fix_x gives tilt effect in X
        int fix_x = 0;

        // iterate over all pixels of char
        for (int y=0; y<8; y++)
        {
            // from 0 to 4, since char now occupies half
            for (int x=0; x<4; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y);
            }
            // the next row has to start one pixel to the left
            fix_x--;
        }
    }

What you have to do now is to have a charset [#]_ that tilts
only horizontally. For example, a charset like
this:

.. figure:: https://lh3.googleusercontent.com/bEDUkJFBU44Uc6vjfmyCPDHVO3jrSTvW0SQzBSoYsQkwuZ7Q1ij8Gl0K6SBfm0LyD8yg6ZaEHsOsJqAgpd2g0CUZUZ1Wvowg72MaX9JjW7GZ058yNLQrtgURQ7NyFOe7RhYbwmI
   :alt: charset

   *Complete charset with letters ready to be tilted*

And so are some of the sloping letters:

.. figure:: https://lh3.googleusercontent.com/K2eFlXjp7iAn72AjmoREX7GsKBPSxmnSi6s02-fFhtfw0JZhdNG1EnyGPJG_KEYPS6T5pBR3ZhmEaeTsH-7dyogYnlm-J7oFN6gjcYB9k_VeY0UJs8Yy0cES7uGD_NMaLhMFTxk
   :alt: charset\_rotated

   *Example of how 'a', 'b', 'c' and 'd' look like*

But we need to figure out wide letters like ``m``, ``M``, ``W``
and ``w``. This is solved by using two chars for those letters
and let the letters occupy 8x8 and not 4x8. It would be like this:

.. figure:: https://lh3.googleusercontent.com/5fnDgzMLnIjb6wNdSE-WdqTxR1lvl42si2gr57JpF_fXMd5J7g0SrG6yuCjTV9TLjMq-gJOvHk4kTEIIPvhGVzybZgPbSUz9PtkdIty4QYurb_gF6rGc40XLvrDFzeZJlAuP1Wc
   :alt: m\_rotated

   *Composing the M*

Then, the final algorithm is:

-  An 8x8 charset is used. But most of the letters are 4x8.
   The right side of most letters is empty
-  The 8x8 pixels of the letters are copied using the algorithm of
   ``Semi_inclination``
-  Some letters like the ``m`` and ``w`` will use two characters. Ex:
   ``Mama`` is written as ``m&am&a``, since char ``&`` will have the
   second part of the the ``m``

So the code is quite simple, which is good (minus
bugs), but it puts more effort into the data. But it's 10 times better
to have simple code and complex data, than the other way around.

Final algorithm to print the sloped letters:

.. code:: c

    // pseudo code
    void plot_name(char* name)
    {
        int offset_pixel_x = 14 * 8;    // start from cell 14 horizontal
        int offset_pixel_y = 3 * 8;     // start from cell 3 vertical

        int l = strlen(name);
        for (int i=0; i<l; ++i)
        {
            plot_char_semi_inclinado(name[i], x, y);
            x += 4;                     // next char starts: 4 pixels on the right
            y += 2;                     // and 2 pixels below
        }
    }

    void plot_char_semi_inclinado(char c, int offset_x, int offset_y)
    {
        char* char_data = charset[c * 8];   // each char occupies 8 bytes.

        // fix_x gives tilt effect in X
        int fix_x = 0;

        // iterate over all pixels of char
        for (int y=0; y<8; y++)
        {
            // from 0 to 8. The integer char is copied
            for (int x=0; x<8; x++)
            {
                if (char_data[y] & (1 << (7-x))
                    set_pixel(offset_x + x + fix_x, offset_y + y);
                else
                    clear_pixel(offset_x + x + fix_x, offset_y + y);
            }
            // the next row has to start one pixel to the left
            fix_x--;
        }
    }

Optimized Version
~~~~~~~~~~~~~~~~~

The above algorithm works fine, but the problem is that it uses a lot
of multiplication in ``set_pixel()`` [#]_, and remember that the 6510
has no multiplication instructions.

The Player uses a slightly more complicated version to improve the
performance. It takes into account the following:

-  Characters can only start in the following offsets
   relative to the cells: (0,0), (4,2), (0,4), (4,6)
-  A character needs two cells to be printed. These cells are
   contiguous.
-  The next character to print will be, at most, a cell's
   distance in both X and Y
-  There are specific functions to draw the possible 4 offsets
   ``plot_char_0()``, ..., ``plot_char_3()``
-  There are specific functions to draw each of the 8 rows:
   ``plot_row_0()``, ..., ``plot_row_7()``
-  There are three global pointers:
   - ``$f6/$f7`` charset offset pointing to the character to be printed
   - ``$f8/$f9``, and `` $fa/$fb`` pointing to the current cell, and
      next cell in the bitmap

With that in mind, it is not necessary to calculate the offset of the pixels for
every pixel and that saves CPU as there are no multiplications in between.
Although it adds complexity.

Here's how the optimized algorithm works (pseudo code):

.. code:: c

    // pseudo code

    // global: points to the beginning of the bitmap
    #define ORIGIN_CELL_X = 14;
    #define ORIGIN_CELL_Y = 3;

    // in the code in assembler, these two variables are represented
    // with `$f8/$f9` y `$fa/$fb`
    int g_bitmap_offset_0, g_bitmap_offset_1;

    void plot_name(char* name)
    {
        int l = strlen(name);
        int idx = 0;

        // initialize offset bitmap with cell source
        g_bitmap_offset_0 = ORIGIN_CELL_Y * 40 + ORIGIN_CELL_X * 8;
        g_bitmap_offset_1 = ORIGIN_CELL_Y * 40 + (ORIGIN_CELL_X + 1) * 8;
        char c;

        while (no_se_hayan_impreso_todos_los_chars) {

            c = fetch_next_char();
            plot_char_0(c);     // print first char (offset 0,0)

            c = fetch_next_char();
            plot_char_1(c);     // print second char (offset 4,2)

            bitmap_next_x();    // cell_x++ (update g_bitmap_offsets)

            c = fetch_next_char();
            plot_char_2(c);     // print third char (offset 0,4)

            c = fetch_next_char();
            plot_char_3(c);     // print fourth char (offset 4,6)

            bitmap_next_x();    // cell_x++ (update g_bitmap_offsets)
            bitmap_next_y();    // cell_y++ (update g_bitmap_offsets)
        }
    }

    // prints char at offset 0,0
    void plot_char_0(char* char_data)
    {
        plot_row_0(char_data[0]);

        bitmap_prev_x();        // cell_x-- (update g_bitmap_offsets)

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

        bitmap_prev_x();        // cell_x-- (update g_bitmap_offsets)

        plot_row_7(char_data[5]);

        bitmap_next_y();        // cell_y++ (update g_bitmap_offsets)

        plot_row_0(char_data[6]);
        plot_row_1(char_data[7]);

        // restore pointers
        bitmap_next_x();
        bitmap_prev_y();
    }

    void plot_char_2(char* char_data)
    {
        // and so on until the plot_char_3()
        ...
    }

    void plot_row_0(char c)
    {
        g_bitmap[g_bitmap_offset_0] = c;
    }

    void plot_row_1(char c)
    {
        rotate_left(c, 1);              // character is rotated one place to the left

        // actualizo celda izquierda
        char value_izq = g_bitmap[g_bitmap_offset_0];
        value_izq &= 0b11111110;        // I turn off the 1st bit LSB
        value_izq |= (c & 0b00000001);  // put what is in the 1st bit LSB of char
        g_bitmap[g_bitmap_offset_0] = value_izq;

        // actualizo celda derecha
        char value_der = g_bitmap[g_bitmap_offset_1];
        value_der &= 0b00000001;        // I turn off the first 7 bit MSB
        value_der |= (c & 0b11111110);  // I put what is in the first 7 bit MSB of char
        g_bitmap[g_bitmap_offset_1] = value_der;
    }

    void plot_row_2(char c)
    {
        rotate_left(c, 2);              // character is rotated two places to the left

        // update left cell
        char value_izq = g_bitmap[g_bitmap_offset_0];
        value_izq &= 0b11111100;        // I turn off both LSB bit
        value_izq |= (c & 0b00000011);  // put what is in the two LSB bits of char
        g_bitmap[g_bitmap_offset_0] = value_izq;

        // update right cell
        char value_der = g_bitmap[g_bitmap_offset_1];
        value_der &= 0b00000011;        // I turn off the first 6 bit MSB
        value_der |= (c & 0b11111100);  // put what is in the first 6 bit MSB of char
        g_bitmap[g_bitmap_offset_1] = value_der;
    }

    void plot_row_3(char c)
    {
        // and so on until the plot_row_7 ()
        ...
    }

These same ideas (more or less) is how the Player works,
but in assembler. With this it was possible to avoid multiplication.

For those who want to see the complete code in assembler, here:

-  `plotter in
   assembler <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/src/chipdisk.s#L1313>`__

It is not worth putting it here, except for some interesting things, such as
macros that are used. For example, instead of repeating code over and over,
Chipisk uses assembler macros.

It is worth highlighting the ``.IDENT``, ``.CONCAT`` that is used to call
the correct functions according to the parameters that are passed to the
macro. Let's see how it works:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; Macros
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; entry:
    ;   number_of_rows: how many rows to print
    ;   char_y_offset: char offset to print
    ;   cell_y_offset: cell offset Y
    ;   cell_x_offset: cell offset X. This is used to call plot_row_xxx
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    .macro PLOT_ROWS number_of_rows, char_y_offset, cell_y_offset, cell_x_offset
            .repeat number_of_rows, YY
                    ldy #char_y_offset + YY
                    lda ($f6),y                 ; $f6 points to charset data
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
    ; Functions
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
            ; And so on to plot_char_3
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
            sta ($f8),y                 ; You do not have to rotate anything
            rts                         ; So print it directly

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; plot_row_1
    ; entry:
    ;       A = byte to plot
    ;       Y = bitmap offset
    ;       $f8,$f9: bitmap
    ;       $fa,$fb: bitmap + 8
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    plot_row_2:
            .repeat 1                       ; Rota character 1 position
                    asl                     ; on the left
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
            .repeat 2                       ; Rotate character 2 positions
                    asl                     ; to the left
                    adc #0                  ; the "adc" puts on the right what came out
                                            ; from the left
            .endrepeat

            tax                             ; save for next value
            PLOT_BYTE $f8, %00000011

            txa
            PLOT_BYTE $fa, %11111100

            rts

    plot_row_3:
            ; And so on to plot_row_7
            ...

Some tricks we use:

Trick: Rotate In-Place
^^^^^^^^^^^^^^^^^^^^^^

The trick we use to rotate "in-place" [#]_ is nice:

.. code:: asm

            asl                     ; It rotates a bit to the left. "C" has the value of bit 7.
            adc #0                  ; And bit 0 has the value of "C"

Trick: Unrolled-loops
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*Unrolled loops* are used a lot within games/demos/intros they
help achieve fast code (in exchange for RAM space):

A normal *loop* looks like this:

.. code:: asm

                lda #$20                ; Puts a $20 from $0400 to $04ff
                ldx #0
        l0:     sta $0400,x             ; Takes 5 cycles, occupies 3 bytes
                dex                     ; Takes 2 cycles, occupies 1 byte
                bne l0                  ; Takes 2 cycles, occupies 2 bytes

The loop is repeated 256 times, so the *loop* takes (5 + 2 + 2) * 256 = 2304 cycles
and occupy 6 bytes.

One way to do it much faster is with an unrolled loop:

.. code:: asm

        lda #$20                        ; Puts a $20 from $0400 to $04ff
        sta $0400                       ; Takes 4 cycles, occupies 3 bytes
        sta $0401                       ; Takes 4 cycles, occupies 3 bytes
        sta $0402                       ; Takes 4 cycles, occupies 3 bytes
        ...
        sta $04fe                       ; Takes 4 cycles, occupies 3 bytes
        sta $04ff                       ; Takes 4 cycles, occupies 3 bytes

In this way the *unrolled loop* takes 4 * 256 = 1024 cycles, but occupies
256 * 3 = 768 bytes.

A more maintainable way of writing *unrolled loops* is, at least with
cc65_, is as follows:

.. code:: asm

        lda #$20
        .repeat 256, XX
                sta $0400 + XX
        .endrepeat

You will see that inside the Chipdisk code this is used a lot. Just search for
``.repeat`` to see how many times it is used. But to be honest
I'm not sure that Chipdisk requires so many *unrolled loops*.


Trick: Add 320
^^^^^^^^^^^^^^

The other thing to speed up, is how ``bitmap_next_y() `` works. What
it does is add ``320`` to the pointer ``$f8/$f9``. And as ``320 = 256 + 64``,
It does this by adding 64 to ``$f8`` and incrementing ``$f9``.

.. code:: asm

    bitmap_next_y:
            clc                             ; Clear Carry for the sum
            lda $f8                         ;
            adc #64                         ; Add 64 to $f8 and save the carry
            sta $f8                         ; save the value in $f8

            lda $f9                         ; increment $f9 with 1 + carry
            adc #1
            sta $f9                         ; save the value in $f9


Reading mouse, joystick and keyboard
------------------------------------

The Player supports 3 methods to control the "arrow":

- Joystick in port #2
- Mouse in port #1
- Keyboard

Joystick
~~~~~~~~

Reading the joystick is relatively simple on the C64. The values of the
Joystick 1 are in `$dc01`_ and those in Joystick 2 are in `$dc00`_

.. code:: asm

        ldx $dc00                       ; "X" has the value of joystick #2
        ldy $dc01                       ; "Y" has the value of joystick #1

The possible values ​​are:

+-----------+---------------------------------+
|$dc00/$dc01| Significado                     |
+===========+=================================+
| Bit  4    | Joystick Button: 0 = Active     |
+-----------+---------------------------------+
| Bit  3    | Joystick Right: 0 = Active      |
+-----------+---------------------------------+
| Bit  2    | Joystick Left: 0 = Active       |
+-----------+---------------------------------+
| Bit  1    | Joystick Down: 0 = Active       |
+-----------+---------------------------------+
| Bit  0    | Joystick Up: 0 = Active         |
+-----------+---------------------------------+

Important: 0 means it is on, and 1 is off. If you want
check if the Joystick 2 button is pressed, the code is:

.. code:: asm

        lda $dc00                       ; Read status of Joystick 2
        and #%00010000                  ; I'm just interested in the button status
        beq boton_apretado              ; If it is 0 then the button is pressed

And something similar for Joystick 1, but with `$dc01`_ instead of `$dc00`_.

Keyboard
~~~~~~~~

The keyboard is a little more complicated ... or not, it depends on what you
need. There is a KERNAL function that returns the pressed key: `$ffe4`_

.. code:: asm

        jsr $ffe4                       ; Returns in A the keyboard byte read

And using the KERNAL for this is more than fine for most
cases. The Player, however, uses the other option that is reading the
"Hardware" directly, and it works like this:

- The keyboard of the Commodore 64 has 64 keys (not counting RESTORE)
- The keys are arranged in an 8 x 8 matrix (8 \* 8 = 64)
- `$dc01`_ contains the values ​​of the columns
- and `$dc00`_ contains the values ​​of the rows

You can determine which keys are pressed by reading the
following matrix:

+---------------+--------------------------------------------------------------------------------+
|Keyboard 8x8   |                                      $DC01                                     |
|Matrix         +---------+---------+---------+---------+---------+----------+---------+---------+
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

If we want to know if the key ``Q`` was pressed then we must do the following:

.. code:: asm

        lda #%01111111              ; Row 7
        sta $dc00
        lda $dc01
        and #%01000000              ; Column 6
        beq tecla_apretada          ; If it is 0, then it was pressed

Like the joystick, a value of 0 indicates that it was pressed, and a 1 indicates that it was not.

**IMPORTANT**: The joysticks and keyboard share the same controller (CIA)
So you must differentiate between a joystick movement and keys pressed
sometimes it gets complicated. Note that both use both `$dc00`_ and `$dc01`_ for
reading the data.

If we want to know if the *cursor left* is pressed, then we must
check if the *Shift* and *cursor left / right* keys are pressed.
To detect that, in the Player we do this:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; read_keyboard
    ;
    ; Check whether cursor right or left was pressed
    ;
    ; A = 0 Nothing was pressed
    ; A = 1 Right cursor was pressed
    ; A = 2 Left cursor was pressed
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    read_keyboard:
            ; IMPORTANT: the bits are inverted in the CIA (0 = on, 1 = off)

            NoKey    = 0
            LeftKey  = 1
            RightKey = 2


            ; Check the left shift
            lda #%11111101    ; Row 2
            sta $dc00
            lda CIA1_PRB
            and #%10000000    ; Col 7
            beq :+

            ; Check for right shift
            lda #%10111111    ; Row 6
            sta $dc00
            lda CIA1_PRB
            and #%00010000    ; Col 4
            beq :+
            lda #$ff          ; Shift not pressed
    :       sta shift_on

            ; Check cursor left / right
            lda #%11111110    ; Row 0
            sta $dc00
            lda CIA1_PRB
            and #%00000100    ; Col 2
            cmp keydown
            bne newkey
            lda #NoKey        ; Nothing was pressed
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

The player can use the mouse as well. It is not very common to use mouse on
the C64, but if you have a Commodore 1351, you can use it. Reading the
mouse is not so complicated, but it is different than joystick.

The first thing to do is tell the CIA that Port 1 (or 2) is going
to use the mouse. Then the *delta x* is read from `$d419`_ and the *delta y*
is read from `$d41a`_ (which are sound chip registers).

The mouse is activated with `$dc00`_.

.. code:: asm

        lda #%01000000                  ; Enable mouse
        sta $dc00                       ; on port 1

        ; After using the mouse, it is disabled as follows

        lda #%00111111                  ; enable joystick
        sta $dc00                       ; on port 1


This is the routine that the Player uses: read the deltas, and check if the button
was pressed

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; read_mouse
    ;       exit    x = delta x movement
    ;               y = delta y movement
    ;               C = 0 if button pressed
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    read_mouse:
            lda $d419                       ; Read delta X (pot x)
            ldy opotx
            jsr mouse_move_check            ; Calculate delta
            sty opotx
            sta ret_x_value

            lda $d41a                       ; Read delta Y (pot y)
            ldy opoty
            jsr mouse_move_check            ; Calculate delta
            sty opoty

            eor #$ff                        ; Delta is inverted ... fix it
            tay
            iny

            sec                             ; C = 1 (means button not pressed)

    ret_x_value = * + 1
            ldx #00                         ; self modifying

            lda $dc01                       ; Read joy button # 1: bit 4
            asl
            asl
            asl
            asl                             ; C = 0 (means button was pressed)
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

To better understand how to enable/disable the mouse/joystick.
This is how the ``main_loop()`` of the Player works:

.. code:: asm

    main_loop:
        ...

        lda #%01000000                  ; Enable mouse
        sta $dc00                       ; (disable joystick)

        jsr read_mouse
        jsr process_mouse

        jsr read_keyboard
        jsr process_keyboard

        lda #%00111111                  ; Enable joystick
        sta $dc00                       ; (disable the mouse)

        jsr read_joystick
        jsr process_joystick

        ...
        jmp main_loop




Animate Pressed Buttons
-----------------------

We are not doing anything strange here. We simply replace a bitmap piece
for another.

.. figure:: https://lh3.googleusercontent.com/gGQcvRrOcIv8tWfcliz_qTAveG2UALJxt9JYd-3JjOKYBzqM9FBiZ0U6nZMknEQt-87LYgH-H_OVP-V_HlMEr4W93M4H1WHOXkL2atCm5TePAqrK2s8CGaXHBg6apUN75M1xnzA
   :alt: 7x7 cells

   *Copies a block of 7x7 cells*

The algorithm looks something like this:

1. The button that is pressed (if any) is replaced by the contents of the temporary buffer
2. The content of the button to be pressed is copied to the buffer
3. Copy the contents of the pressed button to destination

What is copied is a 7x7 block for each button. Both the bitmap
and its color. Each button occupies:

- bitmap: 7 * 7 * 8 (8 bytes per cell) + color: 7 * 7 = 441 bytes

There are 4 buttons that we animate: *Play*, *FF*, *Rew* and *Stop*, and we use a temporary buffer.
So in total we use 441 * 5 (2205) bytes of data for this.

The code in assembler is made with macros:

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


Code: The Easter Egg
====================

.. figure:: https://lh3.googleusercontent.com/Zp52TSOw_i2SzQ9zJhI0Fl28joPzCKIpYGy4v52h4r2AWZVsnXTGAJAh9dxEPs7vhTIv4x0CdGgt55xQcAhK7HoTrVOjsxdmW_cNiF4Yi9BfiLpB43dJ_Gsuoetg5CH5qNnaex8
   :alt: easter egg

The Easter Egg is made with:

- Use text mode (pure PETSCII) for the sun and its animations
- 7 sprites extended in X and Y for the scroll
- Open the vertical edge for use with sprites under the sun
- Play a sid that has to play well in PAL / NTSC / Drean

Vertical Frame
--------------

One way to open the vertical border is more or less like this:

1. 24-row mode is changed when the VIC is drawing row 25 (between rasterlines ``$f2`` and ``$fa``)
2. It is changed to 25 rows mode once the raster has passed row 25.

That has to be done in every frame.

Example:

.. code:: asm

    loop:

            lda #$f9                        ; raster line at $f9?
    :       cmp $d012                       ; wait for it
            bne :-

            lda $d011                       ; Switch to 24 row mode
            and #%11110111                  ;
            sta $d011

            lda #$fc                        ; wait for rater line $fc
    :       cmp $d012
            bne :-

            lda $d011                       ; Switch to 25 row mode
            ora #%00001000                  ; again
            sta $d011

            jmp loop


That is the logic in general. But what needs to be changed is how to wait for the
rasterline ``$f9`` without consuming all the cycles. The simplest way is
with a raster interrupt ... something like:


.. code:: asm

    setup_irq:
            sei
            lda #$f9                        ; Fire IRQ at rasterline $f9
            sta $d012

            ldx #<irq_vector
            ldy #>irq_vector
            stx $fffe                       ; Since BASIC/KERNAL are mapped out
            sty $ffff                       ; Use $fffe/$ffff instead of $0314/$0315
            cli
            rts


    irq_vector:
            pha                             ; Save "A"

            asl $d019                       ; ACK interrupt raster

            lda $d011                       ; Switch to 24-row mode
            and #%11110111                  ;
            sta $d011

            lda #$fc                        ; Wait for rasterline $fc
    :       cmp $d012
            bne :-

            lda $d011                       ; Switch to 25 row mode
            ora #%00001000                  ; again
            sta $d011

            pla                             ; Restore "A"
            rti                             ; Restore "PC" and "Status"

That works in 99% of cases. But remember that we have to play
The sid to work well on PAL, NTSC and Drean. Also for the sid
we have to use a timer to attain the correct speed, a speed
that can be different than the raster irq speed.

Suppose we are running the program in an NTSC (see *Detecting between ...* for more info):

- We will have a timer that fires every ``$4fb3`` (20403) cycles to play the sid
- In addition the IRQ raster fires every ``$42c7`` (263 * 65 = 17095) cycles to open the edge

.. figure:: https://lh3.googleusercontent.com/D50glqRSR3V8MMi-aXe41TiXWk9tHjyTKkTcrhQmUZFfdPHs07WbWRPhok07di0ydzyAkn16MeOLsQzOdxVipXaSjv6diR9pmNJHB2MCG-yg0kSJ8HcqRBvIPInhU3t30N34yXc
   :alt: collision in interrupts

   *Collision between IRQ Raster and IRQ Timer in NTSC. Which one runs first?*

It is possible that the edge will not open at any time because the interruption of the sid
is executed just when you had to call the raster interrupt. In
the animation above the white bar that "low" shows when running
the IRQ Timer and its duration. The little bar below shows the Raster
IRQ. As you can see, sometimes they "collide" and you do not know which one is executed.


NMI Interrupts
--------------

One way to make the border always open is to use the NMI interrupt
(Non-Maskable Interrupt) to trigger the edge code. The NMI interrupt has
priority over other interruptions. If the Raster interrupt is
Is running when the NMI has to be executed, the NMI Interrupt
interrupts the Raster interrupt. But no one can interrupt an
NMI interrupt.

The NMI interrupt can be triggered with the following events:

- Pressing the Restore key
- Hardware
- With CIA Timer A 2: `$dd0d`_ and other friends

In our case, we are going to use Timer A of the CIA 2. It works like this:

.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; init_nmi
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    init_nmi:
                                            ; setup NMI (open border)
            ldx #<nmi_openborder
            ldy #>nmi_openborder
            stx $fffa                       ; Use NMI vector ($fffa/$fffb)
            sty $fffb                       ; And not the IRQ vector ($fffe/$ffff)

            lda #$0                         ; Stop timer A CIA 2
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
            ldx #<$4f37n                    ; Cycles for PAL-N (Drean)
            ldy #>$4f37
            bne @done

    @ntsc:
            ldx #<$42c6                     ; Cycles for NTSC
            ldy #>$42c6
            bne @done

    @ntsc_old:
            ldx #<$417f                     ; Cycles for NTSC-Old
            ldy #>$417f                     ; fall-through

    @done:
            stx $dd04                       ; Timer A: low-cycle-count
            sty $dd05                       ; Timer A: high-cycle-count

            lda #%10000001                  ; Enable interrupt timer A
            sta $dd0d                       ; on CIA 2

    :       lda $d012                       ; Wait for the rasterline to arrive
    :       cmp $d012                       ; at $f9, which is where we want to open
            beq :-                          ; the border
            cmp #$f9
            bne :--

            lda #%10010001                  ; Enable timer A!
            sta $dd0e

            rts

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; nmi_openborder
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    nmi_openborder:
            pha                             ; Save "A"

            lda $dd0d                       ; ACK the interrupt of Timer CIA 2

            lda $d011                       ; Open vertical edge
            and #%11110111                  ; Switch to 24 row mode
            sta $d011

            lda #$fc                        ; Wait for the rasterline to reach $ fc
    :       cmp $d012
            bne :-

            lda $d011                       ; And switch back to 25 row mode
            ora #%00001000
            sta $d011

            pla                             ; Restore "A"
            rti                             ; Restore "PC" and "Status"


And that way the edge is always going to open, regardless of whether the interruption
IRQ is activated.

Scroll with Sprites
-------------------

The Scroll is made with 7 sprites expanded in both X and Y, covering everything
the length of the screen. The length of the screen is 320 pixels. With 7 sprites
expanded in X we cover: 7 * 24 * 2 = 336 pixels.

.. figure:: https://lh3.googleusercontent.com/wqwavZCFHLGy1xzLNMvtDXbfbzDTqjBEZ4rUNuq4R1GR8N-UK4Olh63-YYColFjcexYR_2PnoquipJDkYuf4NDGbcb2hMgCHbeJPDlB2-LriVoEkVfC0c5gpH3xhUwLuBrEBc8Q
   :alt: scroll with sprites

   *Scroll with 7 sprites*

The scroll can not be done with characters because it is done under row
25. The only thing that can be out there are sprites.

The trick is very simple:

1. Put 7 expanded sprites in X, side by side
2. At first the sprites are "empty"
3. Calculate the ``C`` (*carry*) to update the rightmost sprite
4. Each row of the sprite is ``rol``. And ``carry`` is used for the previous column of the same row

A normal text scroll is similar, but instead of scrolling through characters,
Sprites are scrolled. Here is the code:


.. code:: asm

    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    ; animate_scroll
    ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
    animate_scroll:
            ; Uses $fa-$ff as temporary variables
            lda #0
            sta $fa                         ; Temporary variable

            ldx #<CHARSET_ADDR              ; Location of the charset
            ldy #>CHARSET_ADDR
            stx $fc
            sty $fd                         ; $fc/$fd are the pointers to the charset

    load_scroll_addr = * + 1
            lda SCROLL_TEXT                 ; self-modifying
            cmp #$ff                        ; If "char == $ff" then it is the end of the scroll
            bne next
            ldx #0                          ; Reset scroll so it starts again
            stx ZP_BIT_INDEX
            ldx #<SCROLL_TEXT
            ldy #>SCROLL_TEXT
            stx load_scroll_addr
            sty load_scroll_addr+1
            lda SCROLL_TEXT

    next:                                   ; A has the char to draw
            clc                             ; Char_idx * 8, since each char
            asl                             ; Occupies 8 bytes in the charset
            rol $fa
            asl
            rol $fa
            asl
            rol $fa

            tay                             ; Char_def = ($fc),y

            clc
            lda $fd
            adc $fa                         ; A = charset[char_idx * 8]
            sta $fd


            ; Scroll 8 bytes from above
            ; YY =  rows of the sprite. 8 total
            ; SS = sprite number. 7 total
            .repeat 8, YY                   ; "Unrolled loop" to make it faster
                    lda ($fc),y
                    ldx ZP_BIT_INDEX        ; The "C" (carry) is updated with the value
    :               asl                     ; Needed to update the sprite
                    dex                     ; Farther to the right
                    bpl :-

                    .repeat 7, SS           ; Each sprite has 3 "columns". Scroll each of the 3
                                            ; Starting with the right most
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 2
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 1
                            rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 0
                    .endrepeat
                    iny                     ; byte of the char
            .endrepeat


            ldx ZP_BIT_INDEX                ; Bit index goes from 0 to 7
            inx                             ; Is incremented once per "scroll"
            cpx #8                          ; When it reaches 8 (overflow)
            bne l1                          ; Read the following text char
                                            ; of the scroll
            ldx #0
            clc
            inc load_scroll_addr            ; Next char to read from the text
            bne l1                          ; of the scroll
            inc load_scroll_addr+1
    l1:
            stx ZP_BIT_INDEX

            rts

*Note*: Note that we are "rol" at 163 (7 * 8 * 3) bytes per frame, a total
of 978 (163 * 6) CPU cycles. It is not a lot, but it is much more than what is used in
a normal text scroll. If we use the 24 pixels of the sprite,
qould be three times as expensive. Wow!

.. figure:: https://lh3.googleusercontent.com/7j8O3TKZuljEjbSTtlfsd1xLsErRXOsI8W147As4KsvjfNXetZUhP8-BFk3AjiWW1tA7FcGjHrGRQrjOtvjbo38lfcLyaRo1GWP7p_RCFIshxOm3Gb7pOOTug6eVFLZeQ4zcagY
   :alt: rasterbars

   *All of rasterbars. A: Scroll, B: Music, C: Open border*

Unpacking the Easter Egg
-----------------------------

Once the Easter Egg is activated you have to unzip it. The little problem
here is that the Easter Egg is not in a continuous chunk. It is in 3
parts:

- compressed code: ``$0118 - $07ff``
- compressed sid: Somewhere near ``$e000``
- compressed scroll text: Somewhere near ``$f800``

The interesting thing here is the compressed code that uses part of the c64 stack.
To prevent the stack from mangling the compressed code, we tell the stack
That the "top" of the stack is "$0117" with the following instructions:

.. code:: asm

        ldx #$17                        ; Only 24 bytes are used for the stack
        txs                             ; The rest is reserved for the easter egg


The Player's memory with the compressed Easter Egg looks like this:

::

    ; Player Memory with Easter Egg Compressed
    $0000 - $00ff: Zero Page: 256 bytes used for temporary variables
    $0100 - $0117: 24 bytes used for stack
    $0118 - $07ff: Easter Code compressed egg
    $0800 - $0fff: Player Code (1/3)
    $1000 - $32f7: Buffer to touch the largest sid (~ 9k)
    $32f8 - $3fff: Player Code (2/3)
    $4000 - $5fff: Bitmap graphic (8k)
    $6000 - $63ff: Color chart (Screen RAM) (1k)
    $6400 - $68ff: Sprites (~ 1k)
    $6900 - $6cff: Charset (1k)
    $6d00 - $73ff: Sid White Noise (1.7k)
    $7400 - $7caf: Pressed button images + temporary buffer (~ 2k)
    $7cb0 - $fbdf: Compresswd Sids, including the Sid of the Easter Egg (28k)
    $7be0 - $7ccf: Player Code (3/3)
    $fcd0 - $fff0: Text Scroll Easter Egg Compressed (~ 800 bytes)

When the Easter Egg is fired, the three pieces of Easter Egg are decompressed
in the correct location.

Code: The Intro
===============

.. figure:: https://lh3.googleusercontent.com/STIEW1KCcW65Y0U0NMOHebjsrQzkk4IuxsbqR6kVTvzx0O16ZmJYTJ_S0ttv1L5bIv0_Qsg5oGzb3pnVAiJbxBqNMg8HX658PNziScLQB1R3csABQSgB5Pt8nsC-N03Nmv9v_DU
   :alt: intro

The intro is quite simple:

- The top part uses a multi-color bitmap graphic
- The bottom part uses text mode + custom charset
- The texts are colored with inverted rasterbars

Rasterbars
----------

Perhaps the most interesting thing is how to achieve the "inverted color". It's simple:

1. The typical effect of rasterbars is done like so:

.. figure:: https://lh3.googleusercontent.com/IAw3ncdKryj8scLeYa5HpEK-CUChYbFRoTjvT5oNYwmsccQMj6iN1JCmd9xO3aoCUFqJGQEk-rtKjmMECm1hrvyjRILJowwIbQwXc1XvQAJ6AC4Gkj5bnRxty-6gH_WWgHeTIIY
   :alt: intro-rasterbar

2. Text is overwritten

.. figure:: https://lh3.googleusercontent.com/rXTTYVOZKkL5GbZRE01CcRoiRdhjZYQ4rcNd_Y5jlTk9AJvzMIkiMycnhdnJGTlWuqmfsjTELB5Zf_8s43yPObuXVAIGB68sOdWQk43HHUk6KosqHifntMtqmrA9wrKJaAk-FcA

3. An inverted charset is used. That is, the bits that are in 0 happen to 1, and vice versa

.. figure:: https://lh3.googleusercontent.com/0d8Az60NjW3UsVP1Nsc2LGawrjcVQOURv58cpHb4eCFgS9rnyOpUjj92dVuUEUQ6urUC22a7aLwiF2o9Yx_vJmnhvRi-vsdz7kN07dr1dfvCDdY_YgxC1YaqhTcnapGVyfoE6RI


Intro-linker
------------

The Intro, once it is finished, decompresses the Player. For that you have to have
consider two things:

- The decompression code can not get "stepped on" while decompressing
- The data to decompress can not get "stepped on" while it is decompressed

For the first thing, what is done, is to put the decompression code in a
address that will not be used, such as ``$0400`` (Screen address). To
prevent it from being "ugly", we paint everything in black:


.. figure:: https://lh3.googleusercontent.com/Az0sLlckuc5AZ11CAEMfEHt5Qhytwjo5pF8VoXPMUOrXPdhah23WTuGCQ5OHHjImepzFYRMuDoMV6Pj_keYu7i5InAxd5shUcByNSwLibPkMDoTOBi9edcjgBEsKe4IZkIZ9m5U
   :alt: intro-linker black

   *With Everything black. We can not see the decompression code*

But if the background were not black, it would look like this:

.. figure:: https://lh3.googleusercontent.com/4HefvIHOzC2oM23jOjZGUgnR6ChVb4Jj8hm-2_w8MpHTAWKjvFdWt0YDl-KQwV4ox6kVlyNSxbpfqvrtk7KuOesAi8XnnFHebSxmmXL5gk1r5m-ouYBrsAqvgg-X3DKianQIfQs
   :alt: intro-linker not blacked out

   *What you would see if the screen had no black background*

For the second, it is simple too, but it was complicated in the Chipdisk.
The Player + Easter Egg tablets occupy 40585 bytes, and when they decompress
They occupy 63427 bytes. The C64 only has 64k RAM, so the decompression routine
will step on the compressed data at some point. The idea is to step it just one
time you after you have used the compressed data.

The Exomizer_ decompression routine goes from the end
forwards. That is, it begins to decompress the last byte first.

::

        Memory before unzipping the Player + Easter Egg:
        +----+---------------+---------------------------------------------+---------------+
        |    |$0400-$07ff    | $0800 - $afff                               | $b000 - $fff0 |
        |    |Decompressor   | Player Code + Easter Egg                    | Intro         |
        +----+---------------+---------------------------------------------+---------------+

        Memory after decompression:
        +---------------------+------------------------------------------------------------+
        |                     | $0820 - $fff0                                              |
        |                     | Player Code + Easter Egg                                   |
        +---------------------+------------------------------------------------------------+

So to prevent it from treading on the unused data, something like this is done:

-  The address of the last compressed byte is in ``$afff``, and that byte
   once decompressed goes to ``$fff0``. So it does not "step" the compressed data.

-  And the address of the first compressed byte is at ``$0800``, and that byte
   once decompressed goes to ``$0820``. On top of the compressed data, but only
   after is was already decompressed.

**Rules**: Both the address of the first byte of origin and that of the last one
must be less than the addresses of the first and last destination byte
eespectively: ``$0800 < $0820`` and ``$afff < $fff0``.

Stable IRQ Raster
------------------

When one uses raster interrupts, the callback is not always called
where one wants if you use: ``lda #$80; sta $d012``, the
raster will call us when the rasterline is in ``$80``, but in which part
of the rasterline ``$80``? Sometimes it is called in the middle of the line, and other
times ahead or behind.

That makes a simple effect like rasterbar look "unstable" ...
With a line of color split in half, or the like.

Well, something similar happens in the Intro when we change the mode screen
Bitmap to text mode. Sometimes a black line appears / disappears in the rasterline
in between them.

.. figure:: https://lh3.googleusercontent.com/kvWKJs7IEaFXfR8dKVI21ans9NSVY9WMXZ_qr9MuM6ugq7TCIiGyzSkDb-YCMWw_15bN_1TJ-J0FerIf2D8K1j_f37xjTixXUFIP6Bl8E-F89jFnaIJj51qrAsTdTUJSmI_VwCk
   :alt: artifact

   *A black line appears / disappears above the P and other letters. That is the "artifact"*

That is solved with a stable *IRQ raster*. What it does is that the callback
to always be called in the same rasterline cycle. Then you can adjust it
putting in additional ``nop``s. There are different techniques for achieving
a stable raster. Chipdisk uses the technique called "double IRQ".

The code looks like this:

.. code:: asm

        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        ; STABILIZE_RASTER
        ; Routine for a stable raster using Double-IRQ
        ; Sourced from: http://codebase64.org/doku.php?id=base:stable_raster_routine
        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .macro STABILIZE_RASTER
                ; The IRQ Raster is triggered in cycle 0 of the current rasterline ($d012)
                ; But the CPU has to finish the opcode that is running before calling the IRQ
                ; So there may be a delay of 0 to 7 cycles, depending on the opcode
                ; Then there is a delay of about 7 cycles calling the "Interrupt Handler" (Push SR / PC to stack ++)
                ; And then another 13 more cycles are consumed to keep the records (pha, txa, pha, tya, pha)

                ; Cycles consumed so far: 20 ~ 27
                lda #<@irq_stable   ; +2, 2
                ldx #>@irq_stable   ; +2, 4
                sta $fffe       ; +4, 8
                stx $ffff       ; +4, 12
                inc $d012       ; +6, 18
                asl $d019       ; +6, 24
                tsx             ; +2, 26
                cli             ; +2, 28

               .repeat 10
                        ;  The next IRQ will be called while running these nops
                        nop         ; +2 * 10, 48.
                .endrepeat
                ; Cycle count: 68 ~ 75. The new IRQ raster has already been called at this point

        @irq_stable:
                ; Cycle count: 7 ~ 8 .7 cycles for the interrupt handler + 0 ~ 1 cycle Jitter for the NOP
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
        ; IRQ raster splitting screen between bitmap mode and text mode
        ;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        irq_rasterbar:
                pha                             ; Save A, X, Y
                txa
                pha
                tya
                pha

                STABILIZE_RASTER                ; Calls the macro "STABILIZE_RASTER"
                                                ; Which is what makes all the magic

                sei

                ldx #0                          ; Routine that generates the "raster bars"
        @l0:
                lda $d012
        @l1:    cmp $d012
                beq @l1                         ; Wait for new rasterline line
                lda palette,x                   ; And when that happens it changes the color
                sta $d021                       ; $d021 to generate a new line
                inx
                cpx #TOTAL_PALETTE
                bne @l0

                asl $d019                       ; ACK raster interrupt

                lda #%00001000                  ; No scroll, multi-color off, 40-cols
                sta $d016

                lda #%11101100                  ; Screen in 0x3800, charset in $3000
                sta $d018

                lda #%00011011                  ; Bitmap mode off. Text mode.
                sta $d011

                ldx #<irq_bitmap                ; To point to raster IRQ that catches
                ldy #>irq_bitmap                ; The bitmap mode
                stx $fffe
                sty $ffff

                lda #20
                sta $d012                       ; Next raster IRQ in the rasterline $20

                pla                             ; Restore A, X, Y
                tay
                pla
                tax
                pla
                rti                             ; Restore previous PC, status

The Double IRQ technique works quite well but with certain limitations:

- Consumes additional cycles
- You can not use it during *bad lines* [#]_

Chipdisk and other sources
==========================

How to compile the Chipdisk
-------------------------

The complete Chipdisk code is here:

- https://github.com/c64scene-ar/chipdisk-nac-vol.1

To compile it you need:

- cc65_ (tested with v2.15)
- Exomizer_ (tested with v2.0.9)
- VICE_ (optional, used to generate the .d64)
- ``make``

Put everything is in the path, clone the repository, and do:

.. code:: bash

        $ git clone https://github.com/c64scene-ar/chipdisk-nac-vol.1.git
        $ cd chipdisk-nac-vol.1
        $ make

License
-------

`Apache v2 <https://www.apache.org/licenses/LICENSE-2.0>`__


Additional comments
-------------------

The code is **not** just as an example. It's real code
written for the Chipdisk that we present in the Datastorm 2017.
That means you have all the "real code" problems.

-  It is based on the Chipdisk code *Hands Up* that we presented in DeCrunch 2016
-  The requirements were changing. The code changed also. There may be
   code that is not used, or code that no longer makes sense.
-  Too many macros / unrolled-loops were used. Perhaps it would have been better to use less
   To take additional place for a possible new topic.
-  There are few comments
-  The Easter Egg has some bugs in the scroll. With time we will fix
-  There may be other bugs too.

Questions and others
===================

Do you have questions? Do you want to collaborate with PVM? We're here:

-  http://pungas.space
-  On IRC. `EFnet <http://www.efnet.org/>`__ . Channel #pvm
-  `Twitter <https://twitter.com/pungas64>`__
-  `Facebook <https://www.facebook.com/PVM1996/>`__

References
===========

.. [#] The tool used to compress sids is this: `sid_to_exo.py <https://github.com/ricardoquesada/c64-misc/blob/master/tools/sid_to_exo.py>`__
.. [#] The decompression routine is in the .zip of the Exomizer_, but you can also see it here: `exodecrunch.s <https://github.com/c64scene-ar/chipdisk-nac-vol.1/blob/master/src/exodecrunch.s>`__
.. [#] The great idea of ​​making a special charset to simplify the performance is from Alakran
.. [#] Or as Acid recommends, you could only optimize ``set_pixel() `` with tables to avoid multiplication.
.. [#] More tricks on how to optimize 6502 are here: `6502 assembly optimisations <https://wiki.nesdev.com/w/index.php/6502_assembly_optimisations>`__ and here `Synthetic instructions <https://wiki.nesdev.com/w/index.php/Synthetic_instructions>`__. And also here: `CodeBase64 <http://codebase64.org/>`__
.. [#] For more information about Bad Lines go to `Beyond the Screen: Rasters and Cycles <http://dustlayer.com/vic-ii/2013/4/25/vic-ii-for-beginners-beyond-the-screen-rasters-cycle>`__


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

