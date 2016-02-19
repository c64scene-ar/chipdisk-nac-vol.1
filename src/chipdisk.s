;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; chipdisk
; http://pungas.space
;
; To control it use:
;  - Joystick in port #2
;  - or Mouse 1351 in port #1
;       
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.include "c64.inc"                      ; c64 constants

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Imports/Exports
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.import decrunch                        ; exomizer decrunch
.export get_crunched_byte               ; needed for exomizer decruncher


DEBUG = 0                               ; rasterlines:1, music:2, all:3

.segment "CODE"
        sei

        lda #$35                        ; no basic, no kernal
        sta $01

        jsr init_sprites                ; init sprites

        lda $dd00                       ; Vic bank 1: $4000-$7FFF
        and #$fc
        ora #2
        sta $dd00

        lda #0
        sta $d020                       ; border color
        lda #0
        sta $d021                       ; background color

        lda #%00001000                  ; no scroll, hires (mono color), 40-cols
        sta $d016
        
        lda #%00111011                  ; bitmap mode, default scroll-Y position, 25-rows
        sta $d011

        lda #%10000000                  ; video RAM: $2000 (%1000xxxx) bitmap addr: $0000 (%xxxx1xxx)
        sta $d018

        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        sta $dd0d

        lda #01                         ; enable raster irq
        sta $d01a

        ldx #<irq                       ; setup IRQ vector
        ldy #>irq
        stx $fffe
        sty $ffff

        lda #$00
        sta $d012

        lda $dc0d                       ; ack possible interrupts
        lda $dd0d
        asl $d019

        cli

main_loop:
        lda #%01000000                  ; enable mouse
        sta $dc00

        lda sync_raster_irq             ; raster triggered ?
        beq :+
        jsr process_events

:
        lda sync_timer_irq
        beq :+
        dec sync_timer_irq
        jsr $1003                       ; play music

:       jmp main_loop

process_events:
        dec sync_raster_irq
        jsr read_mouse
        jsr process_mouse

        lda #%00111111                  ; enable joystick again
        sta $dc00

        jsr read_joy2
        jsr process_joy

        jsr do_raster_anims


        lda $d01e                       ; load / clear the collision bits

        rts


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_raster_anims
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_raster_anims
.if (::DEBUG & 1)
        inc $d020
.endif
        
        lda is_playing
        beq do_nothing
        jsr do_anim_cassette

do_nothing:

.if (::DEBUG & 1)
        dec $d020
.endif
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; read_joy2
; Taken from here:
; http://codebase64.org/doku.php?id=base:joystick_input_handling&s[]=joystick
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc read_joy2

        lda $dc00                       ; get input from port 2 only
        ldy #0                          ; this routine reads and decodes the
        ldx #0                          ; joystick/firebutton input data in
        lsr                             ; the accumulator. this least significant
        bcs djr0                        ; 5 bits contain the switch closure
        dey                             ; information. if a switch is closed then it
djr0:   lsr                             ; produces a zero bit. if a switch is open then
        bcs djr1                        ; it produces a one bit. The joystick dir-
        iny                             ; ections are right, left, forward, backward
djr1:   lsr                             ; bit3=right, bit2=left, bit1=backward,
        bcs djr2                        ; bit0=forward and bit4=fire button.
        dex                             ; at rts time dx and dy contain 2's compliment
djr2:   lsr                             ; direction numbers i.e. $ff=-1, $00=0, $01=1.
        bcs djr3                        ; dx=1 (move right), dx=-1 (move left),
        inx                             ; dx=0 (no x change). dy=-1 (move up screen),
djr3:   lsr                             ; dy=0 (move down screen), dy=0 (no y change).
                                        ; the forward joystick position corresponds
                                        ; to move up the screen and the backward
                                        ; position to move down screen.
                                        ;
                                        ; at rts time the carry flag contains the fire
                                        ; button state. if c=1 then button not pressed.
                                        ; if c=0 then pressed.
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; read_mouse
;       exit    x = delta x movement
;               y = delta y movement
;               C = 0 if button pressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc read_mouse
;        lda $dc02
;        sta tmp_dc02
;        lda #$ff                        ; pins set to input
;        sta $dc02

        lda $d419                       ; read delta X (pot x)
        ldy opotx
        jsr mouse_move_check            ; calculate delta
        sty opotx
        sta ret_x_value

        lda $d41a                       ; read delay Y (pot y)
        ldy opoty
        jsr mouse_move_check            ; calculate delta
        sty opoty

        eor #$ff                        ; delta is inverted... fix it
        tay
        iny

        sec                             ; C=1 (means button not pressed)

ret_x_value = * + 1
        ldx #00                         ; self modifying
;        lda tmp_dc02
;        sta $dc02
        
        lda $dc01                       ; ready joy #1 for button: bit 4
        asl
        asl
        asl
        asl                             ; C=0: button pressed
        rts

opotx: .byte $00
opoty: .byte $00
;tmp_dc02: .byte $00

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; mouse_move_check
; Taken from here:
; https://github.com/cc65/cc65/blob/master/libsrc/c64/mou/c64-1351.s
;
;       entry   y = old value of pot register
;               a = currrent value of pot register
;       exit    y = value to use for old value
;               x,a = delta value for position
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc mouse_move_check
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

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; process_mouse
;
; entry
;       X = delta x
;       Y = delta y
;       C = 0 if button pressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_mouse
        bcs no_button
        lda mouse_button_already_pressed ; don't trigger events if the button was already pressed
        beq :+
        jmp process_movement

:       lda #1
        sta mouse_button_already_pressed
        jmp process_pressed_button

no_button:
        lda #0
        sta mouse_button_already_pressed      ; button can be pressed
        jmp process_movement
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; process_joy
;
; entry
;       X = delta x
;       Y = delta y
;       C = 0 if button pressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_joy
        bcs no_button
        lda joy_button_already_pressed ; don't trigger events if the button was already pressed
        beq :+
        jmp process_movement

:       lda #1
        sta joy_button_already_pressed
        jmp process_pressed_button

no_button:
        lda #0
        sta joy_button_already_pressed      ; button can be pressed
        jmp process_movement
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; process_pressed_button
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_pressed_button
        lda $d01e                       ; quick and dirty. check for sprite collision
        lsr
        lsr                             ; skip sprite 0 and 1 (cursor)
        lsr                             ; sprite #2: play button
        bcc :+
        jmp do_play_song
:       lsr                             ; sprite #3: rewind
        bcc :+
        jmp do_prev_song
:       lsr                             ; sprite #4: fast forward
        bcc :+
        jmp do_next_song
:       lsr                             ; sprite #5: stop
        bcc :+
        jmp do_stop_song
:       rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; process_movement
;
; entry
;       X = delta x
;       Y = delta y
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_movement
        cpx #0
        beq test_y                      ; skip if no changes in Y

        clc                             ; set new sprite X position
        txa
        adc VIC_SPR0_X
        sta VIC_SPR0_X
        sta VIC_SPR1_X

        pha                             ; save in tmp

        bcs test_addition

        txa                             ; Carry Clear and substracting ?, then change MSB
        bmi change_MSB
        bpl check_x_bounds

test_addition:
        txa
        bpl change_MSB                  ; Carry Set and adding ?, then change MSB
        bmi check_x_bounds

change_MSB:
        lda $d010                       ; sprite x MSB
        eor #%00000011
        sta $d010

check_x_bounds:
        pla                             ; get A from tmp
        tax

        lda $d010
        and #%00000001
        bne x_upper

        txa
        cmp #24                         ; lower bounds: x < 24 and LSB
        bcs test_y
        lda #24
        bne set_x

x_upper:
        txa
        cmp #84                         ; lower bounds: x > 80 and MSB
        bcc test_y
        lda #84

set_x:
        sta VIC_SPR0_X
        sta VIC_SPR1_X


test_y:
        cpy #0                          ; no Y movment... quick end
        beq end

        clc
        tya
        adc VIC_SPR0_Y                  ; set new sprite Y position
        tay                             ; save A in tmp

        cmp #50                         ; don't go lower than 50
        bcs :+
        lda #50                         ; if lower, set 50 as the minimum
        bne set_y                       ; and jump

        tya                             ; restore A from tmp
:       cmp #245                        ; above 200 ?
        bcc set_y
        lda #245                        ; set 200 as the maximum

set_y:
        sta VIC_SPR0_Y
        sta VIC_SPR1_Y
end:
        rts

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_anim_cassette
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_anim_cassette
        ldx $63f8 + 6                   ; sprite pointer for sprite #0
        inx
        cpx #(144 + 16)
        bne :+
        ldx #144
:       stx $63f8 + 6                   ; turning wheel sprite pointer #0
        stx $63f8 + 7                   ; turning wheel sprite pointer #1
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_sprites
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%11111111                  ; enable sprites
        sta VIC_SPR_ENA

        lda #0
        sta $d010                       ; no 8-bit on for sprites x
        sta $d017                       ; no y double resolution
        sta $d01d                       ; no x double resolution
        sta $d01c                       ; no sprite multi-color. hi-res only


        ldx #7
        ldy #14
l1:
        lda sprites_x_pos,x             ; set x position
        sta VIC_SPR0_X,y
        lda sprites_y_pos,x             ; set y position
        sta VIC_SPR0_Y,y
        lda sprites_color,x             ; set sprite color
        sta VIC_SPR0_COLOR,x
        lda sprites_pointer,x           ; set sprite pointers
        sta $63f8,x
        dey
        dey
        dex
        bpl l1

        rts

sprites_x_pos:
        .byte 150, 150,     32, 60, 86, 115,     192, 136

sprites_y_pos:
        .byte 150, 150,     180, 195, 206, 221,     126, 98

sprites_color:
        .byte 0, 1, 1, 1, 1, 1, 12, 12

sprites_pointer:
        .byte 161, 160, 162, 163, 164, 165, 144, 144

.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq vector
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt
        bcs raster

        lda $dc0d                       ; clears CIA interrupts, in particular timer A
        inc sync_timer_irq
        bne end                         ; A will never be 0. Jump to end

raster:
        inc sync_raster_irq

end:
        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_play_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_play_song
        lda is_playing                  ; already playing ? skip
        bne end

        sei

        inc is_playing                  ; is_playing = true

        jmp init_song

        cli
end:
        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_prev_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_prev_song
        ldx current_song                ; current_song = max(0, current_song - 1)
        dex
        bpl :+
        ldx #0
:       stx current_song
        
        jmp init_song
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_next_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_next_song
        ldx current_song                ; current_song = min(7, current_song + 1)
        inx  
        cpx #TOTAL_SONGS
        bne :+
        ldx #TOTAL_SONGS-1
:       stx current_song

        jmp init_song
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_stop_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_stop_song
        sei
        lda #0
        sta is_playing                  ; is_playing = false

        lda #$7f                        ; turn off cia interrups
        sta $dc0d

        lda #$00
        sta $d418                       ; no volume
        cli
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_song
        sei

        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        lda #$00
        sta $d418                       ; no volume

        lda current_song                ; x = current_song * 2
        asl
        tax

        lda song_PAL_frequencies,x
        sta $dc04
        lda song_PAL_frequencies+1,x
        sta $dc05

        jsr init_crunch_data

        dec $01                         ; $34: RAM 100%

        jsr decrunch                    ; copy song

        inc $01                         ; $35: RAM + IO ($D000-$DF00)

        lda #0
        tax
        tay
        jsr $1000                       ; init song

        lda #$81                        ; turn on cia interrups
        sta $dc0d

        cli
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_crunch_data
; initializes the data needed by get_crunched_byte
; entry:
;       x = index of the table (current song * 2)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_crunch_data
        lda song_end_addrs,x
        sta _crunched_byte_lo
        lda song_end_addrs+1,x
        sta _crunched_byte_hi
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; get_crunched_byte
; The decruncher jsr:s to the get_crunched_byte address when it wants to
; read a crunched byte. This subroutine has to preserve x and y register
; and must not modify the state of the carry flag.
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
get_crunched_byte:
        lda _crunched_byte_lo
        bne @byte_skip_hi
        dec _crunched_byte_hi
@byte_skip_hi:
        inc $01                         ; $35 (video available)
	sta $d020
        dec $01                         ; $34 (diable video again)

        dec _crunched_byte_lo
_crunched_byte_lo = * + 1
_crunched_byte_hi = * + 2
        lda song_end_addrs              ; self-modyfing. needs to be set correctly before
	rts			        ; decrunch_file is called.
; end_of_data needs to point to the address just after the address
; of the last byte of crunched data.

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; memcpy
; entry: $fb,$fc: destination
;        $fd,$fe: source
;        x,y:     size
; Taken from here: https://github.com/cc65/cc65/blob/master/libsrc/common/memcpy.s
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc memcpy
        cpy     #$00            ; Get high byte of n
        beq     L2              ; Jump if zero

        stx     copy_size
        sty     copy_size+1
        ldy     #$00
        ldx     copy_size+1

L1:     .repeat 2               ; Unroll this a bit to make it faster...
        lda     ($fd),y         ; copy a byte
        sta     ($fb),y
        iny
        .endrepeat
        bne     L1
        inc     $fd+1
        inc     $fb+1
        dex                     ; Next 256 byte block
        bne     L1              ; Repeat if any

        ldx     copy_size       ; x = <copy_size

        ; the following section could be 10% faster if we were able to copy
        ; back to front - unfortunately we are forced to copy strict from
        ; low to high since this function is also used for
        ; memmove and blocks could be overlapping!
        ; {
L2:                             ; assert Y = 0
                                ; assert X = <copy_size
        beq     done            ; something to copy

L3:     lda     ($fd),y         ; copy a byte
        sta     ($fb),y
        iny
        dex
        bne     L3

        ; }

done:   rts

copy_size: .byte $00, $00
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sync_raster_irq:        .byte 0                 ; boolean
sync_timer_irq:         .byte 0                 ; boolean
is_playing:             .byte 0
current_song:           .byte 0                 ; selected song
joy_button_already_pressed: .byte 0             ; boolean. don't trigger the button again if it is already pressed
mouse_button_already_pressed: .byte 0           ; boolean. don't trigger the button again if it is already pressed


TOTAL_SONGS = 8
song_addrs:
        .addr song_1
        .addr song_2
        .addr song_3
        .addr song_4
        .addr song_5
        .addr song_6
        .addr song_7
        .addr song_8

song_end_addrs:
        .addr song_1_end_of_data
        .addr song_2_end_of_data
        .addr song_3_end_of_data
        .addr song_4_end_of_data
        .addr song_5_end_of_data
        .addr song_6_end_of_data
        .addr song_7_end_of_data
        .addr song_8_end_of_data

song_sizes:
        .word SONG1_SIZE
        .word SONG2_SIZE
        .word SONG3_SIZE
        .word SONG4_SIZE
        .word SONG5_SIZE
        .word SONG6_SIZE
        .word SONG7_SIZE
        .word SONG8_SIZE

song_PAL_frequencies:
        .word $62ae
        .word $4cc8 - 1
        .word $8973
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $6df5
        .word $4cc8 - 1


.segment "BITMAP"
.incbin "datasette.bitmap"

.segment "COLORMAP"
.incbin "datasette.colormap"

.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
; $1000 - $2800 free are to copy the songs


.segment "MUSIC"
song_1: .incbin "pvm5-turro.exo"
SONG1_SIZE = * - song_1
song_1_end_of_data:

song_2: .incbin "uct-balloon_country.exo"
SONG2_SIZE = * - song_2
song_2_end_of_data:

song_3: .incbin "pvm5-porro.exo"
SONG3_SIZE = * - song_3
song_3_end_of_data:

song_4: .incbin "uct-que_hago_en_manila.exo"
SONG4_SIZE = * - song_4
song_4_end_of_data:

song_5: .incbin "Pronta_Entrega.exo"
SONG5_SIZE = * - song_5
song_5_end_of_data:

song_6: .incbin "pvm-mamakilla.exo"
SONG6_SIZE = * - song_6
song_6_end_of_data:

song_7: .incbin "pvm2-indiecito.exo"
SONG7_SIZE = * - song_7
song_7_end_of_data:

song_8: .incbin "uct-carito.exo"
SONG8_SIZE = * - song_8
song_8_end_of_data:

.byte 0                 ; ignore

