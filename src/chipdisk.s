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
:
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
        jsr process_cursor

        lda #%00111111                  ; enable joystick again
        sta $dc00

        jsr read_joy2
        jsr process_cursor

        jsr do_raster_anims
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
; Routine taken from here:
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
        rts

opotx: .byte $00
opoty: .byte $00
;tmp_dc02: .byte $00

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; mouse_move_check
; taken from these places:
; https://github.com/adamdunkels/contiki-1.x/blob/master/contiki-c64/ctk/ctk-mouse-1351.S
; http://codebase64.org/doku.php?id=base:c_1351_standard_mouse_routine&s[]=mouse
;
;       entry   y = old value of pot register
;               a = currrent value of pot register
;       exit    y = value to use for old value
;               x,a = delta value for position
;
; Most of the mouse code is taken from the CC65 libraries written by
; Ullrich von Bassewitz
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
; process_cursor
;
; entry
;       X = delta x
;       Y = delta y
;       C = 0 if button pressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_cursor
        bcs no_button
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
        bcc no_button
        jmp do_stop_song

no_button:
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

        jsr $1000                       ; setups up the timer IRQ

        lda $dc0e
        ora #$11
        sta $dc0e                       ; start timer interrupt A
        lda #$81
        sta $dc0d                       ; enable timer A interrupts

        cli
end:
        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_prev_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_prev_song
        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_next_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_next_song
        rts
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
; global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sync_raster_irq:        .byte 0                 ; boolean
sync_timer_irq:         .byte 0                 ; boolean
is_playing:             .byte 0


.segment "BITMAP"
.incbin "datasette.bitmap"

.segment "COLORMAP"
.incbin "datasette.colormap"

.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
.incbin "pvm5-turro.sid", $7e
