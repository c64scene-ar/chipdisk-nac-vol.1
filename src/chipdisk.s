;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; chipdisk
; http://pungas.space
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

DEBUG = 1                               ; rasterlines:1, music:2, all:3

BITMAP_ADDR = $2000 + 8 * 40 * 12


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
        lda sync_raster                 ; raster triggered ?
        beq :+
        jsr do_raster_anims

        jsr read_joy2
        lda $dc00
        eor #$7f
        beq :+
        jsr process_joy2
:
        jmp main_loop

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_raster_anims
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_raster_anims
.if (::DEBUG & 1)
        inc $d020
.endif
        dec sync_raster

        jsr do_anim_cassette

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
; process_joy2
; X = -1, 0, or 1
; Y = -1, 0, or 1
; C = button pressed if Carry Clear
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_joy2
        cpx #0
        beq test_y                      ; skip if no changes in Y

        clc                             ; set new sprite X position
        txa
        adc $d000 + 6 * 2
        sta $d000 + 6 * 2
        sta $d000 + 7 * 2

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
        eor #%11000000
        sta $d010

check_x_bounds:
        pla                             ; get A from tmp
        tax

        lda $d010
        and #%10000000
        bne x_upper

        txa
        cmp #24                         ; lower bounds: x < 24 and LSB
        bcs test_y
        lda #24
        sta $d000 + 6 * 2
        sta $d000 + 7 * 2
        bne test_y

x_upper:
        txa
        cmp #84                         ; lower bounds: x > 80 and MSB
        bcc test_y
        lda #84
        sta $d000 + 6 * 2
        sta $d000 + 7 * 2


test_y:
        cpy #0                          ; no Y movment... quick end
        beq end

        clc
        tya
        adc $d000 + 6 * 2 + 1           ; set new sprite Y position
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
        sta $d000 + 6 * 2 + 1
        sta $d000 + 7 * 2 + 1
end:
        rts

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_anim_cassette
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_anim_cassette
        ldx $63f8                       ; sprite pointer for sprite #0
        inx
        cpx #(144 + 16)
        bne :+
        ldx #144
:       stx $63f8                       ; turning wheel sprite pointer #0
        stx $63f9                       ; turning wheel sprite pointer #1
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_sprites
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%11000011                  ; enable sprites
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
        .byte 192, 136, 0, 0, 0, 0, 150, 150

sprites_y_pos:
        .byte 126, 98, 0, 0, 0, 0, 150, 150

sprites_color:
        .byte 12, 12, 0, 0, 0, 0, 0, 1

sprites_pointer:
        .byte 144, 144, 0, 0, 0, 0, 207, 206

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

        inc sync_raster

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sync_raster: .byte 0


.segment "BITMAP"
.incbin "datasette.bitmap"

.segment "COLORMAP"
.incbin "datasette.colormap"

.segment "SPRITES"
.incbin "sprites.bin"
