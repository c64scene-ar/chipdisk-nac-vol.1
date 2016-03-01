;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; chipdisk 
; http://pungas.space
;
; code: riq/PVM, munshkr/PVM
; Some code snippets were taken from different places.
; Credit added in those snippets.
;       
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Imports/Exports
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.import __SPRITES_LOAD__
.import decrunch                        ; exomizer decrunch
.export get_crunched_byte               ; needed for exomizer decruncher

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Constants
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
DEBUG = 3                               ; rasterlines:1, music:2, all:3
SPRITE0_POINTER = (__SPRITES_LOAD__ .MOD $4000) / 64

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macro PLOT_NEXT_Y
        iny
        cpy #8
        bne :+
        ldy #64                         ; add 320
        inc $f9
        inc $fb
:
.endmacro

.macro FETCH_NEXT_CHAR
        ldx #0                          ; could use a table to multiply by 8
        stx tmp_mul8_hi                 ; but plotting the name doesn't require speed (at least not now)
        stx tmp_mul8_lo                 ; so it is better to trade speed for space in this case

        asl           
        rol tmp_mul8_hi
        asl
        rol tmp_mul8_hi
        asl
        rol tmp_mul8_hi
        sta tmp_mul8_lo

        clc
        lda #<charset
        adc tmp_mul8_lo
        sta $f6                         ; $f6,$f7 points to the char to plot

        clc
        lda #>charset
        adc tmp_mul8_hi
        sta $f7
.endmacro

; entry
;       X = cursor pos x
;       Y = cursor pos y
.macro CHECK_PRESSED_BUTTON offset_x, offset_y, routine
.scope
        AreaSize = 24

        cpx #offset_x
        bcc :+
        cpx #(offset_x + AreaSize)
        bcs :+
        cpy #offset_y
        bcc :+
        cpy #(offset_y + AreaSize)
        bcs :+
        jmp routine
:
.endscope
.endmacro


.segment "CODE"
        sei

        lda #$35                        ; no basic, no kernal
        sta $01

        jsr fix_bitmap                  ; FIXME: fix THE bitmap, instead of adding code
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

        ldx #<irq_a                     ; setup IRQ vector
        ldy #>irq_a
        stx $fffe
        sty $ffff

        lda #$00
        sta $d012

        lda $dc0d                       ; ack possible interrupts
        lda $dd0d
        asl $d019

        jsr do_play_song                ; start song 0 at the beginning

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
.if (::DEBUG & 2)
        inc $d020
.endif
        jsr $1003                       ; play music
.if (::DEBUG & 2)
        dec $d020
.endif

        inc song_tick                   ; update song_tick
        bne :+
        inc song_tick+1

:       jmp main_loop

process_events:
        dec sync_raster_irq

        jsr read_mouse
        jsr process_mouse

        lda #%00111111                  ; enable joystick again
        sta $dc00

        jsr read_joy2
        jsr process_joy
        jsr skip_song_if_ended

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
        ldx VIC_SPR0_X        ; get cursor position
        ldy VIC_SPR0_Y

        CHECK_PRESSED_BUTTON 36     , 180     , do_play_song
        CHECK_PRESSED_BUTTON 36+28  , 180+14  , do_prev_song
        CHECK_PRESSED_BUTTON 36+28*2, 180+14*2, do_next_song
        CHECK_PRESSED_BUTTON 36+28*3, 180+14*3, do_stop_song

        rts
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
        txa
        and #%00001111                  ; and 16
        ora #%10010000                  ; ora 144
        sta $63f8 + 6                   ; turning wheel sprite pointer #0
        sta $63f8 + 7                   ; turning wheel sprite pointer #1
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; fix_bitmap
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc fix_bitmap

        ldx #7

next_cell:
        lda cells_lo,x
        sta $fe
        lda cells_hi,x
        sta $ff

        ldy #7
loop:
        lda ($fe),y
        eor #$ff
        sta ($fe),y
        dey
        bpl loop

        ;
        lda colors_lo,x
        sta $fe
        lda colors_hi,x
        sta $ff

        ldy #0
        lda ($fe),y
        cmp #$80                        ; swap nibble
        rol
        cmp #$80
        rol
        cmp #$80
        rol
        cmp #$80
        rol
        sta ($fe),y

        dex
        bpl next_cell


        rts
        ; row: 7, 9, 3, 4, 5, 6, 7, 8
        ; cols: 20,24,17,19,21,23,25,27
cells_lo:
        .byte <($4000 + 320 * 7 + 8 * 20)
        .byte <($4000 + 320 * 9 + 8 * 24)
        .byte <($4000 + 320 * 8 + 8 * 18)
        .byte <($4000 + 320 * 9 + 8 * 20)
        .byte <($4000 + 320 * 7 + 8 * 16)
        .byte <($4000 + 320 * 10 + 8 * 22)
        .byte <($4000 + 320 * 13 + 8 * 26)
        .byte <($4000 + 320 * 9 + 8 * 29)

cells_hi:
        .byte >($4000 + 320 * 7 + 8 * 20)
        .byte >($4000 + 320 * 9 + 8 * 24)
        .byte >($4000 + 320 * 8 + 8 * 18)
        .byte >($4000 + 320 * 9 + 8 * 20)
        .byte >($4000 + 320 * 7 + 8 * 16)
        .byte >($4000 + 320 * 10 + 8 * 22)
        .byte >($4000 + 320 * 13 + 8 * 26)
        .byte >($4000 + 320 * 9 + 8 * 29)

colors_lo:
        .byte <($6000 + 40 * 7 + 20)
        .byte <($6000 + 40 * 9 + 24)
        .byte <($6000 + 40 * 8 + 18)
        .byte <($6000 + 40 * 9 + 20)
        .byte <($6000 + 40 * 7 + 16)
        .byte <($6000 + 40 * 10 + 22)
        .byte <($6000 + 40 * 13 + 26)
        .byte <($6000 + 40 * 9 + 29)

colors_hi:
        .byte >($6000 + 40 * 7 + 20)
        .byte >($6000 + 40 * 9 + 24)
        .byte >($6000 + 40 * 8 + 18)
        .byte >($6000 + 40 * 9 + 20)
        .byte >($6000 + 40 * 7 + 16)
        .byte >($6000 + 40 * 10 + 22)
        .byte >($6000 + 40 * 13 + 26)
        .byte >($6000 + 40 * 9 + 29)
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
        .byte 150, 150,     0, 0, 0, 0,     192, 136

sprites_y_pos:
        .byte 150, 150,     0, 0, 0, 0,     126, 98

sprites_color:
        .byte 0, 1,   1, 1, 1, 1,   12, 12

sprites_pointer:
        .byte 161, 160,   162, 163, 164, 165,   144, 144
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq vectors
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_a
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

        jsr button_play_save
        jsr button_play_plot

        lda is_already_loaded
        beq :+

        lda #1
        sta is_playing                  ; is_playing = true
        lda #$81
        sta $dc0d                       ; turn on cia interrups
        bne end

:       jmp init_song
end:
        rts
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_prev_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_prev_song
        jsr button_play_restore
        jsr button_rew_save
        jsr button_rew_plot

        ldx current_song                ; current_song = max(0, current_song - 1)
        dex
        bpl :+
        ldx #0
:       stx current_song

        lda #0
        sta is_already_loaded           ; is_already_loaded = false

        jsr init_song

        ; TODO do this in the next frame
        jsr button_rew_restore
        jsr button_play_save
        jsr button_play_plot

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_next_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_next_song
        jsr button_play_restore
        jsr button_ff_save
        jsr button_ff_plot

        ldx current_song                ; current_song = min(7, current_song + 1)
        inx  
        cpx #TOTAL_SONGS
        bne :+
        ldx #TOTAL_SONGS-1
:       stx current_song

        lda #0
        sta is_already_loaded           ; is_already_loaded = false

        jsr init_song

        ; TODO do this in the next frame
        jsr button_ff_restore
        jsr button_play_save
        jsr button_play_plot

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

        jsr button_play_restore
        ;jsr button_stop_save
        ;jsr button_stop_plot

        ; TODO do this in the next frame
        ;jsr button_stop_restore
        ;jsr button_play_save

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_song
        sei

        lda #1
        sta is_playing                  ; is_playing = true


        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        lda #$00
        sta $d418                       ; no volume

        jsr print_names

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

        lda #1
        sta is_already_loaded           ; is_already_loaded = true

        lda #0
        sta song_tick                   ; reset song tick
        sta song_tick+1

        lda #$81                        ; turn on cia interrups
        sta $dc0d

        cli
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; skip_song_if_ended
;   if (song_tick >= song_durations[current_song]) do_next_song();
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc skip_song_if_ended
        lda current_song                ; x = current_song * 2
        asl                             ; (song_durations is a word array)
        tax

        lda song_durations,x            ; pointer to song durations
        sta $fc
        lda song_durations+1,x
        sta $fd

        ; unsigned comparison per byte
        lda song_tick+1   ; compare high bytes
        cmp $fd
        bcc end_skip_song ; if MSB(song_tick) < MSB(song_duration) then
                          ;     song_tick < song_duration
        bne skip_song     ; if MSB(song_tick) <> MSB(song_duration) then
                          ;     song_tick > song_duration (so song_tick >= song_duration)

        lda song_tick     ; compare low bytes
        cmp $fc
        bcc end_skip_song ; if LSB(song_tick) < LSB(song_duration) then
                          ;     song_tick < song_duration
skip_song:
        jsr do_next_song
end_skip_song:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; print_names
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc print_names
        lda current_song                ; x = current_song * 2
        asl
        tax
        pha                             ; save it

        lda song_names,x                ; pointer to name
        sta $fc
        lda song_names+1,x
        sta $fd

OFFSET_Y_UPPER = 3
OFFSET_X_UPPER = 14
        ldx #<(bitmap + OFFSET_Y_UPPER * 8 * 40 + OFFSET_X_UPPER * 8)
        ldy #>(bitmap + OFFSET_Y_UPPER * 8 * 40 + OFFSET_X_UPPER * 8)
        jsr plot_name

        ;
        pla                             ; restore x
        tax
        lda song_authors,x              ; pointer to name
        sta $fc
        lda song_authors+1,x
        sta $fd

OFFSET_Y_BOTTOM = 6
OFFSET_X_BOTTOM = 11
        ldx #<(bitmap + OFFSET_Y_BOTTOM * 8 * 40 + OFFSET_X_BOTTOM * 8)
        ldy #>(bitmap + OFFSET_Y_BOTTOM * 8 * 40 + OFFSET_X_BOTTOM * 8)
        jsr plot_name

        ;
        ldx current_song 
        inx
        txa
        ora #$30
        sta counter_label + 2           ; store song number in PETSCII format

        lda #<counter_label
        sta $fc
        lda #>counter_label
        sta $fd

OFFSET_Y_COUNTER = 15
OFFSET_X_COUNTER = 32
        ldx #<(bitmap + OFFSET_Y_COUNTER * 8 * 40 + OFFSET_X_COUNTER * 8)
        ldy #>(bitmap + OFFSET_Y_COUNTER * 8 * 40 + OFFSET_X_COUNTER * 8)
        jmp plot_counter
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

        php
        inc $63f8 + 6                   ; sprite pointer for sprite #0
        lda $63f8 + 6                   ; sprite pointer for sprite #0
        and #%00001111                  ; and 16
        ora #%10010000                  ; ora 144
:       sta $63f8 + 6                   ; turning wheel sprite pointer #0
        sta $63f8 + 7                   ; turning wheel sprite pointer #1
        plp

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


.segment "MORECODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_counter
; entry:
;       x = LSB bitmap address
;       y = MSB bitmap address
;       $fc,$fd: pointer to string to print. Only 3 chars
;       
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_counter

        stx $f8                         ; $f8,$f9: bitmap address
        sty $f9

        txa                             ; $fa,$fb points to bitmap address + 8
        clc                             ; plot_char_odd / _event uses ($f8) and
        adc #8                          ; ($fa) to plot the pixels
        sta $fa 
        tya
        adc #0                          
        sty $fb

        ldy #0
        sty tmp_counter

        lda ($fc),y

        FETCH_NEXT_CHAR                 ; updates $f6/$f7. modifies A,X
        jsr plot_char_odd

        inc tmp_counter
        ldy tmp_counter

        lda ($fc),y
        FETCH_NEXT_CHAR                 ; updates $f6/f7. modifies A,X
        jsr set_next_bitmap_even
        jsr plot_char_even

        inc tmp_counter
        ldy tmp_counter

        lda ($fc),y
        FETCH_NEXT_CHAR                 ; updates $f6/f7, modifies A,X
        jsr set_next_bitmap_odd
        jsr plot_char_odd
        
        rts
tmp_counter: .byte 0
tmp_mul8_hi: .byte 0
tmp_mul8_lo: .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_name
; entry:
;       x = LSB bitmap address
;       y = MSB bitmap address
;       $fc,$fd: pointer to string to print. Only 15 chars
;       
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_name

        stx $f8                         ; $f8,$f9: bitmap address
        sty $f9

        txa                             ; $fa,$fb points to bitmap address + 8
        clc                             ; plot_char_odd / _event uses ($f8) and
        adc #8                          ; ($fa) to plot the pixels
        sta $fa 
        tya
        adc #0                          
        sty $fb

        ldy #0
        sty tmp_counter

        ldy tmp_counter
        lda ($fc),y

        FETCH_NEXT_CHAR                 ; updates $f6/$f7. modifies A,X
        jsr plot_char_even

        inc tmp_counter
        ldy tmp_counter

loop:
        lda ($fc),y
        FETCH_NEXT_CHAR                 ; updates $f6/f7. modifies A,X
        jsr set_next_bitmap_odd
        jsr plot_char_odd

        inc tmp_counter
        ldy tmp_counter

        lda ($fc),y
        FETCH_NEXT_CHAR                 ; updates $f6/f7, modifies A,X
        jsr set_next_bitmap_even
        jsr plot_char_even

        inc tmp_counter
        ldy tmp_counter
        cpy #15
        bne loop
        
        rts
tmp_counter: .byte 0
tmp_mul8_hi: .byte 0
tmp_mul8_lo: .byte 0
.endproc

.proc set_next_bitmap_odd
        ldx $fa                         ; $f8/$f9 = previous $fa/$fb
        ldy $fb
        stx $f8
        sty $f9

        clc                             ; $fa/$fb += 8
        txa
        adc #08
        sta $fa
        tya
        adc #00
        sta $fb

        rts
.endproc

.proc set_next_bitmap_even
        clc
        lda $f8                         ; $f8,$f9 += 320 + 8
        adc #64 + 8                     ; but $f9 was already inc'ed (+ 256)
        sta $f8                         ; we only need to add 64 + 8
        lda $f9
        adc #0
        sta $f9

        clc
        lda $fa                         ; $fa,$fb += 320 + 8
        adc #64 + 8                     ; but $fb was already inc'ed (+ 256)
        sta $fa                         ; we only need to add 64 + 8
        lda $fb
        adc #0
        sta $fb

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_even
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_even
        ldy #0
        lda ($f6),y                     ; plot char + y (top row)
        jsr plot_row_0

        ldy #1
        lda ($f6),y                     ; plot char + y
        jsr plot_row_1

        ldy #2
        lda ($f6),y                     ; 
        jsr plot_row_2

        ldy #3
        lda ($f6),y                     ; 
        jsr plot_row_3

        ldy #4
        lda ($f6),y                     ; 
        jsr plot_row_4

        ldy #5
        lda ($f6),y                   
        jsr plot_row_5
        dec $f9
        dec $fb

        ldy #6
        lda ($f6),y                     ; 
        jsr plot_row_6
        dec $f9
        dec $fb

        ldy #7
        lda ($f6),y                     ; 
        jsr plot_row_7
        dec $f9
        dec $fb

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_odd
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_odd
        ldy #0
        lda ($f6),y                     ; plot char + y (top row)
        ldy #4
        jsr plot_row_0

        ldy #1
        lda ($f6),y                     ; plot char + y
        ldy #5
        jsr plot_row_1
        dec $f9
        dec $fb

        ldy #2
        lda ($f6),y                     ; 
        ldy #6
        jsr plot_row_2
        dec $f9
        dec $fb

        ldy #3
        lda ($f6),y                     ; 
        ldy #7
        jsr plot_row_3

                                        ; don't restore $f9,$fb
                                        ; start rendering at 320
        ldy #4
        lda ($f6),y                     ; 
        ldy #64
        jsr plot_row_4

        ldy #5
        lda ($f6),y                     ; 
        ldy #65
        jsr plot_row_5

        ldy #6
        lda ($f6),y                     ; 
        ldy #66
        jsr plot_row_6

        ldy #7
        lda ($f6),y                     ; 
        ldy #67
        jsr plot_row_7

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_0
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_0
        asl                             ; rotate one to left (or 7 to right)
        adc #0
        tax                             ; save for next value

                                        ; start new bit (one bit)
        and #%00000001                  ; x=0, y=0
        sta ora_0
        lda ($f8),y
        and #%11111110
ora_0 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit 
        txa                             ; x=1, y=0 
        and #%10000000
        sta ora_1
        lda ($fa),y
        and #%01111111
ora_1 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; x=2,3, y=0
        and #%01100000
        sta ora_23
        lda ($fa),y                     ; assert: y==1
        and #%10011111
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; x=4,5, y=0
        and #%00011000
        sta ora_45
        lda ($fa),y                     ; assert: y==2
        and #%11100111
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; x=6,7, y=0
        and #%00000110
        sta ora_67
        lda ($fa),y                     ; assert: y==3
        and #%11111001
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_1
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_1
        asl                             ; rotate two to left (or 6 to right)
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bit)
        and #%00000011                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%11111100
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%11000000                  ; x=2,3
        sta ora_23
        lda ($fa),y
        and #%00111111
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00110000                  ; x=4,5
        sta ora_45
        lda ($fa),y
        and #%11001111
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00001100                  ; x=6,7
        sta ora_67
        lda ($fa),y
        and #%11110011
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_2
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_2
        asl                             ; rotate 3 to left (or 5 to right)
        adc #0
        asl
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bits)
        and #%00000110                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%11111001
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit  (one bit)
        PLOT_NEXT_Y
        txa                             ; x=2
        and #%00000001
        sta ora_2
        lda ($f8),y
        and #%11111110
ora_2 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit (one bit)
        txa                             ; x=3
        and #%10000000
        sta ora_3
        lda ($fa),y                     ; assert: y==3
        and #%01111111
ora_3 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        ;                               ; start new bit (two bits)
        PLOT_NEXT_Y
        txa                             ; x=4,5
        and #%01100000
        sta ora_45
        lda ($fa),y                     ; assert: y==4
        and #%10011111
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        ;                               ; start new bit (two bits)
        PLOT_NEXT_Y
        txa                             ; x=4,5
        and #%00011000
        sta ora_67
        lda ($fa),y                     ; assert: y==5
        and #%11100111
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y


        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_3
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_3
        asl                             ; rotate 4 to left (or 4 to right)
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bit)
        and #%00001100                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%11110011
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00000011                  ; x=2,3
        sta ora_23
        lda ($f8),y
        and #%11111100
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%11000000                  ; x=4,5
        sta ora_45
        lda ($fa),y
        and #%00111111
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00110000                  ; x=6,7
        sta ora_67
        lda ($fa),y
        and #%11001111
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_4
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_4
        asl                             ; rotate 5 to left (or 3 to right)
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bits)
        and #%00011000                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%11100111
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y
        txa                             ; start new bit (two bits)
        and #%00000110                  ; x=2,3
        sta ora_23
        lda ($f8),y
        and #%11111001
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit  (one bit)
        PLOT_NEXT_Y
        txa                             ; x=4
        and #%00000001
        sta ora_4
        lda ($f8),y
        and #%11111110
ora_4 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit (one bit)
        txa                             ; x=5
        and #%10000000
        sta ora_5
        lda ($fa),y                     ; assert: y==6
        and #%01111111
ora_5 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        ;                               ; start new bit (two bits)
        PLOT_NEXT_Y
        txa                             ; x=6,7
        and #%01100000
        sta ora_67
        lda ($fa),y                     ; assert: y==7
        and #%10011111
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y


        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_5
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_5
        asl                             ; rotate 6 to left (or 2 to right)
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bit)
        and #%00110000                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%11001111
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00001100                  ; x=2,3
        sta ora_23
        lda ($f8),y
        and #%11110011
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%00000011                  ; x=4,5
        sta ora_45
        lda ($f8),y
        and #%11111100
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y
        txa                             ; start new bit (two bit)
        and #%11000000                  ; x=6,7
        sta ora_67
        lda ($fa),y
        and #%00111111
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_6
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_6
        asl                             ; rotate 7 to left (or 1 to right)
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        asl
        adc #0
        tax                             ; save for next value

                                        ; start new bit (two bits)
        and #%01100000                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%10011111
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

        txa
        and #%00011000                  ; x=2,3
        sta ora_23
        lda ($f8),y
        and #%11100111
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

        txa                             ; x=4,5
        and #%00000110
        sta ora_45
        lda ($f8),y                     ; assert: y==0
        and #%11111001
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

        txa                             ; x=4
        and #%00000001
        sta ora_6
        lda ($f8),y
        and #%11111110
ora_6 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;                               ; Start new bit (one bit)
        txa                             ; x=5
        and #%10000000
        sta ora_7
        lda ($fa),y
        and #%01111111
ora_7 = *+1
        ora #0                          ; self modifying
        sta ($fa),y

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_row_7
; entry:
;       A = byte to plot
;       Y = bitmap offset
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_row_7
        tax
                                        ; start new bit (two bit)
        and #%11000000                  ; x=0,1
        sta ora_01
        lda ($f8),y
        and #%00111111
ora_01 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

:       txa                             ; start new bit (two bit)
        and #%00110000                  ; x=2,3
        sta ora_23
        lda ($f8),y
        and #%11001111
ora_23 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

        txa                             ; start new bit (two bit)
        and #%00001100                  ; x=4,5
        sta ora_45
        lda ($f8),y
        and #%11110011
ora_45 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        ;
        PLOT_NEXT_Y

        txa                             ; start new bit (two bit)
        and #%00000011                  ; x=6,7
        sta ora_67
        lda ($f8),y
        and #%11111100
ora_67 = *+1
        ora #0                          ; self modifying
        sta ($f8),y

        rts
.endproc

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

;; play
.proc button_play_plot
        BUTTON_IMAGE_COPY  img_button_play, 0, 14
.endproc
.proc button_play_save
        BUTTON_IMAGE_COPY  tmp_img_button, 0, 14, 1
.endproc
.proc button_play_restore
        BUTTON_IMAGE_COPY  tmp_img_button,  0, 14
.endproc

;; rew
.proc button_rew_plot
        BUTTON_IMAGE_COPY  img_button_rew, 3, 16
.endproc
.proc button_rew_save
        BUTTON_IMAGE_COPY  tmp_img_button, 3, 16, 1
.endproc
.proc button_rew_restore
        BUTTON_IMAGE_COPY  tmp_img_button, 3, 16
.endproc

;; ff
.proc button_ff_plot
        BUTTON_IMAGE_COPY  img_button_ff,  7, 18
.endproc
.proc button_ff_save
        BUTTON_IMAGE_COPY  tmp_img_button,  7, 18, 1
.endproc
.proc button_ff_restore
        BUTTON_IMAGE_COPY  tmp_img_button, 7, 18
.endproc

;; stop
.proc button_stop_plot
        BUTTON_IMAGE_COPY  img_button_stop, 10, 18
.endproc
.proc button_stop_save
        BUTTON_IMAGE_COPY  tmp_img_button, 10, 18, 1
.endproc
.proc button_stop_restore
        BUTTON_IMAGE_COPY  tmp_img_button,  10, 18
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sync_raster_irq:        .byte 0                 ; boolean
sync_timer_irq:         .byte 0                 ; boolean
is_playing:             .byte 0
is_already_loaded:      .byte 0                 ; boolean. whether current song has already been loaded (init)
current_song:           .byte 0                 ; selected song
joy_button_already_pressed: .byte 0             ; boolean. don't trigger the button again if it is already pressed
mouse_button_already_pressed: .byte 0           ; boolean. don't trigger the button again if it is already pressed
song_tick: .word 0                              ; word. incremented on each frame, when playing


TOTAL_SONGS = 8

song_names:
        .addr song_1_name
        .addr song_2_name
        .addr song_3_name
        .addr song_4_name
        .addr song_5_name
        .addr song_6_name
        .addr song_7_name
        .addr song_8_name

song_authors:
        .addr song_1_author
        .addr song_2_author
        .addr song_3_author
        .addr song_4_author
        .addr song_5_author
        .addr song_6_author
        .addr song_7_author
        .addr song_8_author

song_end_addrs:
        .addr song_1_end_of_data
        .addr song_2_end_of_data
        .addr song_3_end_of_data
        .addr song_4_end_of_data
        .addr song_5_end_of_data
        .addr song_6_end_of_data
        .addr song_7_end_of_data
        .addr song_8_end_of_data


song_PAL_frequencies:
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $4cc8 - 1
        .word $62ae
        .word $4cc8 - 1
        .word $4cc8 - 1


song_durations:                                ; measured in "cycles ticks"
        .word 102 * 50                         ; 100s * 50hz
        .word 137 * 50
        .word 199 * 50
        .word 120 * 50
        .word 10 * 50
        .word 10 * 50
        .word 10 * 50
        .word 10 * 50

                 ;ABCDEFGHIJKLMNO
song_1_name:
        scrcode "Balloon Country"
song_2_name:
        ;scrcode "Ryuuju No Dengon"
        .byte 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142
song_3_name:
        scrcode " Yasashisa  Ni "
song_4_name:
        scrcode "   Leetit 3    "
song_5_name:
        scrcode "  Mama  Killa  "
song_6_name:
        scrcode "     Turro     "
song_7_name:
        scrcode "    Carito     "
song_8_name:
        scrcode "Que Hago En Man"      ; ila

song_1_author:
        scrcode "    Uctumi     "
song_2_author:
        scrcode "    Uctumi     "
song_3_author:
        scrcode "    Uctumi     "
song_4_author:
        scrcode "     CoMu      "
song_5_author:
        scrcode "     CoMu      "
song_6_author:
        scrcode "     Naku      "
song_7_author:
        scrcode "    Uctumi     "
song_8_author:
        scrcode "    Uctumi     "

counter_label:
        .byte $30, $30, $30     ; 000


.segment "IMAGES"
img_button_play:
.incbin "button_play.raw"
img_button_rew:
.incbin "button_rew.raw"
img_button_ff:
.incbin "button_ff.raw"
img_button_stop:
.incbin "button_stop.raw"
tmp_img_button:
.res 441, 0       ; reserved for temporary storing
                  ; button bitmap before being pressed

.segment "BITMAP"
bitmap:
.incbin "datasette.bitmap"

.segment "COLORMAP"
.incbin "datasette.colormap"

.segment "SPRITES"
.incbin "sprites.bin"

.segment "SIDMUSIC"
; $1000 - $2800 free are to copy the songs


.segment "MUSIC"
song_1: .incbin "uct-balloon_country.exo"
song_1_end_of_data:

song_2: .incbin "uc-ryuuju_no_dengon.exo"
song_2_end_of_data:

song_3: .incbin "uc-yasashisa_ni.exo"
song_3_end_of_data:

song_4: .incbin "leetit38580.exo"
song_4_end_of_data:

song_5: .incbin "pvm-mamakilla.exo"
song_5_end_of_data:

song_6: .incbin "pvm5-turro.exo"
song_6_end_of_data:

song_7: .incbin "uct-carito.exo"
song_7_end_of_data:

song_8: .incbin "uct-que_hago_en_manila.exo"
song_8_end_of_data:

.byte 0                 ; ignore

.segment "CHARSET"
charset:
.incbin "names-charset.bin"

