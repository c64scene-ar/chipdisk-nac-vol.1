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
DEBUG = 0                               ; rasterlines:1, music:2, all:3
SPRITE0_POINTER = <((__SPRITES_LOAD__ .MOD $4000) / 64)

BORDER_LEFT = 32
BORDER_TOP = 50

WHEEL_FRAMES = 5
WHEEL_BASE_FRAME = SPRITE0_POINTER
WHEEL_FF_DELAY   = 90
WHEEL_PLAY_DELAY = 150

LED_ON_COLOR  = 2
LED_OFF_COLOR = 12

WHITE_NOISE_INIT = $7400
WHITE_NOISE_PLAY = $7403

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Macros
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macro PLOT_ROWS number_of_rows, char_y_offset, cell_y_offset, cell_x_offset
        .repeat number_of_rows, YY
                ldy #char_y_offset + YY
                lda ($f6),y
                ldy #cell_y_offset + YY
                jsr .IDENT(.CONCAT("plot_row_", .STRING(cell_x_offset + YY)))
        .endrepeat
.endmacro


; entry
;       A = byte to multiply
.macro FETCH_NEXT_CHAR
        ldx #0                          ; could use a table to multiply by 8
        stx tmp_mul8_hi                 ; but plotting the name doesn't require speed (at least not now)
                                        ; so it is better to trade speed for space in this case
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
;       A = byte to plot
;       Y = bitmap offset
;       MUST NOT modify X
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

        jsr init_sprites                ; init sprites

        lda $dd00                       ; Vic bank 1: $4000-$7FFF
        and #$fc
        ora #2
        sta $dd00

        lda #0
        sta $d020                       ; background color
        sta $d021                       ; and border color
        sta $d012                       ; raster MSB is off

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

        lda #<$4cc7                     ; init timer
        sta $dc04                       ; it plays at 50.125hz
        lda #>$4cc7                     ; sync with PAL
        sta $dc05

        lda $dc0d                       ; ack possible interrupts
        lda $dd0d
        asl $d019

        jsr WHITE_NOISE_INIT            ; init white noise sid

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
        jsr play_music

:
        jmp main_loop


play_music:
        dec sync_timer_irq

        lda white_noise_counter         ; playing white noise ?
        bne @do_white_noise             ; counter > 0 means playig white noise

@play_real_song:
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
:
        jmp skip_song_if_ended          ; end of song ?
                                        ; and return


@do_white_noise:
        dec white_noise_counter         ; play white noise for only a few ticks
        beq @stop_white_noise           ; already reached the limit ?
.if (::DEBUG & 2)
        inc $d020
.endif
        jsr WHITE_NOISE_PLAY            ; if not, play white noise
.if (::DEBUG & 2)
        dec $d020
.endif
        rts
@stop_white_noise:                       ; limit reached ? transition to real song
        jsr init_real_song              ; init the real song
        jmp @play_real_song             ; and in the same frame, play it


process_events:
        dec sync_raster_irq

        jsr read_mouse
        jsr process_mouse

        jsr read_keyboard
        jsr process_keyboard

        jsr check_easteregg

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
        jsr do_anim_button

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
; read_keyboard
; Taken from here:
;   https://www.c64-wiki.com/index.php/Keyboard
; Reference:
;   http://sta.c64.org/cbm64kbdlay.html
;
; Only checks if left or right cursor keys are pressed.
;
; A = 0 means no key has been pressed
; A = 1 means Right key has been pressed
; A = 2 means Left key has been pressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc read_keyboard
        ;; NOTE: bits are inverted when using CIA (0 = on, 1 = off)

        NoKey    = 0
        LeftKey  = 1
        RightKey = 2

        ;; First check for shift modifier keys
        ;; Left shift
        lda #%11111101    ; row 2
        sta CIA1_PRA
        lda CIA1_PRB
        and #%10000000    ; col 7
        beq :+
        ;; Right shift
        lda #%10111111    ; row 6
        sta CIA1_PRA
        lda CIA1_PRB
        and #%00010000    ; col 4
        beq :+
        lda #$ff          ; no shift then
:       sta shift_on

        ;; Then, check for "cursor left/right"
        lda #%11111110    ; row 0
        sta CIA1_PRA
        lda CIA1_PRB
        and #%00000100    ; col 2
        cmp keydown
        bne newkey
        lda #NoKey        ; no key change
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

        lda $d41a                       ; read delta Y (pot y)
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
; process_keyboard
;
; entry
;       A = 0 => no key
;       A = 1 => right key
;       A = 2 => left key
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc process_keyboard
        beq end
        cmp #1
        beq left
right:
        ;; if (current_button >= 0)
        ;;     current_button = (current_button + 1) % 4;
        ;; else
        ;;     current_button = 0;
        ldx current_button
        inx
        txa
        cmp #4
        bcc :+
        lda #0
:       sta current_button
        jmp move_cursor
left:
        ldx current_button
        dex
        txa
        cmp #$ff
        bcc :+
        lda #(4-1)
:       sta current_button
move_cursor:
        asl
        tax
        lda buttons_pos, x
        sta VIC_SPR0_X
        sta VIC_SPR1_X
        lda buttons_pos+1, x
        sta VIC_SPR0_Y
        sta VIC_SPR1_Y
end:    rts
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
        CHECK_PRESSED_BUTTON 36+28*2, 180+14*2, do_ff_song
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
.proc do_anim_button
        ;; decrement animation counter
        ldx button_delay_counter
        beq :+
        dex
        stx button_delay_counter
        bne :+

        ;; the only animation so far is for Stop button
        jsr button_stop_restore
        jsr button_play_save
:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_anim_cassette
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_anim_cassette
        dec delay
        bne end

        lda #3
        sta delay

        dec $63f8 + 6                   ; sprite pointer for sprite #0
        lda $63f8 + 6                   ; sprite pointer for sprite #0
        cmp #(WHEEL_BASE_FRAME - 1)
        bne :+
        lda #(WHEEL_BASE_FRAME + WHEEL_FRAMES - 1)
:       sta $63f8 + 6                   ; turning wheel sprite pointer #0
        sta $63f8 + 7                   ; turning wheel sprite pointer #1
end:
        rts
delay:
        .byte 1
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_sprites
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%11111111                  ; enable sprites
        sta VIC_SPR_ENA

        lda #%00100000
        sta $d010                       ; 8-bit on for sprites x
        lda #0
        sta $d01c                       ; sprite multi-color. hi-res only
        sta $d017                       ; y double resolution
        lda #%00011000
        sta $d01d                       ; x double resolution


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

        ; sprite 0, 1: cursor + cursor overlay
        ; sprite 2: LED
        ; sprite 3, 4: artifact fixes
        ; sprite 5: counter
        ; sprite 6, 7: spinning casette wheels
sprites_x_pos:
        .byte 150, 150, 240,    142+24, 184+24, <(232+24 .MOD 255),     202, 146

sprites_y_pos:
        .byte 150, 150, 218,    32+50, 48+50, 141+50,     120, 92

sprites_color:
        .byte 0, 1, 2,   9, 9,   6,   12, 12

sprites_pointer:
        .byte SPRITE0_POINTER + 17      ; cursor
        .byte SPRITE0_POINTER + 16      ; cursor
        .byte SPRITE0_POINTER + 7       ; LED
        .byte SPRITE0_POINTER + 8       ; artifact fix #1
        .byte SPRITE0_POINTER + 9       ; artifact fix #2
        .byte SPRITE0_POINTER + 5       ; counter
        .byte SPRITE0_POINTER + 4       ; casette wheel
        .byte SPRITE0_POINTER + 4       ; casette wheel
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
        lda button_delay_counter
        beq :+                          ; if on button animation, return
        rts
:
        lda is_playing                  ; already playing ? skip
        bne end

        lda #LED_ON_COLOR
        sta VIC_SPR2_COLOR

        jsr button_play_save
        jsr button_play_plot

        lda is_already_loaded
        beq :+

        lda #1
        sta is_playing                  ; is_playing = true
        lda #$81
        sta $dc0d                       ; turn on cia interrups
        bne end

        lda #0
        sta is_rewinding                ; is_rewinding = false

:       jmp init_song
end:
        rts
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_prev_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_prev_song
        lda button_delay_counter
        beq :+                          ; if on button animation, return
        rts
:
        jsr button_play_restore
        jsr button_rew_save
        jsr button_rew_plot

        lda #LED_ON_COLOR
        sta VIC_SPR2_COLOR

        ldx current_song                ; current_song = max(0, current_song - 1)
        dex
        bpl :+
        ldx #0
:       stx current_song

        lda #0
        sta is_already_loaded           ; is_already_loaded = false

        lda #1
        sta is_rewinding                ; is_rewinding = true

        jsr init_song

        jsr button_rew_restore
        jsr button_play_save
        jsr button_play_plot

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_ff_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_ff_song
        lda button_delay_counter
        beq :+                          ; if on button animation, return
        rts
:
        jsr button_play_restore
        jsr button_ff_save
        jsr button_ff_plot

        lda #LED_ON_COLOR
        sta VIC_SPR2_COLOR

        jsr do_next_song

        jsr button_ff_restore
        jsr button_play_save
        jsr button_play_plot

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_next_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_next_song
        lda button_delay_counter
        beq :+                          ; if on button animation, return
        rts
:
        ldx current_song                ; current_song = min(7, current_song + 1)
        inx
        cpx #TOTAL_SONGS
        bne :+
        ldx #TOTAL_SONGS-1
:       stx current_song

        lda #0
        sta is_already_loaded           ; is_already_loaded = false

        lda #0
        sta is_rewinding                ; is_rewinding = false

        jsr init_song

        rts
.endproc
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_stop_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_stop_song
        lda button_delay_counter
        beq :+                          ; if on button animation, return
        rts
:
        sei

        lda #0
        sta is_playing                  ; is_playing = false

        lda #$7f                        ; turn off cia interrups
        sta $dc0d

        lda #$00
        sta $d418                       ; no volume

        cli

        jsr button_play_restore
        jsr button_stop_save
        jsr button_stop_plot

        lda #LED_OFF_COLOR
        sta VIC_SPR2_COLOR

        lda #10
        sta button_delay_counter

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_song
; decrunches real song, and initializes white song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_song
        sei

        lda #1
        sta is_playing                  ; is_playing = true

        lda #1
        sta is_already_loaded           ; is_already_loaded = true

        lda #0
        sta song_tick                   ; reset song tick
        sta song_tick+1

        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        lda #$00
        sta $d418                       ; no volume

        jsr print_names

        lda current_song                ; x = current_song * 2
        asl
        tax
        jsr init_crunch_data            ; requires x

        dec $01                         ; $34: RAM 100%

        jsr decrunch                    ; copy song

        inc $01                         ; $35: RAM + IO ($D000-$DF00)


;        lda #0
;        jsr WHITE_NOISE_INIT            ; init white noise sid

        lda #75
        sta white_noise_counter         ; play it for one second (50 frames)

        ldx #<$4cc7                     ; init timer
        ldy #>$4cc7                     ; sync with PAL
        stx $dc04                       ; it plays at 50.125hz
        sty $dc05                       ; we have to call this everytime

        lda #$81                        ; turn on cia interrups
        sta $dc0d

        cli
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_real_song
; initializes the real song (not the white noise)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_real_song
        sei

        lda #0
        tax
        tay
        jsr $1000                       ; init song

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
        bcc end           ; if MSB(song_tick) < MSB(song_duration) then
                          ;     song_tick < song_duration
        bne :+            ; if MSB(song_tick) <> MSB(song_duration) then
                          ;     song_tick > song_duration (so song_tick >= song_duration)

        lda song_tick     ; compare low bytes
        cmp $fc
        bcc end           ; if LSB(song_tick) < LSB(song_duration) then
                          ;     song_tick < song_duration
:
        lda #WHEEL_PLAY_DELAY
        sta wheel_delay_counter   ; set animation delay for playing

        jsr do_next_song

        lda #WHEEL_FF_DELAY
        sta wheel_delay_counter   ; restore delay for FF
end:
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

OFFSET_Y_BOTTOM = 7
OFFSET_X_BOTTOM = 13
        ldx #<(bitmap + OFFSET_Y_BOTTOM * 8 * 40 + OFFSET_X_BOTTOM * 8)
        ldy #>(bitmap + OFFSET_Y_BOTTOM * 8 * 40 + OFFSET_X_BOTTOM * 8)
        jsr plot_name

        ;
        ldx current_song                ; update sprite counter
        txa
        clc
        adc #SPRITE0_POINTER + 18
        sta $63f8 + 5                   ; sprite 5
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

        dec ff_delay
        bne @cont

        lda wheel_delay_counter
        sta ff_delay

        php

        lda is_rewinding
        beq @anim_ff
        inc $63f8 + 6                   ; sprite pointer for sprite #0
        lda $63f8 + 6                   ; sprite pointer for sprite #0
        cmp #(WHEEL_BASE_FRAME + WHEEL_FRAMES)
        bne :+
        lda #WHEEL_BASE_FRAME
:       sta $63f8 + 6                   ; turning wheel sprite pointer #0
        sta $63f8 + 7                   ; turning wheel sprite pointer #1
        jmp @done_anim
@anim_ff:
        dec $63f8 + 6                   ; sprite pointer for sprite #0
        lda $63f8 + 6                   ; sprite pointer for sprite #0
        cmp #(WHEEL_BASE_FRAME - 1)
        bne :+
        lda #(WHEEL_BASE_FRAME + WHEEL_FRAMES - 1)
:       sta $63f8 + 6                   ; turning wheel sprite pointer #0
        sta $63f8 + 7                   ; turning wheel sprite pointer #1
@done_anim:
        plp

@cont:
        dec _crunched_byte_lo
_crunched_byte_lo = * + 1
_crunched_byte_hi = * + 2
        lda song_end_addrs              ; self-modyfing. needs to be set correctly before
        rts                             ; decrunch_file is called.
; end_of_data needs to point to the address just after the address
; of the last byte of crunched data.
ff_delay:
        .byte 5

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; check_easteregg
; easteregg bootstrap: Do no compress it
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc check_easteregg
        lda #%11111110                  ; row 0
        sta $dc00
        lda $dc01
        and #%00010000                  ; colum 4
        beq setup_easteregg             ; F1 was pressed?
        rts

setup_easteregg:
        sei

        lda #$00
        sta $d418                       ; no volume

        lda #$7f                        ; turn off cia interrups
        sta $dc0d
        sta $dd0d

        dec $01                         ; $34: RAM 100%

        lda #<song_easter_egg_end_of_data   ; music address
        sta _crunched_byte_lo
        lda #>song_easter_egg_end_of_data
        sta _crunched_byte_hi

        jsr decrunch                    ; decrunch song

        inc $01                         ; $35: RAM + IO ($D000-$DF00)

                                        ; multicolor mode + extended color causes
        lda #%01011011                  ; the bug that blanks the screen
        sta $d011                       ; extended color mode: on
        lda #%00011000
        sta $d016                       ; turn on multicolor

        lda #0                          ; disable all sprites
        sta VIC_SPR_ENA                 ; after song was decrunched

        dec $01                         ; $34: RAM 100%

        ldx #<vader_end_of_data         ; vader address
        ldy #>vader_end_of_data
        stx _crunched_byte_lo
        sty _crunched_byte_hi

        jsr decrunch                    ; decrunch vader image

        ldx #<peron_end_of_data
        ldy #>peron_end_of_data
        stx _crunched_byte_lo
        sty _crunched_byte_hi

        jsr decrunch                    ; decrunch peron image

        ldx #<easteregg_charset_end_of_data
        ldy #>easteregg_charset_end_of_data
        stx _crunched_byte_lo
        sty _crunched_byte_hi

        jsr decrunch                    ; decrunch peron image

        inc $01                         ; $35: RAM + IO ($D000-$DF00)
        jmp init_easteregg
.endproc



.segment "MORECODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_name
; entry:
;       x = LSB bitmap address
;       y = MSB bitmap address
;       $fc,$fd: pointer to string to print. terminated with $ff
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

        lda ($fc),y

        FETCH_NEXT_CHAR                 ; updates $f6/$f7. modifies A,X
        jsr plot_char_0

        inc tmp_counter
        ldy tmp_counter

loop:
        jsr bitmap_next_x               ; updates bitmap: $f8,$f9 / $fa,$fb
        lda ($fc),y
        cmp #$ff
        bne :+
        rts
:       FETCH_NEXT_CHAR                 ; needs A. updates $f6/f7. modifies A,X
        jsr plot_char_1

        inc tmp_counter
        ldy tmp_counter
        lda ($fc),y
        cmp #$ff
        bne :+
        rts
:       FETCH_NEXT_CHAR                 ; needs A. updates $f6/f7. modifies A,X
        jsr plot_char_2

        inc tmp_counter
        ldy tmp_counter
        jsr bitmap_next_x               ; updates bitmap: $f8/$f9, $fa/$fb
        lda ($fc),y
        cmp #$ff
        bne :+
        rts
:       FETCH_NEXT_CHAR                 ; needs A. updates $f6/f7. modifies A,X
        jsr plot_char_3

        inc tmp_counter
        ldy tmp_counter
        jsr bitmap_next_y               ; updates bitmap: $f8/$f9, $fa/$fb
        lda ($fc),y
        cmp #$ff
        bne :+
        rts
:       FETCH_NEXT_CHAR                 ; needs A. updates $f6/f7, modifies A,X
        jsr plot_char_0

        inc tmp_counter
        ldy tmp_counter
        jmp loop
tmp_counter: .byte 0
tmp_mul8_hi: .byte 0
tmp_mul8_lo: .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_0
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_0

        PLOT_ROWS 8, 0, 0, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_1
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_1

        PLOT_ROWS 4, 0, 2, 4            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_prev_x

        PLOT_ROWS 2, 4, 6, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_next_y

        PLOT_ROWS 2, 6, 0, 2            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_next_x               ; restore
        jsr bitmap_prev_y               ; restore

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_2
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_2
        PLOT_ROWS 4, 0, 4, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_next_y

        PLOT_ROWS 4, 4, 0, 4            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_prev_y               ; restore

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; plot_char_3
; entry:
;       $f6,$f7: address of char from charset (8 bytes)
;       $f8,$f9: bitmap
;       $fa,$fb: bitmap + 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc plot_char_3
        PLOT_ROWS 2, 0, 6, 4            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_next_y

        PLOT_ROWS 2, 2, 0, 6            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_prev_x

        PLOT_ROWS 4, 4, 2, 0            ; number_of_rows, char_y_offset, cell_y_offset, cell_x_offset

        jsr bitmap_next_x               ; restore
        jsr bitmap_prev_y               ; restore

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
        PLOT_BYTE $f8, %00000001

        txa
        PLOT_BYTE $fa, %11111110

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
        .repeat 2
                asl
                adc #0
        .endrepeat

        tax                             ; save for next value
        PLOT_BYTE $f8, %00000011

        txa
        PLOT_BYTE $fa, %11111100

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
        .repeat 3
                asl
                adc #0
        .endrepeat

        tax                             ; save for next value
        PLOT_BYTE $f8, %00000111

        txa
        PLOT_BYTE $fa, %11111000

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
        .repeat 4
                asl
                adc #0
        .endrepeat

        tax                             ; save for next value
        PLOT_BYTE $f8, %00001111

        txa
        PLOT_BYTE $fa, %11110000

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
        .repeat 5
                asl
                adc #0
        .endrepeat


        tax                             ; save for next value
        PLOT_BYTE $f8, %00011111

        txa
        PLOT_BYTE $fa, %11100000

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
        .repeat 6
                asl
                adc #0
        .endrepeat

        tax                             ; save for next value
        PLOT_BYTE $f8, %00111111

        txa
        PLOT_BYTE $fa, %11000000

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
        .repeat 7
                asl
                adc #0
        .endrepeat

        tax                             ; save for next value
        PLOT_BYTE $f8, %01111111

        txa
        PLOT_BYTE $fa, %10000000

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
;        PLOT_BYTE $f8, %11111111
        sta ($f8),y
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; bitmap_next_y
; $f8/$f9 += 320
; $fa/$fb += 320
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc bitmap_next_y
        clc
        lda $f8                         ; $f8/$f9 += 320
        adc #64
        sta $f8
        lda $f9
        adc #1
        sta $f9

        clc                             ; $fa/$fb += 320
        lda $fa
        adc #64
        sta $fa
        lda $fb
        adc #1
        sta $fb
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; bitmap_prev_y
; $f8/$f9 -= 320
; $fa/$fb -= 320
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc bitmap_prev_y
        sec
        lda $f8                         ; $f8/$f9 -= 320
        sbc #64
        sta $f8
        lda $f9
        sbc #1
        sta $f9

        sec                             ; $fa/$fb -= 320
        lda $fa
        sbc #64
        sta $fa
        lda $fb
        sbc #1
        sta $fb
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; bitmap_prev_x
; $f8/$f9 -= 8
; $fa/$fb -= 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc bitmap_prev_x
        sec
        lda $f8                         ; $f8/$f9 -= 8
        sta $fa                         ; $fa/$fb = $f8/$f9 (which is the same as -=8)
        sbc #8
        sta $f8
        lda $f9
        sta $fb
        sbc #0
        sta $f9
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; bitmap_next_x
; $f8/$f9 += 8
; $fa/$fb += 8
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc bitmap_next_x
        clc
        lda $fa                         ; $fa/$fb += 8
        sta $f8                         ; $f8/$f9 = $fa/$fb (which is the same as +=8)
        adc #8
        sta $fa
        lda $fb
        sta $f9
        adc #0
        sta $fb
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
is_rewinding:           .byte 0                 ; boolean
current_song:           .byte 0                 ; selected song
joy_button_already_pressed: .byte 0             ; boolean. don't trigger the button again if it is already pressed
mouse_button_already_pressed: .byte 0           ; boolean. don't trigger the button again if it is already pressed
song_tick:              .word 0                 ; word. incremented on each frame, when playing
current_button:         .byte $ff               ; byte. current button when using keyboard (default: -1)
wheel_delay_counter:    .byte WHEEL_FF_DELAY    ; delay counter for wheel animation
button_delay_counter:   .byte 0                 ; delay counter for button animation
white_noise_counter:    .byte 0                 ; who many ticks white noise will be played

buttons_pos:
        .repeat 4, II
        .byte BORDER_LEFT + 18 + 28*II, BORDER_TOP + 145 + 14*II
        .endrepeat

song_names:
        .addr song_1_name
        .addr song_2_name
        .addr song_3_name
        .addr song_4_name
        .addr song_5_name
        .addr song_6_name
        .addr song_7_name
        .addr song_8_name
        .addr song_9_name
        .addr song_10_name
TOTAL_SONGS = (* - song_names) / 2


song_authors:
        .addr song_1_author
        .addr song_2_author
        .addr song_3_author
        .addr song_4_author
        .addr song_5_author
        .addr song_6_author
        .addr song_7_author
        .addr song_8_author
        .addr song_9_author
        .addr song_10_author

song_end_addrs:
        .addr song_1_end_of_data
        .addr song_2_end_of_data
        .addr song_3_end_of_data
        .addr song_4_end_of_data
        .addr song_5_end_of_data
        .addr song_6_end_of_data
        .addr song_7_end_of_data
        .addr song_8_end_of_data
        .addr song_9_end_of_data
        .addr song_10_end_of_data


song_durations:                                 ; measured in "cycles ticks"
        .word 102 * 50                          ; #1 1:42
        .word 91 * 50                           ; #2 1:31
        .word 199 * 50                          ; #3 3:19
        .word (60*2+2) * 50                     ; #4 2:02
        .word 210 * 50                          ; #5 3:30
        .word 95 * 50                           ; #6 1:35
        .word 120 * 50                          ; #7 FIXME
        .word 120 * 50                          ; #8 FIXME
        .word 120 * 50                          ; #9 FIXME
        .word 120 * 50                          ; #10 FIXME


; M, m, w and W uses two chars to render
; M = M'
; m = m&
; w = w(
; W = W)
                ;12345678901234567890123456789
                ; Names must be as long as the longest name
                ; must be $ff terminated
song_1_name:
        scrcode "   Balloon Country Bursts"
        .byte $ff
song_2_name:
        scrcode "      Ryuuju No Dengon      "
        .byte $ff
song_3_name:
        ;Yasashisa Ni Tsutsumareta Nara
        scrcode "Yasashisa Ni Tsutsum&areta.."
        .byte $ff
song_4_name:
        scrcode "          Leet it 3         "
        .byte $ff
song_5_name:
        scrcode "    Pop Goes  The W)orld"
        .byte $ff
song_6_name:
        scrcode "        Se Voce Jurar   "
        .byte $ff
song_7_name:
        scrcode "        M'ongolongo  "
        .byte $ff
song_8_name:
        scrcode "         M'atraca 3 "
        .byte $ff
song_9_name:
        scrcode "        Drogacum&bia"
        .byte $ff
song_10_name:
        scrcode "          Juanelo   "
        .byte $ff


song_1_author:
        scrcode "       Uctum&i"
        .byte $ff
song_2_author:
        scrcode "       Uctum&i"
        .byte $ff
song_3_author:
        scrcode "       Uctum&i"
        .byte $ff
song_4_author:
        scrcode "        CoM'u "
        .byte $ff
song_5_author:
        scrcode "       Uctum&i"
        .byte $ff
song_6_author:
        scrcode "       Uctum&i    "
        .byte $ff
song_7_author:
        scrcode "  Los Pat M'oritas"
        .byte $ff
song_8_author:
        scrcode "  Los Pat M'oritas"
        .byte $ff
song_9_author:
        scrcode "  Los Pat M'oritas"
        .byte $ff
song_10_author:
        scrcode "  Los Pat M'oritas"
        .byte $ff


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


.segment "WHITENOISE"
.incbin "ruido_blanco.sid",$7e

.segment "MUSIC"
.incbin "uct-balloon_country.exo"
song_1_end_of_data:

.incbin "uc-ryuuju_no_dengon.exo"
song_2_end_of_data:

.incbin "uc-yasashisa_ni.exo"
song_3_end_of_data:

.incbin "leetit38580.exo"
song_4_end_of_data:

.incbin "uc-pop_goes_the_world.exo"
song_5_end_of_data:

.incbin "uc-se_voce_jurar.exo"
song_6_end_of_data:

.incbin "mongolongo.exo"
song_7_end_of_data:

.incbin "matraca3.exo"
song_8_end_of_data:

.incbin "dragocumbia.exo"
song_9_end_of_data:

.incbin "juanelo.exo"
song_10_end_of_data:


.incbin "uctumi-marcha_imperial_peronista.exo"
song_easter_egg_end_of_data:

.incbin "peron-map.prg.exo"
peron_end_of_data:

.incbin "vader-map.prg.exo"
vader_end_of_data:

.incbin "easteregg-charset.prg.exo"
easteregg_charset_end_of_data:

.byte 0                 ; ignore

.segment "CHARSET"
charset:
.incbin "names-charset.bin"

.segment "EASTEREGG"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; EasterEgg Notes:
; Charset should be exproted to $4000
; Peron-map should be exported to $4800
; Vader-map should be exported to $4c00
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_easteregg
; FIXME: starting from here, the easter egg code should be compressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_easteregg

        jsr easter_init_screen

        lda #0                          ; clear last line in the two screens
        tax                             ; easter_init_screen doesn't clear them
l0:     sta $4800 + 40*24,x             ; because that function will be called again
        sta $4c00 + 40*24,x             ; when the song ends
        inx
        cpx #40
        bne l0

                                        ; turn VIC on again
        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011                       ; extended color mode: on

        lda #%00001000                  ; no scroll, hires (mono color), 40-cols
        sta $d016                               ; turn off multicolor

        lda #%00100000                  ; video matrix = $0800 (%0010xxxx)
        sta $d018                       ; charset = $0000 (%xxxx000x)

        lda #0
        jsr $1000                       ; init song

        lda #40
        sta $d012

        ldx #<irq_easter_a              ; set irq for easter egg
        ldy #>irq_easter_a
        stx $fffe
        sty $ffff

        cli

easter_mainloop:
        lda easter_sync_irq
        beq easter_mainloop

        dec easter_sync_irq

        jsr $1003

        jsr do_easter_effect
        jsr scroll_easter
        jsr easter_check_song

        jmp easter_mainloop
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_init_screen
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_init_screen
        ldx #0
l0:
        lda #0                          ; color RAM: black for vader image
        sta $d800,x                     ; it will be faded-in
        sta $d900,x
        sta $d9d0,x

        lda #1                          ; white for the bottom part
        sta $dad0,x
        sta $dae8,x
        inx
        bne l0

        lda #0
        tax                             ; A,X = 0
l1:     sta $4800 + 40*15,x             ; clean bottom part of vader
        sta $4800 + 40*15 + 104,x
        sta $4c00 + 40*15,x             ; clean bottom part of peron
        sta $4c00 + 40*15 + 104,x
        inx
        bne l1

        ldy #119                        ; "r" is 127
                                        ; print "Juan Domingo Vader"
l2:     tya
        sta $4800 + 40 * 17 + 16 - 119,y
        iny
        cpy #128
        bne l2

        rts
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_check_song
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_check_song
SONG_DURATION = 120 * 50

        inc song_tick                   ; inc tick
        bne :+
        inc song_tick+1
:

        lda song_tick+1                 ; compare high bytes
        cmp #>SONG_DURATION
        bcc end                         ; if MSB(song_tick) < MSB(song_duration) then

        lda song_tick                   ; compare low bytes
        cmp #<SONG_DURATION
        bcc end                         ; if LSB(song_tick) < LSB(song_duration) then
                                        ;     song_tick < song_duration

        sei
        jsr easter_init_screen
        lda #$ff
        sta easter_effect_idx
        jsr set_next_easter_effect

        lda #0
        sta song_tick
        sta song_tick+1
        jsr $1000                       ; re-init after song is finished
        cli
                                        ; this is supposed to fix a bug
end:
        rts

song_tick: .word 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq_easter_a
; no scroll. switch screen addr
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_easter_a:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda #%00001000                  ; no scrolling, 40 cols
        sta $d016

        lda easter_screen_addr
        sta $d018                       ; charset addr

        lda #180
        sta $d012

        ldx #<irq_easter_b              ; set irq for easter egg
        ldy #>irq_easter_b
        stx $fffe
        sty $ffff

        inc easter_sync_irq

exit_irq:
        asl $d019                       ; clears raster interrupt
        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq_easter_b
; fixed screen addr
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_easter_b:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda #%00100000
        sta $d018                       ; screen addr

        lda #234
        sta $d012

        ldx #<irq_easter_c              ; set irq for easter egg
        ldy #>irq_easter_c
        stx $fffe
        sty $ffff

        jmp exit_irq

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq_easter_c
; scroll
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_easter_c:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda easter_scroll_x
        sta $d016

        lda #40
        sta $d012

        ldx #<irq_easter_a              ; set irq for easter egg
        ldy #>irq_easter_a
        stx $fffe
        sty $ffff

        jmp exit_irq

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; do_easter_effect
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_easter_effect
        jmp easter_effect_wait          ; self-modyfing code
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; set_next_easter_effect
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc set_next_easter_effect
        inc easter_effect_idx
        lda easter_effect_idx
        asl
        tax

        lda easter_effects,x
        sta do_easter_effect + 1
        lda easter_effects + 1,x
        sta do_easter_effect + 2
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_wait
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_wait
        dec counter
        bne :+
        lda #$e6
        sta counter
        bne set_next_easter_effect
:       rts
counter:        .byte $f0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_loop
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_loop
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_switch_peron_vader
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_switch_peron_vader
        dec counter
        bne end

        lda #$20
        sta counter

        lda easter_screen_addr
        eor #%00010000
        sta easter_screen_addr

end:
        rts

counter:
    .byte $20
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_fadein_image
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_fadein_image

        dec delay
        beq init_fade
        rts

init_fade:
        lda #8
        sta delay

        ldx fade_idx
        cpx #EASTER_TOTAL_COLORS
        bne do_fade
        lda #0
        sta fade_idx
        jmp set_next_easter_effect

do_fade:
        lda easter_fade_palette,x

        ldx #15
l0:
        .repeat 13,YY
            sta $d850 + 40*YY,x
        .endrepeat
        inx
        cpx #25
        bne l0

        inc fade_idx
        rts

delay:              .byte 8
fade_idx:           .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_fadein_lefthand
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_fadein_lefthand

        dec delay
        beq init_fade
        rts

init_fade:
        lda #8
        sta delay

        ldx fade_idx
        cpx #EASTER_TOTAL_COLORS
        bne do_fade
        lda #0
        sta fade_idx
        jmp set_next_easter_effect

do_fade:
        lda easter_fade_palette,x

        ldx #9
l0:
        .repeat 7,YY
            sta $d800 + 40*YY,x
        .endrepeat
        inx
        cpx #15
        bne l0

        inc fade_idx
        rts

delay:              .byte 8
fade_idx:           .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_fadein_righthand
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_fadein_righthand

        dec delay
        beq init_fade
        rts

init_fade:
        lda #8
        sta delay

        ldx fade_idx
        cpx #EASTER_TOTAL_COLORS
        bne do_fade
        lda #0
        sta fade_idx
        jmp set_next_easter_effect

do_fade:
        lda easter_fade_palette,x

        ldx #27
l0:
        .repeat 7,YY
            sta $d800 + 40*YY,x
        .endrepeat
        inx
        cpx #32
        bne l0

        inc fade_idx
        rts

delay:              .byte 8
fade_idx:           .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter_effect_fadein_name
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc easter_effect_fadein_name

        dec delay
        beq init_fade
        rts

init_fade:
        lda #8
        sta delay

        ldx fade_idx
        cpx #EASTER_TOTAL_COLORS
        bne do_fade
        lda #0
        sta fade_idx
        jmp set_next_easter_effect

do_fade:
        lda easter_fade_palette,x

        ldx #8
l0:
        sta $d800 + 17 * 40,x
        inx
        cpx #32
        bne l0

        inc fade_idx
        rts

delay:              .byte 8
fade_idx:           .byte 0
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; scroll_easter
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc scroll_easter
        ; speed control
        ldx easter_scroll_x
        dex
        txa
        and #07
        sta easter_scroll_x

        cmp #07
        beq @dothescroll
        rts

@dothescroll:

        ; move the chars to the left
        ldx #0
@loop:  lda $4800 + 40*24+1,x
        sta $4800 + 40*24,x
        inx
        cpx #39
        bne @loop

        ; put next char in column 40
        ldx lines_scrolled
        lda scroll_text,x
        cmp #$ff
        bne @printchar

        ; reached $ff ? Then start from the beginning
        ldx #0
        stx lines_scrolled
        lda scroll_text

@printchar:
        ora #192
        sta $4800 + 40*24 +39
        inx
        stx lines_scrolled

endscroll:
        rts

lines_scrolled:
            .byte 0
scroll_text:
        scrcode "hola amiguitos.... pasenme el texto para el scroll... les mando un abrazo desde el lado oscuro..... "
        scrcode ""
        .byte $ff
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter egg global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
easter_sync_irq:        .byte 0                 ; boolean
easter_scroll_x:        .byte 0                 ; 0-7. smooth scroll
easter_screen_addr:     .byte %00110000         ; charset addr for easter egg
easter_effects:
        .addr easter_effect_wait
        .addr easter_effect_fadein_image
        .addr easter_effect_wait
        .addr easter_effect_wait
        .addr easter_effect_wait
        .addr easter_effect_fadein_lefthand
        .addr easter_effect_wait
        .addr easter_effect_fadein_righthand
        .addr easter_effect_wait
        .addr easter_effect_fadein_name
        .addr easter_effect_switch_peron_vader
easter_effect_idx:      .byte 0
easter_fade_palette:    .byte $00,$0b,$0c,$0f,$01
EASTER_TOTAL_COLORS = * - easter_fade_palette

