.segment "EASTEREGG"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

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
.export init_easteregg
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
scroll_addr = *+2
        lda scroll_text,x
        cmp #$ff
        bne @printchar

        ; reached $ff ? Then start from the beginning
        ldx #0
        stx lines_scrolled
        lda #>scroll_text
        sta scroll_addr

        lda scroll_text

@printchar:
        ora #192
        sta $4800 + 40*24 +39
        inx
        stx lines_scrolled
        bne endscroll
        inc scroll_addr

endscroll:
        rts

lines_scrolled:
            .byte 0
scroll_text:
        scrcode "in our twentieth anniversary pvm brings you a music disk full of 8 bit sounds for your ears. press play and enjoy like in the old times. "
        scrcode "this couldn't have been possible without the esoteric and intimate code of riq; triangle and square waves domestication of uctumi, "
        scrcode "naku/los pat moritas and co mu; picassian brush strokes and hieroglyph charsets of alakran and arlequin; "
        scrcode "arachnic bugfixes of munshkr and telepathic collaboration from the rest of the pungas. "
        scrcode "stay tuned for new pvm releases and don't hesitate to contact us at pungas.space if you are a talented human waste eager to participate in future releases"
                ;12345678901234567890123456789
        scrcode "                             "
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

