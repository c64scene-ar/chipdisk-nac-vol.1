.segment "EASTEREGG"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; EasterEgg Notes:
; Charset should be exported to $4000
; Peron-map should be exported to $4800
; Vader-map should be exported to $4c00
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

SCREEN_RAM = $4800                      ; used for scroll
EN_ROW = 21
SP_ROW = 24
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_easteregg
; FIXME: starting from here, the easter egg code should be compressed
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export init_easteregg
.proc init_easteregg

        jsr easter_init_screen

        lda #0                          ; clear last line in the two screens
        tax                             ; easter_init_screen doesn't clear them
l0:     sta $4800 + 40*21,x             ; because that function will be called again
        sta $4c00 + 40*21,x
        inx
        cpx #(40*4)
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
        jsr scroll_easter_en
        jsr scroll_easter_sp
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
        sta $4c00 + 40*15,x             ; clean bottom part of peron
        inx
        cpx #(40*6)                     ; until row 21
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

        lda #210
        sta $d012

        ldx #<irq_easter_c              ; set irq for easter egg
        ldy #>irq_easter_c
        stx $fffe
        sty $ffff

        jmp exit_irq

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq_easter_c
; scroll english
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_easter_c:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda easter_scroll_en_x
        sta $d016

        lda #234
        sta $d012

        ldx #<irq_easter_d              ; set irq for easter egg
        ldy #>irq_easter_d
        stx $fffe
        sty $ffff

        jmp exit_irq

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; irq_easter_d
; scroll spanish
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_easter_d:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda easter_scroll_sp_x
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
; scroll_easter_en
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc scroll_easter_en
        ldy #0

        inc sin_table_idx_en
        ldx sin_table_idx_en
        lda sin_table,x
        clc
        adc easter_scroll_speed_en
        bcc l0
        iny
l0:
        clc
        adc #192
        bcc l1
        iny
l1:
        sta easter_scroll_speed_en              ; save value

        cpy #0
        bne l2
        rts
l2:
        sec
        lda easter_scroll_en_x
        sbc ident,y
        php                                     ; save 'N'
        and #07
        sta easter_scroll_en_x
        plp                                     ; restore 'N'
        bmi @dothescroll                        ; if N, then scroll
        rts

@dothescroll:

        ; move the chars to the left
        ldx #0
@loop:  lda SCREEN_RAM + 40*EN_ROW+1,x
        sta SCREEN_RAM + 40*EN_ROW,x
        inx
        cpx #39
        bne @loop

        ; put next char in column 40
        ldx lines_scrolled_en
scroll_addr = *+2
        lda scroll_text_en,x
        cmp #$ff
        bne @printchar

        ; reached $ff ? Then start from the beginning
        ldx #0
        stx lines_scrolled_en
        lda #>scroll_text_en
        sta scroll_addr

        lda scroll_text_en

@printchar:
        ora #192
        sta SCREEN_RAM + 40*EN_ROW +39
        inx
        stx lines_scrolled_en
        bne endscroll
        inc scroll_addr

endscroll:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; scroll_easter_sp
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc scroll_easter_sp
        ldy #0

        inc sin_table_idx_sp
        ldx sin_table_idx_sp
        lda sin_table,x
        clc
        adc easter_scroll_speed_sp
        bcc l0
        iny
l0:
        clc
        adc #192
        bcc l1
        iny
l1:
        sta easter_scroll_speed_sp              ; save value

        cpy #0
        bne l2
        rts
l2:
        sec
        lda easter_scroll_sp_x
        sbc ident,y
        php                                     ; save 'N'
        and #07
        sta easter_scroll_sp_x
        plp                                     ; restore 'N'
        bmi @dothescroll                        ; if N, then scroll
        rts

@dothescroll:

        ; move the chars to the left
        ldx #0
@loop:  lda SCREEN_RAM + 40*SP_ROW+1,x
        sta SCREEN_RAM + 40*SP_ROW,x
        inx
        cpx #39
        bne @loop

        ; put next char in column 40
        ldx lines_scrolled_sp
scroll_addr = *+2
        lda scroll_text_sp,x
        cmp #$ff
        bne @printchar

        ; reached $ff ? Then start from the beginning
        ldx #0
        stx lines_scrolled_sp
        lda #>scroll_text_sp
        sta scroll_addr

        lda scroll_text_sp

@printchar:
        ora #192
        sta SCREEN_RAM + 40*SP_ROW +39
        inx
        stx lines_scrolled_sp
        bne endscroll
        inc scroll_addr

endscroll:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; easter egg global variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
lines_scrolled_en:      .byte 0
lines_scrolled_sp:      .byte 0
scroll_text_en:
        scrcode "in our twentieth anniversary pvm brings you a music disk full of 8 bit sounds for your ears. press play and enjoy like in the old times. "
        scrcode "this couldn't have been possible without the esoteric and intimate code of riq; triangle and square waves domestication of uctumi, "
        scrcode "naku/los pat moritas and co mu; picassian brush strokes and hieroglyph charsets of alakran and arlequin; "
        scrcode "arachnic bugfixes of munshkr and telepathic collaboration from the rest of the pungas. "
        scrcode "stay tuned for new pvm releases and don't hesitate to contact us at pungas.space if you are a talented human waste "
        scrcode "eager to participate in future releases."
                ;1234567890123456789001234567890123456789
        scrcode "                                        "
        .byte $ff

scroll_text_sp:
        scrcode "en nuestro veinteavo aniversario pvm trae a tus oi'dos un compilado "
        scrcode "de mu'sica lleno de sonidos 8 bits. presiona play y disfruta como en los viejos tiempos. "
        scrcode "esta entrega no hubiera sido posible sin el i'ntimo y esote'rico co'digo de manos de riq; "
        scrcode "la domesticacio'n de las ondas triangulares y cuadradas por parte de uctumi, naku/los pat moritas y co mu; "
        scrcode "los trazos picassescos y los juegos de caracteres jerogli'ficos de alakra'n y arlequi'n; "
        scrcode "la ara'cnida correccio'n de defectos de munshkr y la colaboracio'n telepa'tica del resto de los pungas. "
        scrcode "estate atento a nuevas entregas de pvm y no dudes en contactarnos en pungas.space si te consideras "
        scrcode "un desperdicio humano con talento y deseos de participar en nuestras producciones futuras."
                ;1234567890123456789001234567890123456789
        scrcode "                                        "
        .byte $ff

easter_sync_irq:        .byte 0                 ; boolean
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

easter_scroll_en_x:     .byte 0                 ; 0-7. smooth scroll for english
easter_scroll_sp_x:     .byte 0                 ; 0-7. smooth scroll for spanish
easter_scroll_speed_en: .byte 0                 ; speed control
easter_scroll_speed_sp: .byte 0                 ; speed control

sin_table_idx_en:       .byte 0
sin_table_idx_sp:       .byte 128
sin_table:
; autogenerated table: easing_table_generator.py -s256 -m255 -aTrue sin
.byte   3,  6,  9, 13, 16, 19, 22, 25
.byte  28, 31, 34, 37, 41, 44, 47, 50
.byte  53, 56, 59, 62, 65, 68, 71, 74
.byte  77, 80, 83, 86, 89, 92, 95, 98
.byte 100,103,106,109,112,115,117,120
.byte 123,126,128,131,134,136,139,142
.byte 144,147,149,152,154,157,159,162
.byte 164,167,169,171,174,176,178,180
.byte 183,185,187,189,191,193,195,197
.byte 199,201,203,205,207,208,210,212
.byte 214,215,217,219,220,222,223,225
.byte 226,228,229,231,232,233,234,236
.byte 237,238,239,240,241,242,243,244
.byte 245,246,247,247,248,249,249,250
.byte 251,251,252,252,253,253,253,254
.byte 254,254,255,255,255,255,255,255
.byte 255,255,255,255,255,254,254,254
.byte 253,253,253,252,252,251,251,250
.byte 249,249,248,247,247,246,245,244
.byte 243,242,241,240,239,238,237,236
.byte 234,233,232,231,229,228,226,225
.byte 223,222,220,219,217,215,214,212
.byte 210,208,207,205,203,201,199,197
.byte 195,193,191,189,187,185,183,180
.byte 178,176,174,171,169,167,164,162
.byte 159,157,154,152,149,147,144,142
.byte 139,136,134,131,128,126,123,120
.byte 117,115,112,109,106,103,100, 98
.byte  95, 92, 89, 86, 83, 80, 77, 74
.byte  71, 68, 65, 62, 59, 56, 53, 50
.byte  47, 44, 41, 37, 34, 31, 28, 25
.byte  22, 19, 16, 13,  9,  6,  3,  0

ident:
        .byte 0, 1, 2, 3, 4, 5, 6, 7
