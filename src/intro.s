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

.import player_main

.segment "INTROCODE"

; INTRO SCREEN  $1c00
; INTRO CHARSET $2000

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void intro_main()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export intro_main
.proc intro_main
        sei                             ; disable interrupts

        lda #$35                        ; no basic, no kernal
        sta $01

        lda #$00
        sta $d01a                       ; no raster IRQ
        lda #$7f
        sta $dc0d                       ; no timer A and B IRQ
        sta $dd0d

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK timer A interrupt
        lda $dd0d                       ; ACK timer B interrupt

        lda $dd00                       ; Vic bank 0: $0000-$3FFF (default)
        and #$fc
        ora #3
        sta $dd00

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%00011000                  ; screen addr 0x0400, charset at $2000
        sta $d018

        lda #0                          ; no sprites
        sta VIC_SPR_ENA

        lda #0                          ; black for background color
        sta $d020
        sta $d021
        sta $d022
        sta $d023


        ldx #$00
l:      lda logo_label + $0000,x        ; paint logo
        sta $0400 + $0000,x
        tay
        lda logo_attrib_data,y
        and #%00001000                  ; only 0 or 8. both are black in MC mode
        sta $d800,x

        lda logo_label + $0100,x
        sta $0400 + $0100,x
        tay
        lda logo_attrib_data,y
        and #%00001000                  ; only 0 or 8. both are black in MC mode
        sta $d900,x

        lda logo_label + $0200,x
        sta $0400 + $0200,x
        tay
        lda logo_attrib_data,y
        and #%00001000                  ; only 0 or 8. both are black in MC mode
        sta $da00,x

        lda logo_label + $02e8,x
        sta $0400 + $02e8,x
        tay
        lda logo_attrib_data,y
        and #%00001000                  ; only 0 or 8. both are black in MC mode
        sta $dae8,x

        inx
        bne l

:       jsr fade_delay_2                ; delay
        dec delay_in_idx
        bne :-

        jsr fade_in

        cli


        lda #5
        sta delay

l0:     ldx #0
l1:     ldy #0

l2:
        lda #%01111111                  ; space ?
        sta CIA1_PRA                    ; row 7
        lda CIA1_PRB
        and #%00010000                  ; col 4
        beq end

        dey
        bne l2

        dex
        bne l1

        dec delay
        bne l0

end:
        jsr fade_out_logo

:       jsr fade_delay_2                ; delay
        dec delay_out_idx
        bne :-

        ldx #$3f                        ; only use 64 bytes of stack
        txs

        jsr save_easteregg

        jmp player_main

delay:
        .byte 0
delay_in_idx:
        .byte 20
delay_out_idx:
        .byte 20
.endproc

.proc fade_in
        jsr fade_in_logo
        jmp fade_in_chars
.endproc

.proc fade_in_logo
        ldx fade_in_colors_d022_idx
        lda fade_in_colors_d022,x
        sta $d022
        inc fade_in_colors_d022_idx

        ldx fade_in_colors_d023_idx
        lda fade_in_colors_d023,x
        sta $d023
        inc fade_in_colors_d023_idx

        ldy #0
l0:     lda $d800,y
        and #$0f
        pha
        lda $0400,y
        tax
        pla
        cmp logo_attrib_data,x
        beq next2
        tax
        lda fade_in_colors_mc,x
        sta $d800,y

next2:
        lda $d900,y
        and #$0f
        pha
        lda $0500,y
        tax
        pla
        cmp logo_attrib_data,x
        beq next3
        tax
        lda fade_in_colors_mc,x
        sta $d900,y

next3:
        dey
        bne l0

        jsr fade_delay_2

        dec iters
        beq end
        jmp fade_in_logo
end:
        rts
iters:
        .byte 8
.endproc

.proc fade_in_chars
        ldy #0
l0:
        lda $da00,y
        and #$0f
        pha
        lda $0600,y
        tax
        pla
        cmp logo_attrib_data,x
        beq next0
        tax
        lda fade_in_colors_mc,x
        sta $da00,y
next0:
        iny
        bne l0

l1:     lda $db00,y
        and #$0f
        pha
        lda $0700,y
        tax
        pla
        cmp logo_attrib_data,x
        beq next1
        tax
        lda fade_in_colors_mc,x
        sta $db00,y
next1:

        iny
        cpy #$e8
        bne l1

        jsr fade_delay_2

        dec iters
        bne fade_in_chars
        rts
iters:
        .byte 16
.endproc

.proc fade_out_logo

        dec fade_in_colors_d022_idx
        ldx fade_in_colors_d022_idx             ; reuse fade_in palette
        lda fade_in_colors_d022,x               ; from back to front
        sta $d022

        dec fade_in_colors_d023_idx
        ldx fade_in_colors_d023_idx             ; reuse fade_in palette
        lda fade_in_colors_d023,x               ; from back to front
        sta $d023


        ldy #0
l0:     lda $d800,y
        and #$0f
        tax
        lda fade_out_colors_mc,x
        sta $d800,y

        lda $d900,y
        and #$0f
        tax
        lda fade_out_colors_mc,x
        sta $d900,y

        lda $da00,y
        and #$0f
        tax
        lda fade_out_colors_mc,x
        sta $da00,y

        dey
        bne l0

l1:     lda $db00,y
        and #$0f
        tax
        lda fade_out_colors_mc,x
        sta $db00,y
        iny
        cpy #$e8
        bne l1

        jsr fade_delay

        dec iters
        bne fade_out_logo
        rts
iters:
        .byte 8
.endproc

.proc fade_delay
        ldy #4
l1:     ldx #0
l0:     dex
        bne l0
        dey
        bne l1
        rts
.endproc

.proc fade_delay_2
        ldy #20
l1:     ldx #0
l0:     dex
        bne l0
        dey
        bne l1
        rts
.endproc

.proc save_easteregg
        ldx #0

l0:
        lda easter_egg_bundle_begin,x
        sta $140,x
        lda easter_egg_bundle_begin + $0100,x
        sta $240,x
        lda easter_egg_bundle_begin + $0200,x
        sta $340,x
        lda easter_egg_bundle_begin + $0300,x
        sta $440,x
        lda easter_egg_bundle_begin + $0400,x
        sta $540,x
        lda easter_egg_bundle_begin + $0500,x
        sta $640,x
        lda easter_egg_bundle_begin + $05c0,x
        sta $700,x
        inx
        bne l0
        rts
.endproc


fade_in_colors_d022_idx: .byte 0
fade_in_colors_d023_idx: .byte 0
fade_in_colors_d022:
        .byte $00,$09,$0b,$02,$04,$08,$0c,$0e
fade_in_colors_d023:
        .byte $00,$00,$00,$06,$06,$09,$09,$0b

fade_in_colors_mc:
        ;       0   1   2   3   4   5   6   7
        ;       8   9   a   b   c   d   e   f
        .byte $06,$01,$04,$07,$05,$03,$02,$01
        .byte $0e,$09,$0c,$0f,$0d,$0b,$0a,$09

        ;     $00,$06,$02,$04,$05,$03,$07,$01

fade_out_colors_mc:
        ;       0   1   2   3   4   5   6   7
        ;       8   9   a   b   c   d   e   f
        .byte $00,$07,$06,$05,$02,$04,$00,$03
        .byte $08,$0f,$0e,$0d,$0a,$0c,$08,$0b

;       .byte $0d,$0f,$0a,$0e,$0c,$08,$0b,$09,$00
;       .byte $01,$07,$03,$05,$04,$02,$06,$00,$00

logo_attrib_data:
        .incbin "octavo-arlequin-pvmlogoc64_1m_remix-colors.bin"

.segment "INTROSCREEN"
logo_label:
        .incbin "octavo-arlequin-pvmlogoc64_1m_remix-map.bin"

.segment "INTROCHARSET"
        .incbin "octavo-arlequin-pvmlogoc64_1m_remix-charset.bin"

.segment "EASTEREGG1"

easter_egg_bundle_begin:
        .incbin "easteregg-exo.prg"

.export EASTEREGG_SIZE
EASTEREGG_SIZE = * - easter_egg_bundle_begin
