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

.segment "INTROCODE"

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

        lda #%00011000                  ; screen addr 0x1c00, charset at $2000
        sta $d018

        lda #0                          ; no sprites
        sta VIC_SPR_ENA

        lda #0                          ; black for background color
        sta $d020
        sta $d021
        lda #14                         ; multicolor #1
        sta $d022
        lda #11
        sta $d023                       ; multicolor #2



        ldx #$00                        ; set colors for logo
l:      lda logo_label + $0000,x
        sta $0400 + $0000,x
        tay
        lda logo_attrib_data,y
        sta $d800,x                     ; colors for the chars

        lda logo_label + $0100,x
        sta $0400 + $0100,x
        tay
        lda logo_attrib_data,y
        sta $d800 + $0100,x             ; colors for the chars

        lda logo_label + $0200,x
        sta $0400 + $0200,x
        tay
        lda logo_attrib_data,y
        sta $d800 + $0200,x             ; colors for the chars

        lda logo_label + $02e8,x
        sta $0400 + $02e8,x
        tay
        lda logo_attrib_data,y
        sta $d800 + $02e8,x             ; colors for the chars

        inx
        bne l

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
        jmp fade_out

delay:
        .byte 0
.endproc

.proc fade_out

loop:
        lda $d020
        and #$0f
        tax
        lda fade_colors_hires,x
        sta $d020
        sta $d021

        lda $d022
        and #$0f
        tax
        lda fade_colors_hires,x
        sta $d022

        lda $d023
        and #$0f
        tax
        lda fade_colors_hires,x
        sta $d023

        ldy #0
l0:     lda $d800,y
        and #$0f
        tax
        lda fade_colors_mc,x
        sta $d800,y

        lda $d900,y
        and #$0f
        tax
        lda fade_colors_mc,x
        sta $d900,y

        lda $da00,y
        and #$0f
        tax
        lda fade_colors_mc,x
        sta $da00,y

        dey
        bne l0

l1:     lda $db00,y
        and #$0f
        tax
        lda fade_colors_mc,x
        sta $db00,y
        iny
        cpy #$e8
        bne l1

        jsr fade_delay

        dec iters
        bne loop
        rts
iters:
        .byte 16
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


fade_colors_hires:
        ;       0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
        .byte $00,$0d,$0b,$0f,$02,$0a,$00,$03,$04,$06,$0e,$09,$08,$07,$0c,$05
;       .byte $01,$0d,$07,$03,$0f,$05,$0a,$0e,$0c,$08,$04,$02,$0b,$09,$06,$00

fade_colors_mc:
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
