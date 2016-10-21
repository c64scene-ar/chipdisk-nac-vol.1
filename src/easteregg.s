;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_easteregg
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export start
.proc start
        sei

        lda #$35
        sta $01                         ; no basic, no kernal

                                        ; turn VIC on 
        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011                       ; extended color mode: off

        lda #%00001000                  ; no scroll, hires (mono color), 40-cols
        sta $d016                       ; turn off multicolor

        lda #0
        sta VIC_SPR_ENA

        lda #$00
        sta $d01a                       ; no raster IRQ
        lda #$7f
        sta $dc0d                       ; no timer A and B IRQ
        sta $dd0d

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK timer A interrupt
        lda $dd0d                       ; ACK timer B interrupt

        lda $dd00                       ; Vic bank 2: $8000-$bFFF
        and #$fc
        ora #1
        sta $dd00

        lda #0
        sta $d020
        sta $d021

        lda #%00010100                  ; screen point to $0800
        sta $d018                       ; charset at $1800 (VIC)

        lda #$81                        ; turn on cia interrups
        sta $dc0d                       ; reuses the IRQ from chipdisk

        ldx #<irq_vector
        ldy #>irq_vector
        stx $fffe
        sty $ffff

        ldx #<$4cc7                     ; init timer
        ldy #>$4cc7                     ; sync with PAL
        stx $dc04                       ; it plays at 50.125hz
        sty $dc05

;        jsr $1000

        cli

        ldx #0
l0:
        lda easteregg_color + $0000,x
        sta $d800 + $0000,x
        lda easteregg_color + $0100,x
        sta $d800 + $0100,x
        lda easteregg_color + $0200,x
        sta $d800 + $0200,x
        lda easteregg_color + $02e8,x
        sta $d800 + $02e8,x

        lda easteregg_screen + $0000,x
        sta $8400 + $0000,x
        lda easteregg_screen + $0100,x
        sta $8400 + $0100,x
        lda easteregg_screen + $0200,x
        sta $8400 + $0200,x
        lda easteregg_screen + $02e8,x
        sta $8400 + $02e8,x

        inx
        bne l0

self:
        jmp self
.endproc

.proc irq_vector
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        lda $dc0d                       ; clears CIA interrupts, in particular timer A
;        jsr $1003

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

easteregg_screen:
;screen char data
.byte  $78,$e4,$f7,$ff,$ef,$f7,$79,$e2,$e4,$7b,$fb,$62,$6c,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$f8,$ff,$ef,$f7,$6f,$e2,$e4,$f8,$ff,$20,$e2,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$a0,$a0,$f7,$ff,$f9,$e3,$79,$63,$e2,$e4,$f5,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e4,$ef,$ef,$e4,$a0,$a0,$a0,$a0,$e0,$a0,$a0,$a0,$a0,$e4,$ef,$ef,$e4,$a0,$a0,$a0,$a0
.byte  $a0,$a0,$a0,$a0,$a0,$e3,$62,$e2,$a0,$62,$64,$a0,$a0,$a0,$a0,$a0,$a0,$ec,$77,$6f,$79,$62,$79,$63,$f9,$a0,$a0,$e0,$a0,$a0,$f9,$63,$79,$62,$79,$6f,$77,$fb,$a0,$a0
.byte  $62,$ff,$78,$e2,$e2,$e2,$e2,$e2,$20,$78,$fb,$a0,$a0,$a0,$a0,$a0,$7e,$62,$a0,$a0,$a0,$a0,$a0,$a0,$62,$7c,$a0,$e0,$a0,$7e,$62,$a0,$a0,$a0,$a0,$a0,$a0,$62,$7c,$a0
.byte  $f9,$f9,$ef,$ef,$ef,$ef,$ef,$e4,$e3,$e3,$f4,$a0,$a0,$a0,$a0,$7f,$a0,$a0,$e8,$c3,$c3,$c3,$fb,$a0,$a0,$f6,$f4,$e0,$ea,$f5,$a0,$a0,$ec,$c3,$c3,$c3,$e8,$a0,$a0,$fe
.byte  $f9,$e4,$f7,$f8,$f8,$f8,$62,$79,$20,$20,$a0,$a0,$a0,$a0,$a0,$a0,$ec,$cb,$fb,$4f,$20,$7c,$fc,$ff,$a0,$a0,$e1,$e0,$61,$a0,$a0,$7f,$fe,$7e,$20,$50,$ec,$ca,$fb,$a0
.byte  $e3,$f8,$62,$62,$79,$79,$ff,$e2,$e4,$c8,$a0,$a0,$a0,$a0,$a0,$a0,$fe,$7e,$a0,$20,$20,$20,$f4,$7c,$62,$a0,$dd,$e0,$dd,$a0,$62,$7e,$ea,$20,$20,$20,$a0,$7c,$fc,$a0
.byte  $e2,$e2,$e2,$78,$78,$78,$78,$77,$64,$76,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ef,$62,$6f,$20,$64,$c3,$4e,$cb,$a0,$dd,$e0,$dd,$a0,$ca,$4d,$c3,$64,$20,$6f,$62,$ef,$a0,$a0
.byte  $e3,$e3,$e3,$e4,$ef,$ef,$ef,$f9,$f9,$76,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$f8,$c3,$78,$78,$79,$fe,$a0,$a0,$c7,$e0,$c8,$e0,$a0,$fc,$79,$78,$78,$c3,$f8,$a0,$a0,$a0
.byte  $f7,$f7,$f8,$f8,$f8,$62,$62,$62,$79,$e1,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e5,$e0,$e7,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $79,$79,$79,$c3,$c3,$c3,$c3,$c3,$78,$76,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$61,$a0,$a0,$a0,$e1,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$a0,$a0,$a0,$a0,$ef,$78,$62,$f7,$c8,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$d5,$a0,$e1,$a0,$a0,$a0,$61,$a0,$c9,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$a0,$e4,$e2,$7f,$f7,$ef,$e2,$63,$20,$e0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ea,$e1,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$61,$f5,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $e2,$7f,$f8,$e3,$e2,$6f,$62,$f7,$e3,$e3,$e5,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$20,$78,$20,$f9,$ef,$f9,$20,$78,$60,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $ef,$f9,$c3,$62,$e3,$f9,$7f,$79,$7b,$64,$e1,$e0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$f7,$e3,$f8,$f8,$f8,$e3,$f7,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $e3,$ef,$f9,$c3,$62,$e3,$ec,$7f,$f7,$ef,$78,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ef,$ef,$e4,$ef,$ef,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $f8,$f7,$e3,$a0,$e2,$79,$f7,$f9,$63,$79,$f8,$f4,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ec,$f8,$e3,$f7,$f8,$f7,$e3,$f8,$fb,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$e4,$e2,$62,$e3,$e2,$79,$f8,$e4,$e2,$63,$fe,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ae,$e2,$c3,$c3,$e2,$e2,$e2,$c3,$c3,$e2,$ae,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $77,$f8,$e4,$7f,$62,$e3,$f9,$c0,$f8,$7d,$fe,$ec,$e1,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e3,$ff,$a0,$a0,$a0,$a0,$a0,$7f,$e3,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $a0,$c4,$e3,$f9,$78,$62,$e3,$a0,$7e,$fe,$7e,$79,$f8,$fb,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$6f,$78,$78,$78,$6f,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
.byte  $e4,$e2,$79,$f7,$a0,$a0,$a0,$7e,$fe,$7e,$f7,$ec,$77,$6c,$fb,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$d5,$62,$f8,$62,$c9,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e0
.byte  $f7,$a0,$a0,$a0,$a0,$a0,$7f,$a0,$7f,$fe,$7f,$ff,$6c,$ec,$20,$fb,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ae,$fe,$a0,$a0,$a0,$fc,$ae,$a0,$a0,$a0,$a0,$a0,$a0,$e0,$e0,$7e
.byte  $a0,$a0,$a0,$a0,$ec,$7f,$a0,$7f,$a0,$7f,$a0,$7f,$ec,$20,$fe,$7e,$fb,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$e0,$e0,$7e,$fb
.byte  $a0,$a0,$f9,$e2,$fe,$ec,$7f,$ec,$6c,$a0,$7f,$ec,$6c,$a0,$7e,$fe,$61,$f9,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$ec,$6a,$ea,$6c

easteregg_color:
;screen color data
.byte  $07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$01,$01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$07,$07,$07,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$01,$07,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$07,$07,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$01,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$07,$07,$07,$07,$07,$01,$07,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01
