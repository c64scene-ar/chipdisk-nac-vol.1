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
.macpack macros                         ; our macros
.include "c64.inc"                      ; c64 constants

ZP_NMI_HANDLER  = $70                   ; here will be an `rti`


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "CODE"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CODE"

        lda #$35                        ; no basic, no kernal, vic
        sta $01
        jmp hicode_main

chipdisk_begin:
.incbin "chipdisk-exo.prg"
chipdisk_end:
        .byte 0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "HICODE"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "HICODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void hicode()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc hicode_main
        sei                             ; disable interrupts
        lda #$01
        sta $d01a                       ; raster IRQ
        lda #$7f
        sta $dc0d                       ; no timer A and B IRQ
        sta $dd0d

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK timer A interrupt
        lda $dd0d                       ; ACK timer B interrupt

        lda $dd00                       ; Vic bank 3: $C000-$FFFF
        and #%11111100
        ora #0
        sta $dd00

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%10000000                  ; screen addr 0x2000, charset at $0000, bitmap at $0000
        sta $d018

        lda #%00111011                  ; bitmap mode enabled
        sta $d011

        lda #0                          ; no sprites
        sta VIC_SPR_ENA

        lda #0                          ; black for background color
        sta $d020
        sta $d021
        sta $d022
        sta $d023

        ldx #0

@l0:    lda bitmap_color + $000,x       ; copy color ram, first 14 rows
        sta $d800,x
        lda bitmap_color + $100,x
        sta $d900,x
        lda bitmap_color + $130,x
        sta $d900 + $030,x
        inx
        bne @l0

        lda #0                          ; rest should be 1 (white)
@l00:   sta $da30,x                     ; for the labels
        sta $db00,x
        inx
        bne @l00

        ldx #<irq_bitmap                ; setup irq
        ldy #>irq_bitmap
        stx $fffe
        sty $ffff

        lda #$40                        ; `rti`
        sta ZP_NMI_HANDLER

        ldx #<ZP_NMI_HANDLER            ; restore key disabled
        ldy #>ZP_NMI_HANDLER
        stx $fffa
        sty $fffb

        cli

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_loop:
        lda sync_raster_irq
        bne do_effects

post_sync_raster:

        lda effect_wip
        bne @l0                         ; skip delay if effect is still in progress

                                        ; wait for space bar
                                        ; or wait for delay to end
        lda delay_space_bar
        cmp #120                        ; ~2 seconds
        beq end_intro

@l0:
        lda #%01111111                  ; space ?
        sta CIA1_PRA                    ; row 7
        lda CIA1_PRB
        and #%00010000                  ; col 4
        bne main_loop
        jmp end_intro

do_effects:
        dec sync_raster_irq

        lda effect_wip
        bne @start_effect
        jmp post_sync_raster            ; effect finished, don't do it again


@start_effect:

        ldx #0                          ; do "change letter" effect
        stx effect_wip                  ; reset "effect_wip" if not done

@l0:
        lda $f800 + 40 * 15,x           ; modify: "pungas de villa..."
        cmp label_linyera_ok + 40 * 0,x
        beq @next0
        inc $f800 + 40 * 15,x
        inc effect_wip

@next0:
        lda $f800 + 40 * 18,x           ; modify "presents..."
        cmp label_linyera_ok + 40 * 1,x
        beq @next1
        inc $f800 + 40 * 18,x
        inc effect_wip

@next1:
        lda $f800 + 40 * 21,x           ; modify "de musica linyera..."
        cmp label_linyera_ok + 40 * 2,x
        beq @next2
        inc $f800 + 40 * 21,x
        inc effect_wip

@next2:
        lda $f800 + 40 * 24,x           ; modify "de musica linyera..."
        cmp label_linyera_ok + 40 * 3,x
        beq @skip
        inc $f800 + 40 * 24,x
        inc effect_wip

@skip:
        inx
        cpx #40
        bne @l0

        jmp post_sync_raster

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void end_intro()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc end_intro

        sei
        lda $dd00                       ; Vic bank 0: $0000-$3FFF
        and #%11111100
        ora #3
        sta $dd00

        lda #%00001000                  ; no scroll, multi-color off, 40-cols
        sta $d016

        lda #%00010100                  ; screen addr 0x0400, charset at $1000
        sta $d018

        lda #%00011011                  ; bitmap mode disabled
        sta $d011

        asl $d019                       ; ack raster irq
        lda #0
        sta $d01a                       ; disable irq

        ldx #0
        lda #$20
@l0:    sta $0400,x                     ; clears the screen memory
        sta $0500,x
        sta $0600,x
        sta $06e8,x
        inx                             ; 1000 bytes = 40*25
        bne @l0

        lda #0                          ; ram color. white. for PVM logo
@l1:
        sta $d800,x
        sta $d900,x
        sta $da00,x
        sta $dae8,x
        inx
        bne @l1

        lda #16                         ; P
        sta $7e4
        lda #22                         ; V
        sta $7e5
        lda #13                         ; M
        sta $7e6

        lda #15                         ; gray
        sta $dbe4
        sta $dbe5
        sta $dbe6
        sta $dbe7

        ldx #0                          ; decrunch table. clean it
@l2:    sta $0200,x
        inx
        bne @l2

        ldx #0
@l3:    lda decruncher,x                ; copy decruncher to $400
        sta $0400,x
        lda decruncher + $0100,x
        sta $0500,x
        inx
        bne @l3

        ldx #<chipdisk_end              ; update decruncher end address
        ldy #>chipdisk_end
        stx $041d
        sty $041e

        jmp $0400                       ; jmp to decruncher
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_bitmap:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%10000000                  ; screen addr 0x2000, bitmap at $0000
        sta $d018

        lda #%00111011                  ; bitmap mode enabled
        sta $d011

        ldx #<irq_text
        ldy #>irq_text
        stx $fffe
        sty $ffff

        lda #50 + (8 * 14) + 2
        sta $d012

        inc sync_raster_irq

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla

nmi_irq:
        rti                             ; restores previous PC, status

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_text:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #%00001000                  ; no scroll, multi-color off, 40-cols
        sta $d016

        lda #%11101100                  ; screen addr 0x3800, charset at $3000
        sta $d018

        lda #%00011011                  ; bitmap mode disabled
        sta $d011

        ldx #<irq_rasterbar
        ldy #>irq_rasterbar
        stx $fffe
        sty $ffff

        lda #50 + (8 * 14) + 5
        sta $d012

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_rasterbar:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        STABILIZE_RASTER

        sei

        ldx #0
@l0:
        lda $d012                       ; 4
@l1:    cmp $d012                       ; 4
        beq @l1                         ; 2/3
        lda palette,x                   ; 6
        sta $d021                       ; 4
        inx                             ; 2
        cpx #TOTAL_PALETTE              ; 2
        bne @l0                         ; 2/3

        lda #0
        sta $d020
        sta $d021

        asl $d019                       ; clears raster interrupt

        lda #%00001000                  ; no scroll, multi-color off, 40-cols
        sta $d016

        lda #%11101100                  ; screen addr 0x3800, charset at $3000
        sta $d018

        lda #%00011011                  ; bitmap mode disabled
        sta $d011

        ldx #<irq_bitmap
        ldy #>irq_bitmap
        stx $fffe
        sty $ffff

        lda #20
        sta $d012


        inc delay_space_bar

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status

.endproc

effect_wip:             .byte 1         ; boolean. effect Work In Progress
sync_raster_irq:        .byte 0
delay_space_bar:        .byte 0
palette:
        .byte 6, 4, 14, 14,  3, 13, 1, 1

        .byte 0, 0, 0, 0,    0, 0, 0
        .byte 0, 0, 0, 0,    0

        .byte 1, 1
        .byte 1, 13, 3, 14, 14, 4, 6, 6

        .byte 0, 0, 0, 0,   0, 0, 0
        .byte 0, 0, 0, 0


        .byte 1, 1
        .byte 1, 7, 15, 10, 10, 8, 2, 2

        .byte 0, 0, 0, 0,   0, 0, 0
        .byte 0, 0, 0, 0, 0

        .byte 2, 2, 8, 10, 10, 15, 7, 1
        .byte 1, 1

TOTAL_PALETTE = * - palette

label_linyera_ok:
        .incbin "linyera-map.bin"

decruncher:
        .incbin "decrunch_chipdisk.prg", 2
TOTAL_DECRUNCH_SIZE = * - decruncher

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "BITMAP" $C000
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "BITMAP"
        .incbin "intro_half.bitmap"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "SCREENRAM" $E000
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "SCREENRAM"
        .incbin "intro_half.colormap"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "COLORRAM" $E400
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "COLORRAM"
bitmap_color:
        .incbin "intro_half.attrib"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "CHARSET" $F000
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CHARSET"
        .incbin "octavo-arlequin-pvmlogoc64_1m_remix-charset.bin"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "TEXT" $F800
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "TEXT"
        .incbin "octavo-arlequin-pvmlogoc64_1m_remix-map.bin"

