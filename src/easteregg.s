;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ZP and other variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
ZP_SYNC_RASTER          = $40           ; byte
ZP_EYE_DELAY_LO         = $41           ; byte
ZP_EYE_DELAY_HI         = $42           ; byte
ZP_EYE_MODE             = $43           ; byte

SPRITE_ADDR     = $b000
SPRITE_PTR0     = <((SPRITE_ADDR .MOD $4000) / 64)     ; Sprite 0 at 128
CHARSET_ADDR    = $c000

;DEBUG = 1

.enum EYE_MODE
        EYE_SHOULD_BE_CLOSED
        EYE_SHOULD_BE_OPEN
.endenum

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; start
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export start
.proc start
        sei

        lda #$35
        sta $01

        lda #1
        sta ZP_SYNC_RASTER
        sta ZP_EYE_DELAY_LO
        sta ZP_EYE_MODE
        lda #2
        sta ZP_EYE_DELAY_HI

                                        ; turn VIC on
        lda #%00011011                  ; charset mode, default scroll-Y position, 25-rows
        sta $d011                       ; extended color mode: off

        lda #%00001000                  ; no scroll, hires (mono color), 40-cols
        sta $d016                       ; turn off multicolor

        jsr init_screen
        jsr init_sprites
        jsr init_charset

        lda #$01
        sta $d01a                       ; enable raster IRQ

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

        lda #$50
        sta $d012

        ldx #<irq_top
        ldy #>irq_top
        stx $fffe
        sty $ffff


.ifndef DEBUG
        lda #0
        tax
        tay
        jsr $1000                       ; init sid
.endif

        cli

main_loop:
        lda ZP_SYNC_RASTER
        beq main_loop

handle_raster:
        dec ZP_SYNC_RASTER

.ifndef DEBUG
        jsr $1003
.endif
        jsr animate_scroll
        jsr animate_eye
        jmp main_loop
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void init_screen()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_screen
        ldx #0
@l0:
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
        bne @l0
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void init_sprites()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_sprites
        lda #%01111111
        sta VIC_SPR_ENA

        lda #%01000000
        sta $d010                       ; 8-bit on for sprites x

        lda #0
        sta $d01c                       ; no sprite multi-color. hi-res only

        lda #%11111111
        sta $d017                       ; no y double resolution
        sta $d01d                       ; no x double resolution


        ldx #7
        ldy #14
l1:
        lda sprite_x_pos,x
        sta VIC_SPR0_X,y
        lda sprite_y_pos,x
        sta VIC_SPR0_Y,y
        lda #1                          ; white color
        sta VIC_SPR0_COLOR,x            ; all sprites are white
        lda sprite_pointers,x
        sta $87f8,x                     ; sprite pointers
        dey
        dey
        dex
        bpl l1

        lda #0                          ; all sprites are clean
        tax
l2:     sta SPRITE_ADDR,x               ; 8 sprites = 512 bytes = 64 * 8
        sta SPRITE_ADDR+$100,x
        dex
        bne l2

        rts

sprite_x_pos:
        .byte 48*0+10, 48*1+10, 48*2+10, 48*3+10
        .byte 48*4+10, 48*5+10, (48*6+10) .MOD 256
        .byte 160
sprite_y_pos:
        .byte 252,252,252,252,252,252,252
        .byte 32

sprite_pointers:
        .byte SPRITE_PTR0+0
        .byte SPRITE_PTR0+1
        .byte SPRITE_PTR0+2
        .byte SPRITE_PTR0+3
        .byte SPRITE_PTR0+4
        .byte SPRITE_PTR0+5
        .byte SPRITE_PTR0+6
        .byte SPRITE_PTR0+7
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void init_charset()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_charset
        lda #%00110001          ; make the CPU see the Character Generator ROM...
        sta $01                 ; ...at $D000 by storing %00110001 into location $01

        lda #$d8                ; load high byte of $D000
        sta $fc                 ; store it in a free location we use as vector
        lda #>CHARSET_ADDR
        sta $fe                 ; $FD/$FE = $7000

        ldy #$00                ; init counter with 0
        sty $fb                 ; store it as low byte in the $FB/$FC vector
        sty $fd                 ; $FD/$FE vector

        ldx #8

l0:     lda ($fb),y             ; read byte from vector stored in $fb/$fc
        sta ($fd),y             ; store it in $fd/$fe
        iny                     ; do this 255 times...
        bne l0                  ; ..for low byte $00 to $FF
        inc $fc                 ; when we passed $FF increase high byte...
        inc $fe
        dex                     ; ... and decrease X by one before restart
        bne l0                  ; We repeat this until X becomes Zero

        lda #%00110101          ; restore: RAM + IO
        sta $01

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ: irq_top()
;------------------------------------------------------------------------------;
; used to open the top/bottom borders
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_top
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda #$f8
        sta $d012

        ldx #<irq_bottom
        ldy #>irq_bottom
        stx $fffe
        sty $ffff

        inc ZP_SYNC_RASTER

        jmp exit_irq


irq_bottom:
        pha                             ; saves A, X, Y
        txa
        pha
        tya
        pha

        asl $d019                       ; clears raster interrupt

        lda $d011                       ; open vertical borders trick
        and #%11110111                  ; first switch to 24 cols-mode...
        sta $d011

:       lda $d012
        cmp #$ff
        bne :-

        lda $d011                       ; ...a few raster lines switch to 25 cols-mode again
        ora #%00001000
        sta $d011


        lda #50
        sta $d012
        ldx #<irq_top
        ldy #>irq_top
        stx $fffe
        sty $ffff

exit_irq:
        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_eye
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_eye
        dec ZP_EYE_DELAY_LO
        beq @hi
        rts

@hi:
        dec ZP_EYE_DELAY_HI
        bmi @do
        rts

@do:

        lda ZP_EYE_MODE
        cmp #EYE_MODE::EYE_SHOULD_BE_CLOSED
        beq @close_eyes

        ldx #40*3

@l0:
        lda easteregg_screen + 40 * 6,x
        sta $8400 + 40 * 6,x
        lda easteregg_color + 40 * 6,x
        sta $d800 + 40 * 6,x
        dex
        bpl @l0

        lda #1                                        ; keep them open for 10 seconds
        sta ZP_EYE_DELAY_LO
        lda #2
        sta ZP_EYE_DELAY_HI
        lda #EYE_MODE::EYE_SHOULD_BE_CLOSED
        sta ZP_EYE_MODE
        rts

@close_eyes:
;screen char data
; origin: (18, 6) = $0400 + 40 * 6 + 18
; size: (19, 3)
        ldx #18

@l1:
        lda eyes_closed_screen + 19 * 0,x
        sta $8400 + 40 * 6 + 18,x
        lda eyes_closed_screen + 19 * 1,x
        sta $8400 + 40 * 7 + 18,x
        lda eyes_closed_screen + 19 * 2,x
        sta $8400 + 40 * 8 + 18,x

        lda eyes_closed_color + 19 * 0,x
        sta $d800 + 40 * 6 + 18,x
        lda eyes_closed_color + 19 * 1,x
        sta $d800 + 40 * 7 + 18,x
        lda eyes_closed_color + 19 * 2,x
        sta $d800 + 40 * 8 + 18,x

        dex
        bpl @l1

        lda #10                                         ; keep them closed for 0.20s
        sta ZP_EYE_DELAY_LO
        lda #0
        sta ZP_EYE_DELAY_HI
        lda #EYE_MODE::EYE_SHOULD_BE_OPEN
        sta ZP_EYE_MODE
        rts

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; animate_scroll
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_scroll

        ; zero page variables f0-f9 are being used by the sid player (I guess)
        ; use fa-ff then
        lda #0
        sta $fa                         ; tmp variable

        ldx #<CHARSET_ADDR
        ldy #>CHARSET_ADDR
        stx $fc
        sty $fd                         ; pointer to charset

load_scroll_addr = * + 1
        lda scroll_text                 ; self-modifying
        cmp #$ff
        bne next
        ldx #0
        stx bit_idx
        ldx #<scroll_text
        ldy #>scroll_text
        stx load_scroll_addr
        sty load_scroll_addr+1
        lda scroll_text

next:
        clc                             ; char_idx * 8
        asl
        rol $fa
        asl
        rol $fa
        asl
        rol $fa

        tay                             ; char_def = ($fc),y
        sty $fb                         ; to be used in the bottom part of the char

        clc
        lda $fd
        adc $fa                         ; A = charset[char_idx * 8]
        sta $fd


        ; scroll top 8 bytes
        ; YY = sprite rows
        ; SS = sprite number
        .repeat 8, YY
                lda ($fc),y
                ldx bit_idx             ; set C according to the current bit index
:               asl
                dex
                bpl :-

        .repeat 8, SS
                rol SPRITE_ADDR + (7 - SS) * 64 + YY * 3 + 2
                rol SPRITE_ADDR + (7 - SS) * 64 + YY * 3 + 1
                rol SPRITE_ADDR + (7 - SS) * 64 + YY * 3 + 0
        .endrepeat
                iny                     ; byte of the char
        .endrepeat


        ldx bit_idx
        inx
        cpx #8
        bne l1

        ldx #0
        clc
        lda load_scroll_addr
        adc #1
        sta load_scroll_addr
        bcc l1
        inc load_scroll_addr+1
l1:
        stx bit_idx

        rts

bit_idx:
        .byte 0                         ; points to the bit displayed
.endproc

scroll_text:
        scrcode "...Oid mortales el grito sagrado, LIBERTAD, LIBERTAD, LIBERTAD. Bueno, aca va lo que tenga que ir."
        scrcode "Le quiero enviar un saludo a mi mama que me esta viendo, y a nadie mas. No hay espacio para usar caracteres custom"
        scrcode " asi que vamos a tener que usar los del sistema. Algo mas? Bueno, chau. Probando probando probando"
        scrcode " chau!....."
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        scrcode "aaaaaabbbbbcccccdddddeeeeefffffgggggghhhhiiiiijjjjjkkkkkllllllmmmmmmnnnnnopqrstuvwxz0123456789"
        .byte $ff

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

eyes_closed_screen:
;screen char data
; origin: (18, 6) = $0400 + 40 * 6 + 18
; size: (19, 3)
.byte  $fb,$a0,$a0,$a0,$fc,$ff,$a0,$a0,$e1,$e0,$61,$a0,$a0,$7f,$fe,$a0,$a0,$a0,$ec
.byte  $a0,$a0,$a0,$a0,$a0,$ec,$62,$a0,$dd,$e0,$dd,$a0,$62,$fb,$a0,$a0,$a0,$a0,$a0
.byte  $f8,$c3,$c6,$c6,$c3,$fe,$cb,$a0,$dd,$e0,$dd,$a0,$ca,$fc,$c3,$c6,$c6,$c3,$f8

eyes_closed_color:
;screen color data
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
.byte  $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
