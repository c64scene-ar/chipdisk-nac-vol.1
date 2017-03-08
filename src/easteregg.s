;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ZP and other variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
ZP_SYNC_MUSIC           = $40           ; byte
ZP_SYNC_ANIM            = $41           ; byte
ZP_EYE_DELAY_LO         = $42           ; byte
ZP_EYE_DELAY_HI         = $43           ; byte
ZP_EYE_MODE             = $44           ; byte
ZP_VIC_VIDEO_TYPE       = $60           ; byte. values:
                                        ;   $01 --> PAL
                                        ;   $2F --> PAL-N
                                        ;   $28 --> NTSC
                                        ;   $2e --> NTSC-OLD
ZP_BIT_INDEX            = $61           ; byte  points to the bit displayed

SPRITE_ADDR     = $b000
SPRITE_PTR0     = <((SPRITE_ADDR .MOD $4000) / 64)     ; Sprite 0 at 128
CHARSET_ADDR    = $c000

SCROLL_TEXT     = $c800                 ; where the scroll is

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

.ifdef DEBUG
        sei

        lda #$35                        ; BASIC & KERNAL out
        sta $01

        lda #0
        sta $d020
        sta $d021

        lda #$01
        sta ZP_VIC_VIDEO_TYPE           ; set it to PAL in DEBUG mode
.endif

        lda #0
        sta ZP_SYNC_MUSIC
        sta ZP_SYNC_ANIM
        sta ZP_EYE_MODE
        sta ZP_EYE_DELAY_LO
        STA ZP_BIT_INDEX
        lda #1
        sta ZP_EYE_DELAY_HI

        lda #%00010101
        sta $dd00                       ; Vic bank 2: $8000-$bFFF

        lda #%00010100                  ; screen point to $0800
        sta $d018                       ; charset at $1800 (VIC)

        jsr init_screen
        jsr init_sprites
        jsr init_charset
        jsr init_irq
        jsr init_nmi


.ifndef DEBUG
        lda #0
        jsr $1000                       ; init sid
.endif

        cli

main_loop:
        lda ZP_SYNC_MUSIC
        bne play_music

test_anim:
        lda ZP_SYNC_ANIM
        beq main_loop

        dec ZP_SYNC_ANIM

;        dec $d020
        jsr animate_scroll              ; animation
        jsr animate_eye
;        inc $d020
        jmp main_loop

play_music:
        dec ZP_SYNC_MUSIC               ; music

.ifndef DEBUG
;        inc $d020
        jsr $1003
;        dec $d020
.endif
        jmp test_anim

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

        lda #%01111111
        sta $d017                       ; y double resolution
        sta $d01d                       ; x double resolution


        ldx #6
        ldy #12
l1:
        lda sprite_x_pos,x
        sta VIC_SPR0_X,y
        lda #252                        ; same Y for all sprites
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

sprite_pointers:
        .byte SPRITE_PTR0+0
        .byte SPRITE_PTR0+1
        .byte SPRITE_PTR0+2
        .byte SPRITE_PTR0+3
        .byte SPRITE_PTR0+4
        .byte SPRITE_PTR0+5
        .byte SPRITE_PTR0+6
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

        ldx #0
l1:     lda aeiou,x
        sta CHARSET_ADDR + $80 * 8,x
        inx
        cpx #(8*6)
        bne l1

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_irq
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_irq
                                        ; setup IRQ (play music)

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK CIA 1 interrupts
        lda $dd0d                       ; ACK CIA 2 interrupt

        lda #50                         ; play animation at 50
        sta $d012

        lda #$01                        ; enable
        sta $d01a                       ; raster IRQ

        lda #$7f
        sta $dc0d                       ; disables timer A and B IRQ

        lda #$0                         ; stop timer A
        sta $dc0e

        ldx #<irq_playmusic
        ldy #>irq_playmusic
        stx $fffe
        sty $ffff

.ifdef DEBUG
                                        ; will be set at the correct speed from chipdisk
        ldx #<$4cc7                     ; music speed
        ldy #>$4cc7
        stx $dc04                       ; low-cycle-count
        sty $dc05                       ; high-cycle-count
.endif


:       lda $d012                       ; play music at rasterline #200
:       cmp $d012
        beq :-
        cmp #200
        bne :--

        lda #$81
        sta $dc0d                       ; turn on CIA 1 interrups

        lda #%10010001                  ; and enable Timer A
        sta $dc0e

        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; init_nmi
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_nmi
                                        ; setup NMI (open borders)
        ldx #<nmi_openborder
        ldy #>nmi_openborder
        stx $fffa
        sty $fffb

        lda #$0                         ; stop timer A CIA 2
        sta $dd0e


                                        ; PAL,      (312 by 63) $4cc8 - 1
                                        ; PAL-N,    (312 by 65) $4f38 - 1
                                        ; NTSC,     (263 by 65) $42c7 - 1
                                        ; NTSC Old, (262 by 64) $4180 - 1

        ldx #<$4cc7                     ; default: PAL
        ldy #>$4cc7

        lda ZP_VIC_VIDEO_TYPE           ; $01 --> PAL
                                        ; $2F --> PAL-N
                                        ; $28 --> NTSC
                                        ; $2e --> NTSC-OLD
        cmp #$01
        beq @done

        cmp #$2f
        beq @paln

        cmp #$28
        beq @ntsc
        bne @ntsc_old

@paln:
        ldx #<$4f37
        ldy #>$4f37
        bne @done

@ntsc:
        ldx #<$42c6
        ldy #>$42c6
        bne @done

@ntsc_old:
        ldx #<$417f
        ldy #>$417f                     ; fall-through

@done:
        stx $dd04                       ; low-cycle-count
        sty $dd05                       ; high-cycle-count

        lda #%10000001                  ; enable interrupts in CIA 2
        sta $dd0d

:       lda $d012                       ; wait for raster at #f9
:       cmp $d012
        beq :-
        cmp #$f9
        bne :--

        lda #%10010001                  ; and enable Timer
        sta $dd0e

        rts
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ: irq_playmusic
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_playmusic
        pha                             ; saves A

        asl $d019                       ; clears raster interrupt
        bcs @raster

        lda $dc0d                       ; clears CIA1 timer A interrupt
        inc ZP_SYNC_MUSIC
        jmp @end

@raster:
        inc ZP_SYNC_ANIM

@end:
        pla                             ; restores A
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; nmi_openborder
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc nmi_openborder
        pha                             ; saves A

        lda $dd0d                       ; clears CIA1 timer A interrupt

;        dec $d020

        lda $d011                       ; open vertical borders trick
        and #%11110111                  ; first switch to 24 cols-mode...
        sta $d011

        lda #$fc
:       cmp $d012
        bne :-

        lda $d011                       ; ...a few raster lines switch to 25 cols-mode again
        ora #%00001000
        sta $d011

;        inc $d020

        pla                             ; restores A
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

        ldx #40*3                                       ; animation to open eye
@l0:
        lda easteregg_screen + 40 * 6,x
        sta $8400 + 40 * 6,x
        lda easteregg_color + 40 * 6,x
        sta $d800 + 40 * 6,x
        dex
        bpl @l0

        lda #EYE_MODE::EYE_SHOULD_BE_CLOSED
        sta ZP_EYE_MODE
        jmp @set_delays

@close_eyes:                                            ; aninamtion to close eye
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

        lda #7
        sta $d800 + 40 * 6 + 18,x
        sta $d800 + 40 * 7 + 18,x
        sta $d800 + 40 * 8 + 18,x

        dex
        bpl @l1

        lda #EYE_MODE::EYE_SHOULD_BE_OPEN
        sta ZP_EYE_MODE

@set_delays:
        ldx eyes_delays_idx
        inx
        cpx #EYES_DELAYS_TOTAL
        bne @l2
        ldx #0
@l2:
        stx eyes_delays_idx

        txa                                             ; multiply by 2
        asl
        tax
        lda eyes_delays_tbl,x
        sta ZP_EYE_DELAY_LO
        lda eyes_delays_tbl+1,x
        sta ZP_EYE_DELAY_HI
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
        lda SCROLL_TEXT                 ; self-modifying
        cmp #$ff
        bne next
        ldx #0
        stx ZP_BIT_INDEX
        ldx #<SCROLL_TEXT
        ldy #>SCROLL_TEXT
        stx load_scroll_addr
        sty load_scroll_addr+1
        lda SCROLL_TEXT

next:
        clc                             ; char_idx * 8
        asl
        rol $fa
        asl
        rol $fa
        asl
        rol $fa

        tay                             ; char_def = ($fc),y

        clc
        lda $fd
        adc $fa                         ; A = charset[char_idx * 8]
        sta $fd


        ; scroll top 8 bytes
        ; YY = sprite rows
        ; SS = sprite number
        .repeat 8, YY
                lda ($fc),y
                ldx ZP_BIT_INDEX             ; set C according to the current bit index
:               asl
                dex
                bpl :-

                .repeat 7, SS
                        rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 2
                        rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 1
                        rol SPRITE_ADDR + (6 - SS) * 64 + YY * 3 + 0
                .endrepeat
                iny                     ; byte of the char
        .endrepeat


        ldx ZP_BIT_INDEX
        inx
        cpx #8
        bne l1

        ldx #0
        clc
        inc load_scroll_addr
        bne l1
        inc load_scroll_addr+1
l1:
        stx ZP_BIT_INDEX

        rts

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

eyes_closed_screen:
;screen char data
; origin: (18, 6) = $0400 + 40 * 6 + 18
; size: (19, 3)
.byte  $a0,$a0,$a0,$a0,$fc,$ff,$a0,$a0,$e1,$e0,$61,$a0,$a0,$7f,$fe,$a0,$a0,$a0,$a0
.byte  $a0,$a0,$a0,$a0,$a0,$ec,$62,$a0,$dd,$e0,$dd,$a0,$62,$fb,$a0,$a0,$a0,$a0,$a0
.byte  $f8,$c3,$c6,$c6,$c3,$4e,$cb,$a0,$dd,$e0,$dd,$a0,$ca,$4d,$c3,$c6,$c6,$c3,$f8

eyes_delays_tbl:
        .addr $0201                     ; open:   10.0s
        .addr $0010                     ; closed:  0.2s
        .addr $0201                     ; open:   10.0s
        .addr $0010                     ; closed:  0.2s
        .addr $0201                     ; open:   10.0s
        .addr $0005                     ; closed:  0.2s
        .addr $0005                     ; open:    0.2s
        .addr $0005                     ; closed:  0.2s
        .addr $0101                     ; open:    5.0s
        .addr $0181                     ; closed:  5.0s
        .addr $0010                     ; open:    0.2s
        .addr $0101                     ; closed:  5.0s
EYES_DELAYS_TOTAL = (* - eyes_delays_tbl) / 2
eyes_delays_idx: .byte 0

aeiou:
        ; only read first 6 chars: aeiou6
        .incbin "aeiou_acentos-charset.bin", 0, 8 * 6

