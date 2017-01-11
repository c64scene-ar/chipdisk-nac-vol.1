;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; The Uni Games: https://github.com/ricardoquesada/c64-the-uni-games
;
; Collection of utils functions
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ZP and other variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
ZP_VIC_VIDEO_TYPE       = $60           ; byte. values:
                                        ;   $01 --> PAL
                                        ;   $2F --> PAL-N
                                        ;   $28 --> NTSC
                                        ;   $2e --> NTSC-OLD

.segment "MORECODE2"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; char ut_detect_pal_paln_ntsc(void)
;------------------------------------------------------------------------------;
; It counts how many rasterlines were drawn in 312*63 (19656) cycles.
; 312*63-1 is passed to the timer since it requires one less.
;
; In PAL,      (312 by 63)  19656/63 = 312  -> 312 % 312   (00, $00)
; In PAL-N,    (312 by 65)  19656/65 = 302  -> 302 % 312   (46, $2e)
; In NTSC,     (263 by 65)  19656/65 = 302  -> 302 % 263   (39, $27)
; In NTSC Old, (262 by 64)  19656/64 = 307  -> 307 % 262   (45, $2d)
;
; Return values:
;   $01 --> PAL
;   $2F --> PAL-N
;   $28 --> NTSC
;   $2e --> NTSC-OLD
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

.export ut_detect_pal_paln_ntsc
.proc ut_detect_pal_paln_ntsc
        sei                             ; disable interrupts

        lda #0
        sta $d011                       ; turn off display to disable badlines

:       lda $d012                       ; wait for start of raster (more stable results)
:       cmp $d012
        beq :-
        bmi :--

        lda #$00
        sta $dc0e                       ; stop timer A, in case it is running

        lda #$00
        sta $d01a                       ; disables raster IRQ
        lda #$7f
        sta $dc0d                       ; disables timer A and B IRQ
        sta $dd0d


        lda #$00
        sta sync

        ldx #<(312*63-1)                ; set the timer for PAL
        ldy #>(312*63-1)
        stx $dc04
        sty $dc05

        lda #%00001001                  ; one-shot only
        sta $dc0e

        ldx #<timer_irq
        ldy #>timer_irq
        stx $fffe
        sty $ffff

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK possible timer A and B interrupts
        lda $dd0d

        lda #$81
        sta $dc0d                       ; enable timer A interrupts
        cli

:       lda sync
        beq :-

        lda #$1b                        ; enable the display again
        sta $d011
        lda ZP_VIC_VIDEO_TYPE           ; load ret value
        rts

timer_irq:
        pha                             ; only saves A

        lda $dc0d                       ; clear timer A interrupt

        lda $d012
        sta ZP_VIC_VIDEO_TYPE

        inc sync
        cli

        pla                             ; restores A
        rti

sync:  .byte $00

.endproc

