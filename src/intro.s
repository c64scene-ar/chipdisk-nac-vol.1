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
        bne @l1
        jmp post_sync_raster            ; effect finished, don't do it again
@l1:

        ldx #0                          ; do "change letter" effect
        stx effect_wip                  ; reset "effect_wip" if not done

@l0:
        lda $f800 + 22 * 40,x
        cmp label_linyera_ok,x          ; if letter already in place, skip

        beq @skip                       ; skip, if no needed

        inc $f800 + 22 * 40,x           ; inc letter
        inc effect_wip                  ; and set effect as not done. Work in Progress
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

        lda #15                         ; ram color. white. for PVM logo
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

        ldx #0                          ; decrunch table. clean it
@l2:    sta $0200,x
        inx
        bne @l2
        cli
        jmp do_decrunch
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

        lda #50 + (15*8)
        sta $d012

        inc sync_raster_irq

        pla                             ; restores A, X, Y
        tay
        pla
        tax
        pla
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

        lda #50 + (8 * 15) + 5
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

        ldx #0
@l0:
        lda $d012                       ; 4
@l1:    cmp $d012                       ; 4
        beq @l1                         ; 2/3
        lda palette,x                   ; 6
        sta $d021                       ; 4
        inx                             ; 2
        cpx #8 * 7                      ; 2
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

        lda #0
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
        .byte 6, 6
        .byte 6, 4, 14, 14,  3, 13, 1, 1

        .byte 0, 0, 0, 0,    0, 0, 0
        .byte 0, 0, 0, 0,    0

        .byte 1, 1
        .byte 1, 13, 3, 14, 14, 4, 6, 6

        .byte 0, 0, 0, 0,   0, 0, 0
        .byte 0, 0, 0, 0


        .byte 1, 1
        .byte 1, 7, 15, 10, 10, 8, 2, 2

        .byte 0, 0, 0, 0, 0, 0, 0

label_linyera_ok:
        .incbin "linyera-map.bin"

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
;segment "COLORRAM" $E200
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

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;segment "DECRUNCH"
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "DECRUNCH"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void do_decrunch()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc do_decrunch
        dec $01                         ; everything is RAM
        jsr decrunch
        inc $01                         ; using $d000 for VIC again
        jmp $0820                       ; jump to chipdisk
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; void get_crunched_byte()
;
; The decruncher jsr:s to the get_crunched_byte address when it wants to
; read a crunched byte. This subroutine has to preserve x and y register
; and must not modify the state of the carry flag.
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc get_crunched_byte
get_crunched_byte:
        lda _byte_lo
        bne _byte_skip_hi
        dec _byte_hi
_byte_skip_hi:
        inc $07e7
        dec $07e7

        dec _byte_lo
_byte_lo = * + 1
_byte_hi = * + 2
        lda chipdisk_end                        ; needs to be set correctly before
        rts			                            ; decrunch_file is called.
.endproc

; end_of_data needs to point to the address just after the address
; of the last byte of crunched data.
; -------------------------------------------------------------------
; if literal sequences is not used (the data was crunched with the -c
; flag) then the following line can be uncommented for shorter code.
;LITERAL_SEQUENCES_NOT_USED = 1
; -------------------------------------------------------------------
; zero page addresses used
; -------------------------------------------------------------------
zp_len_lo = $a7

zp_src_lo  = $ae
zp_src_hi  = zp_src_lo + 1

zp_bits_hi = $fc

zp_bitbuf  = $fd
zp_dest_lo = zp_bitbuf + 1	; dest addr lo
zp_dest_hi = zp_bitbuf + 2	; dest addr hi

decrunch_table = $0200
tabl_bi = decrunch_table
tabl_lo = decrunch_table + 52
tabl_hi = decrunch_table + 104

; -------------------------------------------------------------------
; no code below this comment has to be modified in order to generate
; a working decruncher of this source file.
; However, you may want to relocate the tables last in the file to a
; more suitable address.
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; jsr this label to decrunch, it will in turn init the tables and
; call the decruncher
; no constraints on register content, however the
; decimal flag has to be #0 (it almost always is, otherwise do a cld)
decrunch:
; -------------------------------------------------------------------
; init zeropage, x and y regs. (12 bytes)
;
	ldy #0
	ldx #3
init_zp:
	jsr get_crunched_byte
	sta zp_bitbuf - 1,x
	dex
	bne init_zp
; -------------------------------------------------------------------
; calculate tables (50 bytes)
; x and y must be #0 when entering
;
nextone:
	inx
	tya
	and #$0f
	beq shortcut		; starta på ny sekvens

	txa			; this clears reg a
	lsr a			; and sets the carry flag
	ldx tabl_bi-1,y
rolle:
	rol a
	rol zp_bits_hi
	dex
	bpl rolle		; c = 0 after this (rol zp_bits_hi)

	adc tabl_lo-1,y
	tax

	lda zp_bits_hi
	adc tabl_hi-1,y
shortcut:
	sta tabl_hi,y
	txa
	sta tabl_lo,y

	ldx #4
	jsr get_bits		; clears x-reg.
	sta tabl_bi,y
	iny
	cpy #52
	bne nextone
	ldy #0
	beq begin
; -------------------------------------------------------------------
; get bits (29 bytes)
;
; args:
;   x = number of bits to get
; returns:
;   a = #bits_lo
;   x = #0
;   c = 0
;   z = 1
;   zp_bits_hi = #bits_hi
; notes:
;   y is untouched
; -------------------------------------------------------------------
get_bits:
	lda #$00
	sta zp_bits_hi
	cpx #$01
	bcc bits_done
bits_next:
	lsr zp_bitbuf
	bne ok
	pha
literal_get_byte:
	jsr get_crunched_byte
	bcc literal_byte_gotten
	ror a
	sta zp_bitbuf
	pla
ok:
	rol a
	rol zp_bits_hi
	dex
	bne bits_next
bits_done:
	rts
; -------------------------------------------------------------------
; main copy loop (18(16) bytes)
;
copy_next_hi:
	dex
	dec zp_dest_hi
	dec zp_src_hi
copy_next:
	dey
.IFNDEF LITERAL_SEQUENCES_NOT_USED
	bcc literal_get_byte
.ENDIF
	lda (zp_src_lo),y
literal_byte_gotten:
	sta (zp_dest_lo),y
copy_start:
	tya
	bne copy_next
begin:
	txa
	bne copy_next_hi
; -------------------------------------------------------------------
; decruncher entry point, needs calculated tables (21(13) bytes)
; x and y must be #0 when entering
;
.IFNDEF LITERAL_SEQUENCES_NOT_USED
	inx
	jsr get_bits
	tay
	bne literal_start1
.ELSE
	dey
.ENDIF
begin2:
	inx
	jsr bits_next
	lsr a
	iny
	bcc begin2
.IFDEF LITERAL_SEQUENCES_NOT_USED
	beq literal_start
.ENDIF
	cpy #$11
.IFNDEF LITERAL_SEQUENCES_NOT_USED
	bcc sequence_start
	beq bits_done
; -------------------------------------------------------------------
; literal sequence handling (13(2) bytes)
;
	ldx #$10
	jsr get_bits
literal_start1:
	sta <zp_len_lo
	ldx <zp_bits_hi
	ldy #0
	bcc literal_start
sequence_start:
.ELSE
	bcs bits_done
.ENDIF
; -------------------------------------------------------------------
; calulate length of sequence (zp_len) (11 bytes)
;
	ldx tabl_bi - 1,y
	jsr get_bits
	adc tabl_lo - 1,y	; we have now calculated zp_len_lo
	sta zp_len_lo
; -------------------------------------------------------------------
; now do the hibyte of the sequence length calculation (6 bytes)
	lda zp_bits_hi
	adc tabl_hi - 1,y	; c = 0 after this.
	pha
; -------------------------------------------------------------------
; here we decide what offset table to use (20 bytes)
; x is 0 here
;
	bne nots123
	ldy zp_len_lo
	cpy #$04
	bcc size123
nots123:
	ldy #$03
size123:
	ldx tabl_bit - 1,y
	jsr get_bits
	adc tabl_off - 1,y	; c = 0 after this.
	tay			; 1 <= y <= 52 here
; -------------------------------------------------------------------
; Here we do the dest_lo -= len_lo subtraction to prepare zp_dest
; but we do it backwards:	a - b == (b - a - 1) ^ ~0 (C-syntax)
; (16(16) bytes)
	lda zp_len_lo
literal_start:			; literal enters here with y = 0, c = 1
	sbc zp_dest_lo
	bcc noborrow
	dec zp_dest_hi
noborrow:
	eor #$ff
	sta zp_dest_lo
	cpy #$01		; y < 1 then literal
.IFNDEF LITERAL_SEQUENCES_NOT_USED
	bcc pre_copy
.ELSE
	bcc literal_get_byte
.ENDIF
; -------------------------------------------------------------------
; calulate absolute offset (zp_src) (27 bytes)
;
	ldx tabl_bi,y
	jsr get_bits;
	adc tabl_lo,y
	bcc skipcarry
	inc zp_bits_hi
	clc
skipcarry:
	adc zp_dest_lo
	sta zp_src_lo
	lda zp_bits_hi
	adc tabl_hi,y
	adc zp_dest_hi
	sta zp_src_hi
; -------------------------------------------------------------------
; prepare for copy loop (8(6) bytes)
;
	pla
	tax
.IFNDEF LITERAL_SEQUENCES_NOT_USED
	sec
pre_copy:
	ldy <zp_len_lo
	jmp copy_start
.ELSE
	ldy <zp_len_lo
	bcc copy_start
.ENDIF
; -------------------------------------------------------------------
; two small static tables (6(6) bytes)
;
tabl_bit:
	.byte 2,4,4
tabl_off:
	.byte 48,32,16
; -------------------------------------------------------------------
; end of decruncher
; -------------------------------------------------------------------
;
