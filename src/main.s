
.import ut_detect_pal_paln_ntsc
.import intro_main
.import player_main

.segment "CODE"

main:
        lda #$35                        ; no basic, no kernal
        sta $01

        jsr ut_detect_pal_paln_ntsc
        jmp player_main
