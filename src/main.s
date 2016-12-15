
.import ut_detect_pal_paln_ntsc
.import intro_main

.segment "CODE"

main:
        lda #$35                        ; no basic, no kernal
        sta $01

        jsr ut_detect_pal_paln_ntsc
        jsr intro_main                                  ; stack will be broken.
                                                        ; calls player_main directly
        jmp main
