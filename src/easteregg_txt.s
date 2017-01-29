;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"
scroll_text:
        scrcode "Pungas de Villa Martelli brings you this time a compilation of argentine "
        scrcode "popular songs composed by argentine musicians. CoMu and Uctumi have chosen "
        scrcode "songs they love to offer us their re-interpretation, hoping that you will "
        scrcode "listen to them on your C64 and perhaps, make you wanna check out the original "
        scrcode "versions. Thanks to Riq's magical coding skills, this production is comprised "
        scrcode "of only one executable and it's compatible with every C64 model out there, including Drean. "
        scrcode "The dazzling graphics were carefully crafted by Alakran. What you're listening to right now "
        scrcode "is the 8-bit version of the argentine national anthem, which talks about freedom. Feel free "
        scrcode "to contact us and tell us how do you like this. See you in our next release!"

        scrcode "                                      "

        ; á: $80
        ; é: $81
        ; í: $82
        ; ó: $83
        ; ú: $84
        ; ñ: $85
        scrcode "Pungas de Villa Martelli quiso revisitar la m"
        .byte $84       ; ú
        scrcode "sica que te acompa"
        .byte $85       ; ñ
        .byte $83       ; ó
        scrcode " durante toda tu vida para que puedas escucharla en la que probablemente "
        scrcode "fue tu primera computadora. CoMu y Uctumi nos ofrecen su re-interpretaci"
        .byte $83       ; ó
        scrcode "n de los "
        scrcode "temas originales para esta producci"
        .byte $83       ; ó
        scrcode "n que gracias al m"
        .byte $80       ; á
        scrcode "gico c"
        .byte $83       ;ó
        scrcode "digo de Riq ocupa un "
        scrcode "solo ejecutable y es compatible con todas las versiones de commodore 64 que existen, "
        scrcode "incluida la Drean. Finalmente Alakr"
        .byte $80       ; á
        scrcode "n nos aporta sus gr"
        .byte $80       ; á
        scrcode "ficos renovados llenos de detalle, "
        scrcode "color, lealtad y patriotismo punga. Que lo disfrutes y nos vemos en la pr"
        .byte $83       ; ó
        scrcode "xima producci"
        .byte $83       ; ó
        scrcode "n! "

        scrcode "                                      "

        .byte $ff
