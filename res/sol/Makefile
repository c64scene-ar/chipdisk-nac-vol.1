.SILENT:

.PHONY: all clean buttons

X64 = x64

all:
	acme sol_screen.asm
	acme sol_color.asm
	exomizer mem sol_screen.prg -q -o sol_screen.exo
	exomizer mem sol_color.prg -q -o sol_color.exo


clean:
	rm -f *.exo *.prg
