.SILENT:

.PHONY: all clean buttons

X64 = x64

all: chipdisk

SRC = src/chipdisk.s src/exodecrunch.s src/easteregg.s

chipdisk: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	exomizer sfx sys -x1 -Di_line_number=1996 -o bin/$@-dev.prg bin/$@.prg
	$(X64) -moncommands bin/$@.sym bin/$@-dev.prg

buttons:
	src/extract_image.py 7 7 <res/buttons/play.prg >src/button_play.raw
	src/extract_image.py 7 7 <res/buttons/rew.prg  >src/button_rew.raw
	src/extract_image.py 7 7 <res/buttons/ff.prg   >src/button_ff.raw
	src/extract_image.py 7 7 <res/buttons/stop.prg >src/button_stop.raw

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
