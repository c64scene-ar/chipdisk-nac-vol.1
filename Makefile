.SILENT:

.PHONY: all clean buttons

X64 = x64

all: easteregg_txt easteregg chipdisk decrunch_chipdisk intro

SRC = src/main.s src/chipdisk.s src/exodecrunch.s src/utils.s

chipdisk: ${SRC}
	echo
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	echo Building Player
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -t c64 -C chipdisk.cfg $^
	exomizer mem -o bin/$@-exo.prg bin/$@.prg
	cp bin/$@-exo.prg src/
	#cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	#exomizer sfx sys -x1 -Di_line_number=1996 -o bin/$@-exo.prg bin/$@.prg

testchipdisk: chipdisk
	$(X64) -moncommands bin/chipdisk.sym bin/chipdisk.prg

easteregg: src/easteregg.s
	echo
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	echo Building EasterEgg
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -t c64 -C easteregg.cfg $^
	exomizer mem -o bin/$@-exo.prg bin/$@.prg
	cp bin/$@-exo.prg src/

easteregg_txt: src/easteregg_txt.s
	echo
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	echo Building EasterEgg Scroll Text
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -t c64 -C easteregg_txt.cfg $^
	exomizer mem -o bin/$@-exo.prg bin/$@.prg
	cp bin/$@-exo.prg src/

testeaster: easteregg
	$(X64) -moncommands bin/easteregg.sym bin/easteregg.prg

decrunch_chipdisk: src/decrunch_chipdisk.s
	echo
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	echo Building Decruncher for chipdisk
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -t c64 -C decrunch_chipdisk.cfg $^
	cp bin/$@.prg src/

intro: src/intro.s
	echo
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	echo Building Intro
	echo =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	-cp res/octavo-arlequin-pvmlogoc64_1m_remix-map.bin src/
	-cp res/octavo-arlequin-pvmlogoc64_1m_remix-charset.bin src/
	-cp res/linyera-map.bin src/
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C intro.cfg $^
	exomizer sfx sys -x1 -Di_line_number=1996 -o bin/$@-exo.prg bin/$@.prg
	$(X64) -moncommands bin/$@.sym bin/$@-exo.prg

buttons:
	src/extract_image.py 7 7 <res/buttons/play.prg >src/button_play.raw
	src/extract_image.py 7 7 <res/buttons/rew.prg  >src/button_rew.raw
	src/extract_image.py 7 7 <res/buttons/ff.prg   >src/button_ff.raw
	src/extract_image.py 7 7 <res/buttons/stop.prg >src/button_stop.raw

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
