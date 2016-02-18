.SILENT:

.PHONY: all clean build

X64 = x64

all: chipdisk

SRC = src/chipdisk.s

chipdisk: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	exomizer sfx sys -x2 -o bin/$@_exo.prg bin/$@.prg
	$(X64) -moncommands bin/$@.sym bin/$@_exo.prg

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
