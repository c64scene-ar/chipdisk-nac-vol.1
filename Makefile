.SILENT:

.PHONY: all clean build

X64 = x64

all: chipdisk

SRC = src/chipdisk.s

chipdisk: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	exomizer sfx sys -x1 -Di_line_number=1996 -o bin/$@-dev.prg bin/$@.prg
	$(X64) -moncommands bin/$@.sym bin/$@-dev.prg

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
