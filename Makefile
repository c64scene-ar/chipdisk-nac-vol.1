.SILENT:

.PHONY: all clean build

X64 = x64

all: chipdisk-dev

SRC = src/chipdisk.s

chipdisk-dev: ${SRC}
	cl65 -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	$(X64) -moncommands $@.sym bin/$@.prg

clean:
	rm -f src/*.o *.sym bin/*.prg
