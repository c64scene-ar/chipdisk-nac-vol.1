.SILENT:

.PHONY: all clean build

X64 = x64

all: chipdisk-dev

SRC = src/chipdisk.s

chipdisk-dev: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C chipdisk.cfg $^
	$(X64) -moncommands bin/$@.sym bin/$@.prg

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
