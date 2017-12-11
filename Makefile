all: build/icestick-3k-prog.bin

clean:
	-rm -f build/*

syn: build/icestick-3k.asc

program: build/icestick-3k-prog.bin
	iceprog $<

build/test.hex: tools/test.fs
	mkdir -p build
	stack --silent setup && \
	  stack --silent build && \
	  stack --silent exec cfm-as tools/test.fs build/test.hex

%.readmemb: %.hex
	ruby -e 'STDIN.each_line { |line| printf("%016b\n", line.to_i(16)) }' \
	  < $< \
	  > $@

build/icestick-3k.asc:
	mkdir -p build
	stack --silent setup && \
	  stack --silent build --only-dependencies
	rtl/syn/syn-1k.sh
	cp rtl/syn/out/syn1k.asc build/icestick-3k.asc

build/icestick-3k-prog.asc: build/icestick-3k.asc build/test.hex
	icebram -v rtl/syn/random-3k.hex build/test.hex \
	  < build/icestick-3k.asc \
	  > build/icestick-3k-prog.asc

%.bin: %.asc
	icepack < $< > $@

FORCE:

.PHONY: all clean syn
