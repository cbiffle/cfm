all: syn bits

clean:
	-rm -f build/*

syn: syn-ico syn-icestick
syn-ico: build/ico.asc
syn-icestick: build/icestick.asc

bits: bits-ico bits-icestick
bits-ico: build/ico-prog.bin
bits-icestick: build/icestick-prog.bin

program-icestick: build/icestick-prog.bin
	iceprog $<

program-ico: build/ico-prog.bin
	icoprog -p < $<

program-ico-boot: build/test.hbin
	icoprog -f -O 4 < $<

build/test.hex: bf/bf.fs
	mkdir -p build
	stack --silent setup && \
	  stack --silent build && \
	  stack --silent exec bsforth $< $@

%.readmemb: %.hex
	ruby -e 'STDIN.each_line { |line| printf("%016b\n", line.to_i(16)) }' \
	  < $< \
	  > $@

build/ico.asc:
	mkdir -p build
	stack --silent setup && \
	  stack --silent build --only-dependencies
	rtl/syn/syn-8k.sh
	cp rtl/syn/out/syn8k.asc build/ico.asc

build/icestick.asc:
	mkdir -p build
	stack --silent setup && \
	  stack --silent build --only-dependencies
	rtl/syn/syn-1k.sh
	cp rtl/syn/out/syn1k.asc build/icestick.asc

%-prog.asc: %.asc build/test.hex
	icebram -v rtl/syn/random-4k.hex build/test.hex \
	  < $< \
	  > $@

%.hbin: %.hex
	cat $< | sed 's/^\([0-9a-f][0-9a-f]\)\(..\)/\2 \1/g' | xxd -r -p > $@

%.bin: %.asc
	icepack < $< > $@

FORCE:

.PHONY: all clean syn syn-ico syn-icestick bits bits-ico bits-icestick
