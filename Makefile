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

program-ico-boot: build/bf-ico.hbin
	icoprog -f -O 4 < $<

build/bf-ice.fs: bf/bf0.fs bf/ice0.fs bf/mirq.fs bf/ice1.fs
	mkdir -p build
	cat $^ > $@

build/bf-ico.fs: bf/bf0.fs bf/bf1.fs \
                 bf/ico0.fs bf/mirq.fs bf/ico1.fs
	mkdir -p build
	cat $^ > $@

build/bf-%.hex: build/bf-%.fs
	mkdir -p build
	stack --silent setup && \
	  stack --silent build && \
	  stack --silent exec bsforth $< $@

build/boot.hex: bios/boot.fs
	mkdir -p build
	stack --silent setup && \
	  stack --silent build && \
	  stack --silent exec cfm-as $< $@

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

build/ico-prog.asc: build/ico.asc build/boot.hex
	icebram -v rtl/syn/random-256.hex build/boot.hex \
	  < $< \
	  > $@

build/icestick-prog.asc: build/icestick.asc build/bf-ice.hex
	icebram -v rtl/syn/random-3k5.hex build/bf-ice.hex \
	  < $< \
	  > $@

%.hbin: %.hex
	cat $< | sed 's/^\(..\)\(..\)/\2\1/g' | xxd -r -p > $@

%.bin: %.asc
	icepack < $< > $@

FORCE:

.PHONY: all clean syn syn-ico syn-icestick bits bits-ico bits-icestick
