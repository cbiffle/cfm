SHELL := /bin/bash

all: ico-all icestick-all

clean:
	-rm -rf build/clash/*
	-rm -f build/*

.PHONY: all clean

######################################
# Common

build/stack: stack.yaml $(wildcard */cfm-*.cabal) \
		$(shell find rtl arch tools -name "*.hs")
	stack --silent setup && stack build
	touch $@

build/%.fblocks: bf/%.fs build/stack
	stack --silent exec blocktool pack $< $@

build/bf-%.hex: build/bf-%.fs build/stack
	stack --silent exec bsforth $< $@

%.readmemb: %.hex
	ruby -e 'STDIN.each_line { |line| printf("%016b\n", line.to_i(16)) }' \
	  < $< \
	  > $@

%.hbin: %.hex
	cat $< | sed 's/^\(..\)\(..\)/\2\1/g' | xxd -r -p > $@

%.bin: %.asc
	icepack < $< > $@

######################################
# Icoboard

ICO_RTL_SOURCES=\
  arch/src/CFM/Inst.hs \
  arch/src/CFM/Types.hs \
  rtl/src/RTL/BootROM.hs \
  rtl/src/RTL/Common/Bits.hs \
  rtl/src/RTL/Common/Strobe.hs \
  rtl/src/RTL/Core.hs \
  rtl/src/RTL/CoreInterface.hs \
  rtl/src/RTL/GPIO.hs \
  rtl/src/RTL/IcoTop.hs \
  rtl/src/RTL/IOBus.hs \
  rtl/src/RTL/IRQ.hs \
  rtl/src/RTL/MMU.hs \
  rtl/src/RTL/Shifter.hs \
  rtl/src/RTL/SRAM.hs \
  rtl/src/RTL/Str.hs \
  rtl/src/RTL/Strobes.hs \
  rtl/src/RTL/Timer.hs \
  rtl/src/RTL/UART.hs \
  rtl/src/RTL/VGA/FrameGen.hs \
  rtl/src/RTL/VGA.hs \
  rtl/src/RTL/VGA/Palette.hs \
  rtl/src/RTL/VGA/Timing.hs

ICO_BF_SOURCES=\
  bf/bf0.fs \
  bf/bf1.fs \
  bf/ico0.fs \
  bf/mirq.fs \
  bf/uart.fs \
  bf/ico1.fs

build/ico_soc_clash.v: $(ICO_RTL_SOURCES) build/stack
	rtl/clash -irtl/src -iarch/src -outputdir build/clash \
	          -fclash-inline-limit=50 --verilog rtl/src/RTL/IcoTop.hs
	cat build/clash/verilog/RTL/ico_soc/*.v > $@

build/ico.blif: build/ico_soc_clash.v rtl/syn/ico-top.v
	cd rtl/syn && yosys -q \
	      -p "read_verilog $(patsubst %,../../%,$^)" \
	      -p "synth_ice40 -top top -abc2 -blif ../../$@"

build/ico.asc: build/ico.blif rtl/syn/icoboard.pcf
	arachne-pnr -d 8k -p rtl/syn/icoboard.pcf $< -o $@.tmp -s 1
	SEED=1; \
	until icetime -md hx8k -c 40 $@.tmp; do \
	  ((SEED = SEED + 1)); \
	  echo "Retrying with seed $${SEED}"; \
	  arachne-pnr -d 8k -p rtl/syn/icoboard.pcf $< -o $@.tmp -s $${SEED}; \
	done
	mv $@.tmp $@

build/bf-ico.fs: $(ICO_BF_SOURCES)
	cat $^ > $@

build/bf-ico.fblocks: $(patsubst bf/%.fs,build/%.fblocks,$(ICO_BF_SOURCES))
	cat $^ > $@

build/ico-boot.hex: bios/boot.fs build/stack
	stack --silent exec cfm-as $< $@

build/ico-prog.asc: build/ico.asc build/ico-boot.hex
	icebram -v rtl/syn/random-256.hex build/ico-boot.hex < $< > $@

program-ico-fpga: build/ico-prog.bin
	icoprog -p < $<

program-ico-fpga-flash: build/ico-prog.bin
	icoprog -f < $<

program-ico-boot: build/bf-ico.hbin
	icoprog -f -O 4 < $<

ico-all: build/ico-prog.bin build/bf-ico.hbin

.PHONY: program-ico program-ico-boot ico-all

######################################
# Icestick

ICE_RTL_SOURCES=\
  arch/src/CFM/Inst.hs \
  arch/src/CFM/Types.hs \
  rtl/src/RTL/Common/Bits.hs \
  rtl/src/RTL/Common/Strobe.hs \
  rtl/src/RTL/Core.hs \
  rtl/src/RTL/CoreInterface.hs \
  rtl/src/RTL/GPIO.hs \
  rtl/src/RTL/IcestickTop.hs \
  rtl/src/RTL/IOBus.hs \
  rtl/src/RTL/IRQ.hs \
  rtl/src/RTL/Shifter.hs \
  rtl/src/RTL/Str.hs \
  rtl/src/RTL/Strobes.hs \
  rtl/src/RTL/Timer.hs \
  rtl/src/RTL/UART.hs

ICE_BF_SOURCES=\
  bf/bf0.fs \
  bf/ice0.fs \
  bf/mirq.fs \
  bf/uart.fs \
  bf/ice1.fs

build/icestick_soc_clash.v: $(ICE_RTL_SOURCES) build/stack
	rtl/clash -irtl/src -iarch/src -outputdir build/clash \
	          -fclash-inline-limit=50 --verilog rtl/src/RTL/IcestickTop.hs
	cat build/clash/verilog/RTL/icestick_soc/*.v > $@

build/icestick.blif: build/icestick_soc_clash.v rtl/syn/icestick-top.v
	cd rtl/syn && yosys -q \
	      -p "read_verilog $(patsubst %,../../%,$^)" \
	      -p "synth_ice40 -top top -abc2 -blif ../../$@"

build/icestick.asc: build/icestick.blif rtl/syn/icestick.pcf
	arachne-pnr -d 1k -p rtl/syn/icestick.pcf $< -o $@.tmp -s 1
	SEED=1; \
	until icetime -md hx1k -c 40 $@.tmp; do \
	  ((SEED = SEED + 1)); \
	  echo "Retrying with seed $${SEED}"; \
	  arachne-pnr -d 1k -p rtl/syn/icestick.pcf $< -o $@.tmp -s $${SEED}; \
	done
	mv $@.tmp $@

build/bf-icestick.fs: $(ICE_BF_SOURCES)
	cat $^ > $@

build/bf-icestick.fblocks: $(patsubst bf/%.fs,build/%.fblocks,$(ICE_BF_SOURCES))
	cat $^ > $@

build/icestick-prog.asc: build/icestick.asc build/bf-icestick.hex
	icebram -v rtl/syn/random-3k5.hex build/bf-icestick.hex \
	  < $< \
	  > $@

program-icestick: build/icestick-prog.bin
	iceprog $<

icestick-all: build/icestick-prog.bin

.PHONY: program-icestick icestick-all
