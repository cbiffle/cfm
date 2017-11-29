#!/bin/sh

set -e

cd "$(dirname "$0")"
SYN="."
ROOT=".."
OUT="out"
VERILOG="${OUT}/verilog/CFMTop/cfm_demo_top"

rm -rf "${OUT}"

"${ROOT}/clash" -i"${ROOT}/src" \
                -i"${ROOT}/../arch/src" \
                -outputdir "${OUT}" \
                --verilog \
                "${ROOT}/src/CFMTop.hs"

rm -rf "${VERILOG}"/*testbench*v
rm -rf "${VERILOG}"/*outputVerifier*v


yosys -p "read_verilog ${VERILOG}/*.v ${SYN}/icestick-top.v" \
      -p "synth_ice40 -top top -abc2 -blif ${OUT}/syn1k.blif" \
      -q

arachne-pnr -d 1k -p "${SYN}/icestick.pcf" "${OUT}/syn1k.blif" \
            -o "${OUT}/syn1k.asc"

icepack "${OUT}/syn1k.asc" "${OUT}/syn1k.bin"
icetime -md hx1k -c 40 "${OUT}/syn1k.asc"
