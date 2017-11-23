#!/bin/sh

set -e

cd "$(dirname "$0")"
SYN="."
ROOT=".."
OUT="out"

rm -rf "${OUT}"

"${ROOT}/clash" -i"${ROOT}/src" \
                -outputdir "${OUT}" \
                --verilog \
                "${ROOT}/src/CFMTop.hs"

rm -rf "${OUT}"/verilog/CFMTop/*testbench*v
rm -rf "${OUT}"/verilog/CFMTop/*outputVerifier*v


yosys -p "read_verilog ${OUT}/verilog/CFMTop/*.v" \
      -p "synth_ice40 -top CFMTop_topEntity -abc2 -blif ${OUT}/syn1k.blif"

arachne-pnr -d 1k -p "${SYN}/icestick.pcf" "${OUT}/syn1k.blif" \
            -o "${OUT}/syn1k.asc"

icepack "${OUT}/syn1k.asc" "${OUT}/syn1k.bin"
icetime -md hx1k -c 12 "${OUT}/syn1k.asc"
