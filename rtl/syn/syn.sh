#!/bin/sh

set -e

SYN="$(dirname "$0")"
ROOT="${SYN}/.."
OUT="${SYN}/out"

rm -rf "${OUT}"

"${ROOT}/clash" -i"${ROOT}/src" \
                -outputdir "${OUT}" \
                --verilog \
                "${ROOT}/src/CFMTop.hs"

rm -rf "${OUT}"/verilog/CFMTop/*testbench*v
rm -rf "${OUT}"/verilog/CFMTop/*outputVerifier*v


yosys -p "read_verilog ${OUT}/verilog/CFMTop/*.v" \
      -p "synth_ice40 -top CFMTop_topEntity -abc2 -run begin:blif"

exit 0

arachne-pnr -d 8k -P ct256 -p "${SYN}/syn.pcf" "${OUT}/syn.blif" \
            -o "${OUT}/syn.asc"

icepack "${OUT}/syn.asc" "${OUT}/syn.bin"
icetime -md hx8k -c 25 "${OUT}/syn.asc"
