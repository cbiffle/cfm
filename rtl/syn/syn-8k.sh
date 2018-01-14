#!/bin/sh

set -e

cd "$(dirname "$0")"
SYN="."
ROOT=".."
OUT="out"
VERILOG="${OUT}/verilog/RTL/ico_soc"

rm -rf "${OUT}"

"${ROOT}/clash" -i"${ROOT}/src" \
                -i"${ROOT}/../arch/src" \
                -outputdir "${OUT}" \
                -fclash-inline-limit=50 \
                --verilog \
                "${ROOT}/src/RTL/IcoTop.hs"

rm -rf "${VERILOG}"/*testbench*v
rm -rf "${VERILOG}"/*outputVerifier*v

yosys -p "read_verilog ${VERILOG}/*.v ${SYN}/ico-top.v" \
      -p "synth_ice40 -top top -abc2 -blif ${OUT}/syn8k.blif" \
      -q

arachne-pnr -d 8k -p "${SYN}/icoboard.pcf" "${OUT}/syn8k.blif" \
            -o "${OUT}/syn8k.asc" -s 2

icepack "${OUT}/syn8k.asc" "${OUT}/syn8k.bin"
icetime -md hx8k -c 40 "${OUT}/syn8k.asc"
