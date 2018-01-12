#!/bin/sh

set -e

cd "$(dirname "$0")"
SYN="."
ROOT=".."
OUT="out"
VERILOG="${OUT}/verilog/RTL/icestick_soc"

rm -rf "${OUT}"

"${ROOT}/clash" -i"${ROOT}/src" \
                -i"${ROOT}/../arch/src" \
                -outputdir "${OUT}" \
                --verilog \
                "${ROOT}/src/RTL/IcestickTop.hs"

rm -rf "${VERILOG}"/*testbench*v
rm -rf "${VERILOG}"/*outputVerifier*v

yosys -p "read_verilog ${VERILOG}/*.v ${SYN}/icestick-top.v" \
      -p "synth_ice40 -top top -abc2 -blif ${OUT}/syn1k.blif" \
      -q

SEED=1
echo "Running synthesis with seed ${SEED}"
arachne-pnr -d 1k -p "${SYN}/icestick.pcf" "${OUT}/syn1k.blif" \
            -o "${OUT}/syn1k.asc" -s ${SEED}
until icetime -md hx1k -c 40 "${OUT}/syn1k.asc"
do
  SEED=$((SEED + 1))
  echo "Retrying with seed ${SEED}"
  arachne-pnr -d 1k -p "${SYN}/icestick.pcf" "${OUT}/syn1k.blif" \
              -o "${OUT}/syn1k.asc" -s ${SEED}
done
