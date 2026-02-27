#!/usr/bin/env bash
set -euo pipefail

# ------------- Config (override via env) -------------
LGMS="${LGMS:-8 10 12}"                       # LGM sweep
RESULTS_CSV="${RESULTS_CSV:-tlmon_results.csv}"
CACHE_CSV="${CACHE_CSV:-tlmon_minstret_cache.csv}"

# Default targets (override by listing //targets... on CLI)
TARGETS_DEFAULT=(
  "//sw/device/tests:aes_smoketest_sim_verilator"
  "//sw/device/tests:aon_timer_smoketest_sim_verilator"
  "//sw/device/tests:clkmgr_smoketest_sim_verilator"
  "//sw/device/tests:csrng_smoketest_sim_verilator"
  "//sw/device/tests:entropy_src_smoketest_sim_verilator"
  "//sw/device/tests:flash_scrambling_smoketest_sim_verilator"
  "//sw/device/tests:gpio_smoketest_sim_verilator"
  "//sw/device/tests:hmac_smoketest_sim_verilator"
  "//sw/device/tests:kmac_smoketest_sim_verilator"
  "//sw/device/tests:mbx_smoketest_sim_verilator"
  "//sw/device/tests:otbn_smoketest_sim_verilator"
  "//sw/device/tests:otp_ctrl_smoketest_sim_verilator"
  "//sw/device/tests:pwm_smoketest_sim_verilator"
  "//sw/device/tests:pwrmgr_smoketest_sim_verilator"
  "//sw/device/tests:pwrmgr_usbdev_smoketest_sim_verilator"
  "//sw/device/tests:rstmgr_smoketest_sim_verilator"
  "//sw/device/tests:rv_plic_smoketest_sim_verilator"
  "//sw/device/tests:rv_timer_smoketest_sim_verilator"
  "//sw/device/tests:soc_proxy_smoketest_sim_verilator"
  "//sw/device/tests:spi_device_flash_smoketest_sim_verilator"
  "//sw/device/tests:spi_host_smoketest_sim_verilator"
  "//sw/device/tests:sram_ctrl_smoketest_sim_verilator"
  "//sw/device/tests:uart_smoketest_sim_verilator"
)

# Extra opentitantool / verilator args common to all runs
COMMON_ARGS=(
  "--test_output=streamed"
  "--disk_cache=${HOME}/bazel_cache"
  "--test_timeout=7200"
)
VERILATOR_ARGS=(
  "--verilator-args=--trace=${HOME}/opentitan/waves/ot_sim.fst"
  "--verilator-args=--timeout=eternal"
  "--verilator-args=+define+RVFI"
)

# ------------- Helpers -------------
ts() { date +"%Y-%m-%d %H:%M:%S"; }

# Build the full arg list for a test, adding per-run +args at the end.
build_test_args() {
  # $1=nameref array to fill, ${@:2}=extra +args
  local -n _out="$1"; shift
  _out=("${COMMON_ARGS[@]}")
  for va in "${VERILATOR_ARGS[@]}"; do _out+=("--test_arg=${va}"); done
  # Always capture stdout CSV
  _out+=("--test_arg=--exec=console")
  # Add any `+TLMON_...` or +define passed for this run:
  for extra in "$@"; do _out+=("--test_arg=--verilator-args=${extra}"); done
}

# Run bazel test and tee raw output to a log; print the log path.
bazel_run_logged() {
  local target="$1"; shift
  local log="logs/$(basename "$target")_$(date +%Y%m%d_%H%M%S).log"
  mkdir -p logs
  echo "[$(ts)] bazel test $target"
  # shellcheck disable=SC2068
  if bazel test "$target" $@ 2>&1 | tee "$log" ; then
    :
  else
    echo "[$(ts)] WARN: bazel returned non-zero for $target (kept $log)" >&2
  fi
  echo "$log"
}

# Extract a key=value from the TLMON_CSV line in a log
# usage: tlmon_get "$log" Bbits   -> echoes value or "" if not found
tlmon_get() {
  local log="$1" key="$2"
  local line
  line="$(grep -F "TLMON_CSV" "$log" || true)"
  [[ -z "$line" ]] && { echo ""; return 0; }
  # change commas to newlines then grep exact key=
  echo "$line" | tr ',' '\n' | grep -E "^${key}=" | head -n1 | cut -d= -f2
}

append_results_row() {
  local target="$1" lgm="$2" log="$3"
  local minstret A_total Bbits aliasB if_rom_bpi if_flash_bpi if_tot_bpi sram_deadB
  minstret="$(tlmon_get "$log" minstret)"
  A_total="$(tlmon_get "$log" A_total)"
  [[ -z "$A_total" ]] && A_total="$(tlmon_get "$log" A)"   # backward compat
  Bbits="$(tlmon_get "$log" Bbits)"
  aliasB="$(tlmon_get "$log" aliasB)"
  if_rom_bpi="$(tlmon_get "$log" ifetch_rom_bpi)"
  if_flash_bpi="$(tlmon_get "$log" ifetch_flash_bpi)"
  if_tot_bpi="$(tlmon_get "$log" ifetch_tot_bpi)"
  sram_deadB="$(tlmon_get "$log" sram_deadB)"

  [[ -z "$minstret" ]] && return 1

  if [[ ! -s "$RESULTS_CSV" ]]; then
    echo "target,lgm,minstret,A_total,Bbits,aliasB,ifetch_rom_bpi,ifetch_flash_bpi,ifetch_tot_bpi,sram_deadB,log" > "$RESULTS_CSV"
  fi
  printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n" \
    "$target" "$lgm" "$minstret" "$A_total" "$Bbits" "$aliasB" \
    "$if_rom_bpi" "$if_flash_bpi" "$if_tot_bpi" "$sram_deadB" "$log" \
    >> "$RESULTS_CSV"
  return 0
}

# Minstret cache: read & write
cache_read_minstret() {
  local target="$1"
  [[ -f "$CACHE_CSV" ]] || { echo ""; return 0; }
  awk -F, -v t="$target" '$1==t {print $2}' "$CACHE_CSV" | tail -n1
}
cache_write_minstret() {
  local target="$1" minstret="$2"
  if [[ ! -s "$CACHE_CSV" ]]; then
    echo "target,minstret" > "$CACHE_CSV"
  fi
  echo "$target,$minstret" >> "$CACHE_CSV"
}

# ------------- Main -------------
mkdir -p logs

# Targets come from CLI (if any) else defaults:
TARGETS=("$@")
if [[ ${#TARGETS[@]} -eq 0 ]]; then
  TARGETS=("${TARGETS_DEFAULT[@]}")
fi

echo "Targets:"
for t in "${TARGETS[@]}"; do echo "  - $t"; done
echo "LGM sweep: $LGMS"
echo

for target in "${TARGETS[@]}"; do
  echo "========== $target =========="

  # 1) Discover minstret (use cache if present)
  minstret="$(cache_read_minstret "$target")"
  if [[ -z "$minstret" ]]; then
    echo "  → Discovering minstret (baseline run)"
    declare -a BASE_ARGS=()
    build_test_args BASE_ARGS   "+MINSTRET_CHECK=1" # no STOP_AFTER; full PASS run
    base_log="$(bazel_run_logged "$target" "${BASE_ARGS[@]}")"
    minstret="$(tlmon_get "$base_log" minstret)"
    if [[ -z "$minstret" ]]; then
      echo "  !! Could not parse minstret from $base_log; skipping target" >&2
      continue
    fi
    cache_write_minstret "$target" "$minstret"
    echo "  ✓ minstret=$minstret  (cached)"
  else
    echo "  → Using cached minstret=$minstret"
  fi

  # 2) Sweep LGM
  for lgm in $LGMS; do
    echo "  → Sweep LGM=$lgm"
    declare -a SWEEP_ARGS=()
    build_test_args SWEEP_ARGS "+TLMON_LGM=${lgm}" "+TLMON_STOP_AFTER=${minstret}"
    sweep_log="$(bazel_run_logged "$target" "${SWEEP_ARGS[@]}")"
    if append_results_row "$target" "$lgm" "$sweep_log"; then
      echo "    ✓ appended to $RESULTS_CSV"
    else
      echo "    !! TLMON_CSV missing in $sweep_log" >&2
    fi
  done
done

echo
echo "Done. Results → $RESULTS_CSV"
echo "Minstret cache → $CACHE_CSV"
