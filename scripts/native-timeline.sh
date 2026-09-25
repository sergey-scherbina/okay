#!/bin/sh
# native-timeline.sh [out.tsv] [seconds]
#
# The TIMELINE the native-runner-error entry asked for and nobody took
# (BACKLOG-ARCHIVE, "THE CAUSE": "record the runner processes beside the
# gate once a second, with the clock"). Samples every Scala Native test
# binary on the box — any worktree, any sibling's gate — once a second:
#
#   epoch_ms  pid  stat  cputime  etime  rss_kb  binary
#
# What it is for: shape C of a lost test process (gate.sh) is the
# adapter killing a binary that did not connect back within ComRunner's
# hard-coded 40 s. A killed binary shows here as a pid that lived ~40 s
# and vanished; its `stat` and `cputime` over those 40 s say WHY it did
# not connect — runnable with almost no CPU is starvation, sleeping is a
# block, busy is a slow start. `--report out.tsv` summarises a run.
#
# It only reads `ps`; it starts, signals and waits on nothing.
set -u
if [ "${1:-}" = "--report" ]; then
  f="${2:?--report needs a tsv}"
  # per pid: first and last sample, samples seen, the states it was in,
  # and its cpu time at the end; a lifetime of 35 s or more is flagged
  awk -F'\t' '
    { pid=$2; if (!(pid in first)) { first[pid]=$1; bin[pid]=$7 } last[pid]=$1; n[pid]++
      st[pid]=st[pid] substr($3,1,1); cpu[pid]=$4; rss[pid]=$6 }
    END {
      for (p in first) {
        life=(last[p]-first[p])/1000.0
        # the states seen, collapsed to the set
        s=st[p]; set=""; for (i=1;i<=length(s);i++){c=substr(s,i,1); if (index(set,c)==0) set=set c}
        printf "%s\t%.0f\t%s\t%s\t%s\t%s%s\n", p, life, set, cpu[p], rss[p], bin[p], (life>=35 ? "\tLONG" : "")
      }
    }' "$f" | sort -t'	' -k2,2nr
  exit 0
fi
out="${1:-native-timeline.tsv}"
secs="${2:-3600}"
end=$(( $(date +%s) + secs ))
while [ "$(date +%s)" -lt "$end" ]; do
  now=$(perl -MTime::HiRes=time -e 'printf "%d", time*1000')
  ps -axo pid=,stat=,time=,etime=,rss=,command= 2>/dev/null \
    | awk -v now="$now" '$6 ~ /\/\.native\/target\/.*-test$/ {
        n=split($6, parts, "/"); printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n", now, $1, $2, $3, $4, $5, parts[n] }' >> "$out"
  sleep 1
done
