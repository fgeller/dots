#!/bin/sh
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
dir=$(basename "$cwd")
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
dur_ms=$(echo "$input" | jq -r '.cost.total_duration_ms // empty')
agent=$(echo "$input" | jq -r '.agent.name // empty')
worktree=$(echo "$input" | jq -r '.worktree.name // empty')

parts="📂 $dir"

if [ -n "$used" ]; then
  parts="$parts  🧠 ${used}%"
fi

if [ -n "$dur_ms" ]; then
  dur_s=$((dur_ms / 1000))
  mins=$((dur_s / 60))
  secs=$((dur_s % 60))
  parts="$parts  ⏱ ${mins}m${secs}s"
fi

if [ -n "$agent" ]; then
  parts="$parts  🤖 $agent"
fi

if [ -n "$worktree" ]; then
  parts="$parts  🌳 $worktree"
fi

printf "\033[38;5;244m%s\033[0m" "$parts"
