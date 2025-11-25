#!/usr/bin/env bash
# apply_patches.sh
# Usage:
#   ./apply_patches.sh          # apply all filled patches in ./patches/, create local branches and commit
#   ./apply_patches.sh --push   # also push branches to origin
set -euo pipefail

PATCH_DIR="./patches"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
PUSH_FLAG=${1:-""}

if [ ! -d "$PATCH_DIR" ]; then
  echo "Patch directory not found: $PATCH_DIR"
  exit 1
fi

for P in "$PATCH_DIR"/*.patch; do
  [ -e "$P" ] || continue
  # Skip template/empty patches
  if grep -q "^# Fill this file" "$P"; then
    echo "Skipping template patch: $P"
    continue
  fi
  BASENAME=$(basename "$P" .patch)
  BRANCH="merge/${BASENAME}_${TIMESTAMP}"
  echo "=== Processing patch: $P"
  echo "Creating branch: $BRANCH"
  git checkout -b "$BRANCH"
  if git apply --check "$P"; then
    echo "Patch passes git apply --check; applying..."
    git apply "$P"
    git add -A
    git commit -m "Apply patch: ${BASENAME}"
    if [ -f Makefile ]; then
      echo "Performing dry build check: make -n"
      if ! make -n; then
        echo "Dry build reported issues. Inspect locally but branch remains for debugging."
      else
        echo "Dry build OK for $BRANCH."
      fi
    else
      echo "No Makefile in repo root; skipping build check."
    fi
    if [ "$PUSH_FLAG" = "--push" ]; then
      git push -u origin "$BRANCH"
      echo "Pushed branch: $BRANCH"
    else
      echo "Branch created locally: $BRANCH (not pushed)."
    fi
  else
    echo "git apply --check FAILED for $P. Aborting this patch. Restoring previous branch."
    git checkout -
  fi
done

echo "All patches processed. Inspect branches and run tests."

