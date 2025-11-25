#!/usr/bin/env python3
"""
safe_unify_signatures.py
Generates conservative 'sed' scripts for commonly-needed renames found in diffs.
The sed scripts are stored in ./sed_patches/ and create .bak backups when run.
Review the generated scripts thoroughly before running them.

Usage:
  python3 safe_unify_signatures.py
"""
from pathlib import Path
import textwrap, os

sed_dir = Path('sed_patches')
sed_dir.mkdir(exist_ok=True)

rules = [
    ('timestep_len', 'timestep', 'Rename timestep_len -> timestep (verify types and callers)'),
    ('riverout_rgrid', 'rivers_outflow_rp', 'Rename riverout_rgrid -> rivers_outflow_rp (verify intents)'),
    ('ch4_mml', 'm_water', 'Replace ch4_mml with m_water constant (verify semantic)'),
]

for pat, repl, desc in rules:
    fname = sed_dir / f"{pat}_to_{repl}.sh"
    content = textwrap.dedent(f"""\
    #!/bin/sh
    # {desc}
    # WARNING: review before running. This script creates *.bak backups for edited files.
    set -euo pipefail
    FILES=$(grep -RIn --exclude-dir=.git -E "{pat}" . | cut -d: -f1 | sort -u || true)
    if [ -z "$FILES" ]; then
      echo "No files found for pattern {pat}"
      exit 0
    fi
    for F in $FILES; do
      echo "Patching $F"
      sed -E -i.bak -e 's/{pat}/{repl}/g' "$F"
    done
    echo "Done. Backups saved as *.bak"
    """)
    fname.write_text(content)
    os.chmod(fname, 0o755)

print("Generated sed patch scripts in ./sed_patches/ — review before running.")

