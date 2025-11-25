#!/usr/bin/env python3
"""
generate_patch_stubs.py
Reads /mnt/data/science_diff.txt or a provided diff file and creates per-file
stub files in ./patches/:
  - <safe_header>.patch.stub.txt  (human-readable info and preview)
  - <safe_header>.patch           (empty patch file for you to fill in unified diff format)

Usage:
  python3 generate_patch_stubs.py /mnt/data/science_diff.txt
If no argument given, defaults to /mnt/data/science_diff.txt.
"""
import sys
from pathlib import Path
import re, textwrap
from datetime import datetime

diff_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path('/mnt/data/science_diff.txt')
if not diff_path.exists():
    print(f"Diff file not found: {diff_path}")
    raise SystemExit(1)

out_dir = Path('patches')
out_dir.mkdir(exist_ok=True)

txt = diff_path.read_text(errors='ignore')

# Split into parts where header lines start with "diff -r " or "Only in "
parts = re.split(r'(?=^diff -r |^Only in )', txt, flags=re.M)

def safe_name(s):
    s2 = re.sub(r'[^0-9A-Za-z._\\-]', '_', s)[:180]
    return s2.replace('/', '__')

for p in parts:
    p = p.strip()
    if not p:
        continue
    header = p.splitlines()[0]
    name = safe_name(header)
    stub_file = out_dir / (name + '.patch.stub.txt')
    patch_file = out_dir / (name + '.patch')
    # counts
    lines = p.splitlines()
    added = sum(1 for L in lines if L.startswith('>'))
    removed = sum(1 for L in lines if L.startswith('<'))
    onlyin = header.startswith('Only in')
    preview = '\n'.join(lines[0:160])
    recommendations = []
    if onlyin:
        recommendations.append("File exists only in one tree: decide whether to copy/integrate.")
    if 'NAMELIST' in p or 'READ (UNIT' in p or 'jules_deposition_species' in p:
        recommendations.append("Namelist/IO changes: reconcile derived types and broadcast logic.")
    if 'soil_model_4pool' in p or 'dzsoil' in p or 'z_burn_max' in p:
        recommendations.append("Soil-layer/4-pool changes: verify indexing and dzsoil availability.")
    if 'timestep_len' in p and 'timestep' in p:
        recommendations.append("Signature rename: unify timestep_len/timestep or provide wrapper.")
    if not recommendations:
        recommendations.append("Manual review recommended; inspect the preview below.")

    stub_text = f"""\
Patch stub generated: {datetime.utcnow().isoformat()} UTC

HEADER:
{header}

SUMMARY:
  - added lines (>): {added}
  - removed lines (<): {removed}
  - only in one tree: {onlyin}

RECOMMENDATIONS:
{chr(10).join('- ' + r for r in recommendations)}

DIFF PREVIEW:
{preview}

INSTRUCTIONS:
  1) Edit the corresponding .patch file: {patch_file}
     - Place a git-style unified diff (``--- a/..., +++ b/...``) there.
  2) Create small focused patches (one logical change per file).
  3) Run apply_patches.sh (after filling patches) from the repository root.

"""
    stub_file.write_text(textwrap.dedent(stub_text))
    # create an empty patch template if missing
    if not patch_file.exists():
        patch_file.write_text("# Fill this file with a git-style unified patch (use git diff format)\n")
print("Stub generation complete. See ./patches/")

