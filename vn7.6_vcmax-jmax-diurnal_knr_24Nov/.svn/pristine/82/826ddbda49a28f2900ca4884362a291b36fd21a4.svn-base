#!/usr/bin/env python3
# *****************************COPYRIGHT******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT******************************
#
"""
Loop through a dictionary of sites with known working groups
(WORKING_CONFIGS) to validate each one.
Expected to be run as part of a rose-stem suite
"""

import os
import sys
import subprocess
import argparse

WORKING_CONFIGS = {
    "cehwl1": ["all"],
    "jasmin": ["all"],
    "meto": ["all", "ex1a"],
    "nci": ["all"],
    "niwa": ["all"],
    "vm": ["all"],
}

def is_cylc8():
    """
    Determine if the Cylc version is Cylc 8 (or later)

    """

    if "CYLC_VERSION" not in os.environ:
        sys.exit("CYLC_VERSION not in environment")

    if int(os.environ["CYLC_VERSION"].split(".")[0]) < 8:
        return False
    else:
        return True


def run_command(command):
    """
    Launch a subprocess command, capture the output and return the result
    """
    pobj = subprocess.Popen(
        command.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    pobj.wait()
    retcode, stdout, stderr = (
        pobj.returncode,
        pobj.stdout.read().decode("utf-8"),
        pobj.stderr.read().decode("utf-8"),
    )
    return retcode, stdout, stderr


def generate_validate_command(source, cylc8, site, group):
    """
    Generate a command to determine if rose-stem suite is valid.
    """

    suitename = f"test_validate_jules_{site}_{group}"

    if cylc8:
        install_cmd = (
            "cylc validate --check-circular "
            f"-S RUN_NAMES=['{group}'] "
            f"-S SITE='{site}' "
            f"-S SOURCE_JULES='{source}' "
            f"-S SOURCE_JULES_BASE='{source}' "
            f"-S HOST_SOURCE_JULES='{source}' "
            f"-S HOST_SOURCE_JULES_BASE='{source}' "
            f"{source}"
        )
    else:
        install_cmd = (
            "rose suite-run "
            f"--config={source} "
            f"--name={suitename} "
            "--validate-suite-only --new "
            f"-S RUN_NAMES=['{group}'] "
            f"-S SITE='{site}' "
            f"-S SOURCE_JULES='{source}' "
            f"-S SOURCE_JULES_BASE='{source}' "
            f"-S HOST_SOURCE_JULES='{source}' "
            f"-S HOST_SOURCE_JULES_BASE='{source}'"
        )

    return install_cmd, suitename


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Validate rose-stem Jules suites for different sites"
    )
    parser.add_argument(
        "-s",
        "--source",
        help="The Jules Source",
        required=True,
    )
    args = parser.parse_args()

    failures = False
    for site in WORKING_CONFIGS:
        for group in WORKING_CONFIGS[site]:
            print(f"[INFO] Validating {site} with {group}")
            command, suitename = generate_validate_command(
                args.source, is_cylc8(), site, group
            )
            retcode, stdout, stderr = run_command(command)
            if retcode:
                print(f"[FAIL] {site} with {group} failed to validate")
                print(stdout)
                print(stderr, file=sys.stderr)
                failures = True
            else:
                print(f"[Pass] {site} with {group} validated successfully")
                if not is_cylc8():
                    clean_command = f"rose suite-clean --yes {suitename}"
                    clean_retcode, clean_stdout, clean_stderr = run_command(
                        clean_command
                    )
                    if clean_retcode:
                        print(clean_stdout)
                        print(clean_stderr, file=sys.stderr)
                        print(f"[FAIL] Suite {suitename} failed to clean")
                        failures = True

    if failures:
        sys.exit(1)
