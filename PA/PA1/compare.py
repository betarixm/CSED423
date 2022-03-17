from wasabi import *

import os
import sys
import glob
import subprocess

import difflib

LEXER_PATH = "src/lexer"
REF_LEXER_PATH = "reference/lexer"


def build():
    p = subprocess.Popen(
        ["make"], cwd="src", stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    return p.communicate()


def clean():
    p = subprocess.Popen(
        ["make", "clean"], cwd="src", stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    return p.communicate()


def diff_strings(a, b):
    output = []
    matcher = difflib.SequenceMatcher(None, a, b)
    for opcode, a0, a1, b0, b1 in matcher.get_opcodes():
        if opcode == "equal":
            output.append(a[a0:a1])
        elif opcode == "insert":
            output.append(color(b[b0:b1], fg=16, bg="green"))
        elif opcode == "delete":
            output.append(color(a[a0:a1], fg=16, bg="red"))
        elif opcode == "replace":
            output.append(color(b[b0:b1], fg=16, bg="green"))
            output.append(color(a[a0:a1], fg=16, bg="red"))
    return "".join(output)


def generate_abspath(filename: str):
    return os.path.abspath(filename)


def run(runner_path: str, abs_path: str):
    runner_name = runner_path.split("/")[-1]
    p = subprocess.Popen(
        [f"./{runner_name}", abs_path],
        cwd="/".join(runner_path.split("/")[:-1]),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return p.communicate()


def compare(filepath: str):

    out, _ = run(LEXER_PATH, generate_abspath(filepath))
    ref_out, _ = run(REF_LEXER_PATH, generate_abspath(filepath))

    out, ref_out = out.decode("utf-8"), ref_out.decode("utf-8")

    if out != ref_out:
        print(diff_strings(out, ref_out))
        print(color("[-] Result: Diff", fg="red"))
    else:
        print(color("[+] Result: Same", fg="green"))


if __name__ == "__main__":
    build()

    for filename in list(
        glob.glob(sys.argv[1])
    ):
        print(f"[*] {filename.split('/')[-1]}")
        compare(filename)
        print()

    clean()
