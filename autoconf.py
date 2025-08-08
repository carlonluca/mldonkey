# Copyright 2025 Luca Carlon
#
# This file is part of mldonkey.
#
# mldonkey is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# mldonkey is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mldonkey; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

import os
import shutil
import sys
import subprocess
import re
import ctypes
import platform

current_script_dir = os.path.dirname(os.path.abspath(__file__))
target_path = os.path.join(current_script_dir, "src", "utils", "lib", "autoconf.ml")

def get_perl_path():
    """Return the path to the perl executable, or None if not found."""
    return shutil.which("perl")

def get_latest_git_tag():
    """Return the latest Git tag in the directory of this script."""
    global current_script_dir
    try:
        tag = subprocess.check_output(
            ["git", "describe", "--tags", "--abbrev=0"],
            cwd=current_script_dir,
            stderr=subprocess.DEVNULL
        ).decode().strip()
        return tag
    except subprocess.CalledProcessError:
        return None

def get_git_version_description():
    try:
        result = subprocess.run(
            ["git", "describe", "--tags", "--long"],
            cwd=current_script_dir,
            text=True,
            capture_output=True,
            check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        print(f"Error running git describe: {e.stderr.strip()}")
        return None

def extract_semver(s):
    match = re.search(r'v?(\d+)\.(\d+)\.(\d+)', s)
    if match:
        major, minor, patch = map(int, match.groups())
        return major, minor, patch
    return None

def get_glibc_version():
    try:
        libc = ctypes.CDLL("libc.so.6")
        gnu_get_libc_version = libc.gnu_get_libc_version
        gnu_get_libc_version.restype = ctypes.c_char_p
        version = gnu_get_libc_version().decode("utf-8")
        return version
    except Exception as e:
        print(f"Could not determine glibc version: {e}")
        return None

def get_gcc_versions():
    def get_version(cmd):
        try:
            output = subprocess.check_output([cmd, "-dumpfullversion"], text=True)
            # First line, extract the last word that looks like a version
            first_line = output.splitlines()[0]
            version_str = first_line.split()[-1]
            return version_str
        except (subprocess.CalledProcessError, FileNotFoundError):
            return None
    gcc_version = get_version("gcc")
    gpp_version = get_version("g++")  # g++ is the GNU C++ compiler
    return gcc_version, gpp_version

def get_machine_type() -> str:
    system = platform.system()        # e.g. "Linux"
    machine = platform.machine()      # e.g. "x86_64"
    kernel = platform.release()       # e.g. "6.12.38-1-lts"
    return f"{system} {machine} {kernel}"

perl_path = get_perl_path()
if perl_path is None:
    sys.stderr.write("Error: perl not found in PATH.\n")
    sys.exit(1)

check_bounds = False
current_version = get_latest_git_tag()
if current_version is None:
    sys.stderr.write("Error: cannot find latest tag. git command missing?\n")
    sys.exit(1)

current_version = current_version + ".git"
semver = extract_semver(current_version)
if semver is None:
    sys.stderr.write("Error: cannot extract semver from git tag.\n")
    sys.exit(1)

major_version, minor_version, sub_version = semver
scm_version = get_git_version_description()
if scm_version is None:
    sys.stderr.write("Error: cannot extract version description from git.\n")
    sys.exit(1)

glibc_version = get_glibc_version() or "-"

gcc_versions = get_gcc_versions()
if gcc_versions is None:
    sys.stderr.write("Error: cannot get gcc versions.\n")
    sys.exit(1)

cc_version, cxx_version = gcc_versions

build_system = get_machine_type()
if build_system is None:
    sys.stderr.write("Error: cannot get machine type.\n")
    sys.exit(1)

configure_arguments = "-"
system = platform.system()
windows = 'system = "cygwin" || system = "mingw"'

opennapster = "no"
gnutella = "no"
gnutella2 = "no"
direct_connect = "no"
soulseek = "no"
openft = "no"
fasttrack = "no"
filetp = "no"
bittorrent = "yes"
donkey = "yes"
donkey_sui = "yes"
donkey_sui_urandom = "ref false"
donkey_sui_works = 'donkey_sui = "yes" && !donkey_sui_urandom'

has_iconv = True
has_gd = True
has_gd_png = True
has_gd_jpg = True
bzip2 = True
magic = True
magic_works = "ref false"
upnp_natpmp = True

with open(target_path, "w") as f:
    f.write(f'let perl_path = "{perl_path}"\n')
    f.write(f'let check_bounds = {str(check_bounds).lower()}\n')
    f.write(f'let current_version = "{current_version}"\n')
    f.write(f'let major_version = "{major_version}"\n')
    f.write(f'let minor_version = "{minor_version}"\n')
    f.write(f'let sub_version = "{sub_version}"\n')
    f.write(f'let scm_version = "{scm_version}"\n')
    f.write(f'let glibc_version = "{glibc_version}"\n')
    f.write(f'let cc_version = "{cc_version}"\n')
    f.write(f'let cxx_version = "{cxx_version}"\n')
    f.write(f'let build_system = "{build_system}"\n')
    f.write(f'let configure_arguments = "{configure_arguments}"\n\n')

    f.write(f'let system = "{system}"\n')
    f.write(f'let windows = {windows}\n\n')

    f.write(f'let opennapster = "{opennapster}"\n')
    f.write(f'let gnutella = "{gnutella}"\n')
    f.write(f'let gnutella2 = "{gnutella2}"\n')
    f.write(f'let direct_connect = "{direct_connect}"\n')
    f.write(f'let soulseek = "{soulseek}"\n')
    f.write(f'let openft = "{openft}"\n')
    f.write(f'let fasttrack = "{fasttrack}"\n')
    f.write(f'let filetp = "{filetp}"\n')
    f.write(f'let bittorrent = "{bittorrent}"\n')
    f.write(f'let donkey = "{donkey}"\n')
    f.write(f'let donkey_sui = "{donkey_sui}"\n')
    f.write(f'let donkey_sui_urandom = {donkey_sui_urandom}\n')
    f.write(f'let donkey_sui_works () = {donkey_sui_works}\n\n')

    f.write("exception OutOfBoundsAccess\n")
    f.write("let outofboundsaccess = OutOfBoundsAccess\n\n")
    f.write("let check_string s pos =\n")
    f.write("  if check_bounds && pos >= String.length s then\n")
    f.write("    raise outofboundsaccess\n\n")
    f.write("let check_array s pos =\n")
    f.write("  if check_bounds && pos >= Array.length s then\n")
    f.write("    raise outofboundsaccess\n\n")

    f.write(f'let has_iconv = {str(has_iconv).lower()}\n')
    f.write(f'let has_gd = {str(has_gd).lower()}\n')
    f.write(f'let has_gd_png = {str(has_gd_png).lower()}\n')
    f.write(f'let has_gd_jpg = {str(has_gd_jpg).lower()}\n\n')

    f.write(f'let bzip2 = {str(bzip2).lower()}\n')
    f.write(f'let magic = {str(magic).lower()}\n')
    f.write(f'let magic_works = {magic_works}\n')
    f.write(f'let upnp_natpmp = {str(upnp_natpmp).lower()}\n')

print("Wrote OCaml config to config.ml")