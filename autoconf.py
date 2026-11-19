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
import argparse

current_script_dir = os.path.dirname(os.path.abspath(__file__))
target_path = os.path.join(current_script_dir,
                           "src", "utils", "lib", "autoconf.ml")
networks_target_path = os.path.join(current_script_dir,
                                    "src", "networks", "dune")
enabled_networks = ""
disabled_dirs = ""

# Dictionary with the list of networks that can be enabled and disabled.
# "directory" refers to the corresponding directory in src/networks
# "library" referst to the library name defined in src/networks/*/dune
# If "directory" or "library" are not set, they default to the network key

networks = {
    "gnutella": {
        "description": "Gnutella",
        "default": True
    },
    "gnutella2": {
        "description": "Gnutella2",
        "default": False
    },
    "direct_connect": {
        "library": "directconnect",
        "description": "Direct Connect",
        "default": True
    },
    "fasttrack": {
        "description": "FastTrack",
        "default": True
    },
    "filetp": {
        "directory": "fileTP",
        "description": "FileTP",
        "default": True
    },
    "bittorrent": {
        "description": "BitTorrent",
        "default": True
    },
    "donkey": {
        "description": "eDonkey",
        "default": True
    }
}

argparser = argparse.ArgumentParser(description="Configure MLDonkey build")

for k, v in networks.items():
    argparser.add_argument(f"--enable-{v['library'] if 'library' in v else k}",
                           action="store_const",
                           const=True,
                           default=v["default"],
                           help=f"Enable {v['description']} network{' (default)' if v['default'] else ''}",
                           dest=k)
    argparser.add_argument(f"--disable-{v['library'] if 'library' in v else k}",
                           action="store_const",
                           const=False,
                           default=v["default"],
                           help=f"Disable {v['description']} network{' (default)' if not v['default'] else ''}",
                           dest=k)

argparser.add_argument("--enable-upnp-natpmp",
                       action="store_const",
                       const=True,
                       default=True,
                       help="Enable the use of libminiupnpc and libnatpmp",
                       dest="upnp_natpmp")
argparser.add_argument("--disable-upnp-natpmp",
                       action="store_const",
                       const=False,
                       default=True,
                       help="Disable the use of libminiupnpc and libnatpmp",
                       dest="upnp_natpmp")

args = vars(argparser.parse_args())


def get_perl_path():
    """Return the path to the perl executable, or None if not found."""
    return shutil.which("perl")


def get_latest_semver_tag():
    try:
        latest_tag = subprocess.check_output(
            ["git", "describe", "--tags", "--abbrev=0"],
            text=True
        ).strip()
        match = re.search(r'(\d+\.\d+\.\d+)', latest_tag)
        return match.group(1) if match else None
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


def is_commit_tagged():
    try:
        tags = subprocess.check_output(
            ["git", "tag", "--points-at", "HEAD"],
            text=True
        ).strip()
        return bool(tags)
    except subprocess.CalledProcessError:
        return False


perl_path = get_perl_path()
if perl_path is None:
    sys.stderr.write("Error: perl not found in PATH.\n")
    sys.exit(1)

check_bounds = False
current_version = get_latest_semver_tag()
if current_version is None:
    sys.stderr.write("Error: cannot find latest tag. git command missing?\n")
    sys.exit(1)

if not is_commit_tagged():
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

configure_arguments = " ".join(sys.argv[1:] or "-")
system = platform.system()
windows = 'system = "cygwin" || system = "mingw"'

opennapster = "no"
soulseek = "no"
openft = "no"
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
upnp_natpmp = args["upnp_natpmp"]

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

    for k, v in networks.items():
        f.write(f'let {k} = "{"yes" if args[k] else "no"}"\n')
        if args[k]:
            enabled_networks += f"{v['library'] if 'library' in v else k}\n"
        else:
            disabled_dirs += f"{v['directory'] if 'directory' in v else k} "
    f.write(f'let opennapster = "{opennapster}"\n')
    f.write(f'let soulseek = "{soulseek}"\n')
    f.write(f'let openft = "{openft}"\n')
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

print(f"Wrote OCaml config to {target_path}")

with open(networks_target_path, "w") as f:
    if disabled_dirs:
        f.write(f'(dirs :standard \\ {disabled_dirs})\n')
    f.write(f"""(library
    (name networks)
    (wrapped false)
    (modules)
    (libraries
        {enabled_networks}
    ))""")

print(f"Wrote dune network selection to {networks_target_path}")

with open(os.path.join(current_script_dir,
                       "src", "utils", "net", "upnp_cflags.sexp"), "w") as f:
    f.write("(-DENABLE_UPNP_NATPMP)" if args["upnp_natpmp"] else "()")
with open(os.path.join(current_script_dir,
                       "src", "upnp_link_flags.sexp"), "w") as f:
    f.write("(-cclib -lminiupnpc -cclib -lnatpmp)" if args["upnp_natpmp"] else "()")
