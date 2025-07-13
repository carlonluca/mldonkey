let perl_path = "/usr/bin/perl"
let check_bounds = false
let current_version = "3.2.1.git"
let major_version = "3"
let minor_version = "2"
let sub_version = "1"
let scm_version = "next-release-v3.2.1-46-g0940447d-dune"
let glibc_version = "2.41"
let cc_version = "14"
let cxx_version = "14"
let build_system = "Linux x86_64 6.15.5-arch1-1"
let configure_arguments = " '--enable-option-checking=fatal' '--prefix=/mldonkey/out' '--enable-batch' '--enable-upnp-natpmp' '--enable-gnutella' '--enable-gnutella2' '--disable-gui'"

let system = "linux"
let windows = system = "cygwin" || system = "mingw"

let opennapster = "no"
let gnutella = "yes"
let gnutella2 = "yes"
let direct_connect = "yes"
let soulseek = "no"
let openft = "no"
let fasttrack = "yes"
let filetp = "yes"
let bittorrent = "yes"
let donkey = "yes"
let donkey_sui = "no"
let donkey_sui_urandom = ref false
let donkey_sui_works () = donkey_sui = "yes" && !donkey_sui_urandom

exception OutOfBoundsAccess
let outofboundsaccess = OutOfBoundsAccess
  
let check_string s pos =
  if check_bounds && pos >= String.length s then
    raise outofboundsaccess
  
let check_array s pos =
  if check_bounds && pos >= Array.length s then
    raise outofboundsaccess

let has_iconv = "yes" = "yes"

let has_gd = "yes" = "yes"
let has_gd_png = "yes" = "yes"
let has_gd_jpg = "yes" = "yes"

let bzip2 = "yes" = "yes"
let magic = "yes" = "yes"
let magic_works = ref false
let upnp_natpmp = "yes" = "yes"
