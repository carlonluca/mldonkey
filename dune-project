(lang dune 3.12)
(name mldonkey)
(package
    (name mldonkey)
    (allow_empty)
    (synopsis "Cross-platform multi-network peer-to-peer daemon")
    (description "
 Supported networks include:
 * eDonkey (with Overnet and Kad)
 * BitTorrent (with DHT)
 * Direct Connect
 * HTTP/FTP
MLdonkey core runs as a background daemon and provides several control
interfaces : telnet, web, and binary protocol for third party GUIs.
")
    (depends
        (ocaml (= 4.14.2))
        dune
        unix
        str
        threads
        camlp-streams
        curl
        num
        xml-light
        syslog
        uutf
        uucp
    )
)
