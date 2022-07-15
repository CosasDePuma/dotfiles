{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;

mkShell {
  buildInputs = [
    # fuzzers
    dirb
    ffuf
    gobuster
    wfuzz
    # probers
    httpx
    metasploit
    # scanners
    nikto
    nuclei
    nmap
    sqlmap
  ];
}

