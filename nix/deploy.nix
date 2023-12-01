let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
in
with pkgs;
mkShell {
  LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive"; 
  buildInputs = [ postgresql_16 lzma zlib glibcLocales gnumake ];
}