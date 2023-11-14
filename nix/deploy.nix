let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
in
pkgs.mkShell {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive"; 
  buildInputs = [ pkgs.postgresql_16 pkgs.lzma pkgs.zlib pkgs.imagemagick pkgs.glibcLocales pkgs.gnumake ];
  }