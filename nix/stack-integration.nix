let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:
with pkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "haskell-stack-nix";
  # System dependencies needed at compilation time
  buildInputs = 
    [ postgresql_16 
      pg_cron 
      lzma 
      git 
      zlib
      # 16 1637.0 bzlib-conduit                    > Error: Cabal-simple_9p6GVs8J_3.8.1.0_ghc-9.4.5: Missing dependency on a
      # 16 1637.0 bzlib-conduit                    > foreign library:
      # 16 1637.0 bzlib-conduit                    > * Missing (or bad) C library: bz2
      bzip2
      #16 848.3 HsOpenSSL                        > Error: setup: Missing dependencies on foreign libraries:
      #16 848.3 HsOpenSSL                        > * Missing (or bad) header file: openssl/asn1.h
      #16 848.3 HsOpenSSL                        > * Missing (or bad) C libraries: ssl, crypto
      openssl
    ];
}
