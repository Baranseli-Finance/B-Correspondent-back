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
  buildInputs = [ postgresql_16 pg_cron lzma git zlib ];
}
