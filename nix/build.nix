let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
in
let
  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  #
  # - nix: Enable Nix support
  # - no-nix-pure: Pass environment variables, like `NIX_PATH`
  # - nix-shell-file: Specify the Nix file to use (otherwise it uses `shell.nix` by default)
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };
  openapi-generator = import ./openapi3-haskell.nix {};
in
with pkgs;
mkShell {
  buildInputs = [
    stack-wrapped
    openapi-generator
    glibcLocalesUtf8
    glibcLocales
    python311
    haskell.compiler.ghc928
    ormolu
    python3
    expat
    gcc
    postgresql_16
    pg_cron
    gnumake
    figlet
    # 16 1637.0 bzlib-conduit                    > Error: Cabal-simple_9p6GVs8J_3.8.1.0_ghc-9.4.5: Missing dependency on a
    # 16 1637.0 bzlib-conduit                    > foreign library:
    # 16 1637.0 bzlib-conduit                    > * Missing (or bad) C library: bz2
    # 16 1637.0 bzlib-conduit                    > This problem can usually be solved by installing the system package that
    # 16 1637.0 bzlib-conduit                    > provides this library (you may need the "-dev" version). If the library is
    # 16 1637.0 bzlib-conduit                    > already installed but in a non-standard location then you can use the flags
    # 16 1637.0 bzlib-conduit                    > --extra-include-dirs= and --extra-lib-dirs= to specify where it is.If the
    # 16 1637.0 bzlib-conduit                    > library file does exist, it may contain errors that are caught by the C
    # 16 1637.0 bzlib-conduit                    > compiler at the preprocessing stage. In this case you can re-run configure
    # 16 1637.0 bzlib-conduit                    > with the verbosity flag -v3 to see the error messages.
    bzip2
  ];
  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs> when looking for the right `ghc` argument to pass in `nix/stack-integration.nix`
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + path;
  shellHook = ''
      figlet -f smslant Welcome to B-Correspondent nix-shell!
  '';
}