{ pkgs, postgresql, ext }:

pkgs.stdenv.mkDerivation {
  name = "pg_cron";
  buildInputs = [ postgresql ];

  src = pkgs.fetchFromGitHub {
    owner = "citusdata";
    repo = "pg_cron";
    rev = "be3876deead213dc33db87c7fad4f72345f14c81";
    sha256 = "sha256-/dD1gX0+RRsBFIjSV9TVk+ppPw0Jrzssl+rRZ2qAp4w=";
    };

  installPhase = ''
    mkdir -p $out/bin
    install -D ${ext}/lib/pg_cron.so -t $out/lib
    install -D -t $out/share/postgresql/extension *.sql
    install -D -t $out/share/postgresql/extension pg_cron.control
  '';
}