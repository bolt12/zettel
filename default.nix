{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "zettel" =
        hself.callCabal2nix
          "zettel"
          (gitignore ./.)
          {};
      "polysemy-plugin" =
        pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
          pkg = "polysemy-plugin";
          ver = "0.2.5.0";
          sha256 = "0jnps8kwxd0hakis5ph77r45mv1qnkxdf5506shcjb1zmxqmxpjv";
        }
        {});
      "polysemy" =
        pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
          pkg = "polysemy";
          ver = "1.3.0.0";
          sha256 = "1p75i56qpl0v79vrlzw04117czzgwhn1l0vadvka8m7drmcvwsf6";
        }
        {});
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."zettel"
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      pkgs.neo4j
      (import sources.niv {}).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."zettel");

  docker = pkgs.dockerTools.buildImage {
    name = "zettel";
    config.Cmd = [ "${exe}/bin/zettel" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "zettel" = myHaskellPackages."zettel";
}
