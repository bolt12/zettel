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
          pkg = sources.polysemy-plugin.name;
          ver = sources.polysemy-plugin.version;
          sha256 = sources.polysemy-plugin.sha256;
        }
        {});
      "polysemy" =
        pkgs.haskell.lib.dontCheck (hself.callCabal2nix
          "polysemy"
          sources.polysemy
        {});

      mkDerivation = args: hsuper.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        jailbreak = true;
      });
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
