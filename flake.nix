{
  description = "tailwind's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/568e0bc498ee51fdd88e1e94089de05f2fdbd18b";
    tailwind-nix.url = "github:srid/tailwind-nix";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
          };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "tailwind-haskell";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              # Example: 
              # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
              # Assumes that you have the 'NanoID' flake input defined.
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with (if system == "aarch64-darwin"
                then m1MacHsBuildTools
                else pkgs.haskellPackages); [
                  # Specify your build/dev dependencies here. 
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                  inputs.tailwind-nix.defaultPackage.${system}
                ]);
          };
      in
      rec {
        # Used by `nix build` & `nix run` (prod exe)
        packages.tailwind = project false;
        defaultPackage = packages.tailwind;
        defaultApp = rec {
          type = "app";
          script = pkgs.writers.writeBash "tailwind-run.sh" ''
            set -xe
            exec ${packages.tailwind}/bin/tailwind-run $* 
          '';
          program = builtins.toString script;
        };

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
