{
  description = "tailwind-haskell's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/22dc22f8cedc58fcb11afe1acb08e9999e78be9c";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        tailwindCss =
          pkgs.nodePackages.tailwindcss.overrideAttrs (oa: {
            plugins = [
              pkgs.nodePackages."@tailwindcss/aspect-ratio"
              pkgs.nodePackages."@tailwindcss/forms"
              pkgs.nodePackages."@tailwindcss/language-server"
              pkgs.nodePackages."@tailwindcss/line-clamp"
              pkgs.nodePackages."@tailwindcss/typography"
            ];
          });
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
              relude = self.callHackage "relude" "1.0.0.1" { };  # Not on nixpkgs, for some reason.
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with pkgs.haskellPackages; [
                  # Specify your build/dev dependencies here. 
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                  tailwindCss
                ]);
          };
      in
      rec {
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
