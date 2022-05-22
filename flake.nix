{
  description = "tailwind-haskell's description";
  inputs = {
    ema.url = "github:srid/ema"; # Using ema only for its nixpkgs; TODO: don't do this, and don't use tailwind-haskell as a flake.
    nixpkgs.follows = "ema/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
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
        apps.default = {
          type = "app";
          program = pkgs.writeShellApplication
            {
              name = "tailwind-run.sh";
              text = ''
                set -xe
                exec ${packages.tailwind}/bin/tailwind-run "$@"
              '';
            } + /bin/tailwind-run.sh;
        };

        # Used by `nix develop` (dev shell)
        devShell = project true;
        defaultPackage = packages.tailwind;
        defaultApp = apps.default;
      });
}
