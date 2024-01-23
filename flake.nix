{
  description = "tailwind-haskell's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs =
          nixpkgs.legacyPackages.${system};
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
