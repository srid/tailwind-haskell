{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem = { config, system, pkgs, ... }:
        let
          tailwindCss = pkgs.nodePackages.tailwindcss.overrideAttrs (oa: {
            plugins = [
              pkgs.nodePackages."@tailwindcss/aspect-ratio"
              pkgs.nodePackages."@tailwindcss/forms"
              pkgs.nodePackages."@tailwindcss/language-server"
              pkgs.nodePackages."@tailwindcss/line-clamp"
              pkgs.nodePackages."@tailwindcss/typography"
            ];
          });
        in
        {
          # Haskell configuration
          haskellProjects.default = {
            settings = {
              tailwind = {
                extraBuildDepends = [ tailwindCss ];
              };
            };
            # Development shell configuration
            devShell = {
              enable = true;
              mkShellArgs = {
                nativeBuildInputs = [
                  pkgs.nixpkgs-fmt
                  tailwindCss
                ];
              };
            };
          };

          # Define apps
          apps = {
            default = {
              type = "app";
              program = "${pkgs.writeShellApplication {
                name = "tailwind-run.sh";
                text = ''
                  set -xe
                  exec ${config.packages.tailwind}/bin/tailwind-run "$@"
                '';
              }}/bin/tailwind-run.sh";
            };
          };
        };
    };
}
