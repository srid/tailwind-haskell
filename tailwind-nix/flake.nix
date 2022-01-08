{
  description = "tailwind-nix";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        name = "tailwind";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };
        tailwindPkgs = import ./default.nix {
          inherit pkgs system;
        };

        tailwind =
          let
            p = tailwindPkgs.package;
            node_modules = "${p}/lib/node_modules/tailwind-nix/node_modules";
          in
          pkgs.writeShellScriptBin "tailwind"
            ''
              export NODE_PATH=${node_modules}
              exec ${node_modules}/.bin/tailwind $*
            '';
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = tailwind;
      });
}
