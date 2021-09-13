{
  description = "flake-lock-diff";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt, nix-filter, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "flake-lock-diff";
            root = nix-filter.lib.filter {
              root = ./.;
              include = [
                "flake-lock-diff.cabal"
                (nix-filter.lib.inDirectory "app")
              ];
            };
            withHoogle = true;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              if returnShellEnv
              then [
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
                pkgs.nixpkgs-fmt
                treefmt.defaultPackage.${system}
              ]
              else [ ]
              );
          };
      in
      {
        packages = {
          flake-lock-diff = project false;
          lock-update = pkgs.writeShellScriptBin "lock-update" ''
            set -e
            function lock_updated {
              git diff --quiet flake.lock &> /dev/null
            }
            nix flake update
            if ! lock_updated
            then
              git add flake.lock
              git show HEAD:flake.lock | ${self.packages.${system}.flake-lock-diff}/bin/flake-lock-diff ./flake.lock | git commit -F -
            fi
          '';
        };
        defaultPackage = self.packages.${system}.flake-lock-diff;
        devShell = project true;
      }
    )
  ;
}
