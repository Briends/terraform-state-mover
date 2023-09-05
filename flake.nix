{
  description = "terraform-state-mover";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    ...
  }: let
    overlay = final: prev: {
      haskell =
        prev.haskell
        // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
            // {
              terraform-state-mover = hfinal.callCabal2nix "terraform-state-mover" ./. {};
            };
        };
      terraform-state-mover = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.terraform-state-mover;
    };
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = {
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          withHoogle = true;
          packages = p: [p.terraform-state-mover];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.ormolu
            pkgs.bashInteractive
            pkgs.terraform
          ];
        };
        packages.default = pkgs.terraform-state-mover;
        apps.default.program = pkgs.terraform-state-mover;
      };

      flake = {
      };
    };
}
