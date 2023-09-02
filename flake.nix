{
  description = "terraform-state-mover";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs: let
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
    perSystem = system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay];
      };
      hspkgs = pkgs.haskellPackages;
    in {
      devShells.default = hspkgs.shellFor {
        withHoogle = true;
        packages = p: [p.terraform-state-mover];
        buildInputs = [
          hspkgs.cabal-install
          hspkgs.haskell-language-server
          hspkgs.hlint
          hspkgs.ormolu
          pkgs.bashInteractive
          pkgs.terraform
        ];
      };
      packages.default = pkgs.terraform-state-mover;
    };
  in
    {overlays.default = overlay;} // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
