{pkgs, ...}: let
  project =
    pkgs.haskell-nix.project'
    {
      name = "terraform-state-mover";
      src = ./.;
      evalSystem = "aarch64-darwin";
      compiler-nix-name = "ghc928";
      shell.tools = {
        cabal = {};
        hlint = {};
        haskell-language-server = {};
        ormolu = {};
      };
      shell.buildInputs = with pkgs; [
        alejandra
      ];
    };
  flake = project.flake {};
in {
  apps.default = flake.apps."terraform-state-mover:exe:terraform-state-mover";
  packages.default = flake.packages."terraform-state-mover:exe:terraform-state-mover";
  devShells = flake.devShells;
}
