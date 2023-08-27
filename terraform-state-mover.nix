{pkgs, ...}: let
  project =
    pkgs.haskell-nix.project'
    {
      name = "terraform-state-mover";
      src = ./.;
      evalSystem = "aarch64-darwin";
      compiler-nix-name = "ghc928";
    };
  flake = project.flake {};
in {
  packages.default = flake.packages."terraform-state-mover:exe:terraform-state-mover";
}
