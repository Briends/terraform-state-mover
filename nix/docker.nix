{inputs, ...}: {
  config = {
    perSystem = {
      lib,
      pkgs,
      system,
      self',
      ...
    }: let
      targetSystems = ["x86_64-linux" "aarch64-linux"];
      imageName = "terraform-state-mover";
      imageForSystem = system: {
        name = "docker-${imageName}-${system}";
        value = mkImage system;
      };
      linuxImages = lib.listToAttrs (map imageForSystem targetSystems);

      mkImage = targetSystem: let
        pkgs = import inputs.nixpkgs {
          localSystem = system;
          crossSystem = targetSystem;
        };
      in
        pkgs.dockerTools.buildLayeredImage {
          name = "briends/${imageName}";
          tag = "${targetSystem}";

          contents = [
            pkgs.hello
          ];

          config = {
            WorkingDir = "/workspace";
            Volumes = {
              "/workspace" = {};
            };
            Tmpfs = {
              "/tmp" = {};
            };
            # Cmd = ["${lib.getExe self'.packages.default}"];
          };
        };
    in {
      packages = linuxImages;
    };
  };
}
