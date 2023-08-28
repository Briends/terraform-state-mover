{
  config,
  self',
  inputs',
  pkgs,
  system,
  ...
}: {
  config = {
    packages =
      pkgs.lib.optionalAttrs (system == "x86_64-linux")
      {
        dockerImage =
          pkgs.dockerTools.builLayeredImage
          {
            name = "briends/terraform-state-mover";
            tag = "${system}";
            contents = [
              self'.packages.default
              pkgs.terraform
            ];
            config = {
              WorkingDir = "/workspace";
              Volumes = {
                "/workspace" = {};
              };
              Tmpfs = {
                "/tmp" = {};
              };
              Cmd = ["${pkgs.lib.getExe self'.packages.default}"];
            };
          };
      };
  };
}
