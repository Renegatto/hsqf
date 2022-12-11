{
  description = "Based on Plutarch 2.0";
  inputs = {
    plutarch-core.url = "github:Plutonomicon/plutarch-core";
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  };
  outputs = inputs@{ self, tooling, plutarch-core }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.shell.withHoogle = true;
          project.modules = [
            ({ config, ... }: {
              packages.plutus-simple-model.doHaddock = false;
            })
          ];
          project.extraHackage = [ "${plutarch-core}" ];
        })
      ];
    };

}