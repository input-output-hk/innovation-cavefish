{
  description = "Cavefish monorepo";

  inputs = {
    cavefish-server.url = "path:./cavefish-server";
  };

  outputs =
    { cavefish-server, ... }:
    {
      inherit (cavefish-server) packages devShells hydraJobs;
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
