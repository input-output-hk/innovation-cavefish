{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell =
    ghc:
    import ./shell.nix {
      inherit
        inputs
        pkgs
        lib
        project
        utils
        ghc
      ;
    };

  wbpsDevShell =
    pkgs.mkShell {
      packages = with pkgs; [

        gnumake
        # The Deno runtime
        deno
        # JavaScript and TypeScript tools
        typescript
        nodejs_22

        # Tools for formatting and linting
        nodePackages.eslint_d # Fast, daemonized version of ESLint
        nodePackages.prettier

        # Python tools
        nixfmt-rfc-style
        pkgs.python3Packages.python-lsp-server
        pkgs.python3Packages.pip
        pkgs.python3Packages.click
        pkgs.python3Packages.mypy
        python3Packages.nox
        python3Packages.pytest
        python3Packages.ruff

        # circom circuit compiler
        circom
        #
        # nixfmt for formatting Nix code according to RFC style
        pkgs.nixfmt-rfc-style
        # misc tools
        codespell
        figlet
      ];

      shellHook = ''
        npm install snarkjs@latest --save-dev
        npm install vite@latest --save-dev

        export PATH=$PWD/node_modules/.bin:$PATH
        echo "WBPS dev shell" | figlet -f cybermedium
        #
        # to set up project
        # deno init --npm vite --template react-ts
      '';
    };

  packages = { };

  devShells = rec {
    default = ghc966;
    ghc966 = mkShell "ghc966";
    ghc984 = mkShell "ghc984";
    ghc9102 = mkShell "ghc9102";
    ghc9122 = mkShell "ghc9122";
    wbps = wbpsDevShell;
  };

  projectFlake = project.flake { };

  defaultHydraJobs = {
    ghc966 = projectFlake.hydraJobs.ghc966;
    ghc984 = projectFlake.hydraJobs.ghc984;
    ghc9102 = projectFlake.hydraJobs.ghc9102;
    ghc9122 = projectFlake.hydraJobs.ghc9122;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
  };

  hydraJobsPerSystem = {
    "x86_64-linux" = defaultHydraJobs;
    "x86_64-darwin" = defaultHydraJobs;
    "aarch64-linux" = defaultHydraJobs;
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
in

{
  inherit packages;
  inherit devShells;
  inherit hydraJobs;
}
