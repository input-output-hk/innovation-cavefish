{
  description = "Weakly Blind Predicate Signature development shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in

      {
        devShells.default = pkgs.mkShell {
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

          # Configure ESLint and Prettier for the shell
          shellHook = ''
            if [ ! -d "node_modules/snarkjs" ]; then
              npm install snarkjs@0.7.0 --save-dev
            fi
            export PATH=$PWD/node_modules/.bin:$PATH
            echo "WBPS dev shell" | figlet -f cybermedium
          '';
        };
      }
    );
}
