{ inputs
, pkgs
, lib
, project
, utils
, ghc
,
}:

let
  allTools = {
    "ghc966".cabal = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    "ghc966".haskell-language-server =
      project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    "ghc966".stylish-haskell = project.projectVariants.ghc966.tool "stylish-haskell" "latest";
    "ghc966".fourmolu = project.projectVariants.ghc966.tool "fourmolu" "latest";
    "ghc966".hlint = project.projectVariants.ghc966.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  cardanoPackages =
    if pkgs.hostPlatform.isAarch64 then
      [ ]
    else
      let
        legacy = inputs.cardano-node.legacyPackages.${pkgs.system};
      in
      [
        legacy.cardano-node
        legacy.cardano-cli
      ];
  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [
          "--config"
          ".stylish-haskell.yaml"
        ];
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
        args = [
          "-m"
          "inplace"
        ];
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [
          "--hint"
          ".hlint.yaml"
        ];
      };
      shellcheck = {
        enable = true;
        package = pkgs.shellcheck;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.cabal-fmt
    tools.hlint

    pkgs.nixfmt-rfc-style
    pkgs.shellcheck
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.git-lfs
    pkgs.which
    pkgs.watchexec
    pkgs.ghciwatch
    pkgs.nix-prefetch-git

    # Additions for gen-tags.sh
    pkgs.haskellPackages.fast-tags
    pkgs.haskellPackages.hie-bios
    pkgs.jq

    # wbps and ZKsnark dependencies
    pkgs.nodejs_22
    pkgs.typescript
    pkgs.circom

    pkgs.figlet
  ];

  shell = project.shellFor {
    name = "my-project-shell-${project.args.compiler-nix-name}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
      cardanoPackages
    ];

    withHoogle = true;
    shellHook = ''
      ${preCommitCheck.shellHook}
      export TMPDIR=/tmp
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "

      if [ ! -d "node_modules/snarkjs" ]; then
        npm install snarkjs@0.7.0 --save-dev
      fi
      if [ ! -f .git/hooks/pre-push ] || ! grep -q "git-lfs" .git/hooks/pre-push 2>/dev/null; then
        git lfs install
      fi
      echo "cavefish" | figlet -f cybermedium

      # Make repo-local tooling available in the shell (including babyjubjub-keygen).
      repo_root=$PWD
      if [ -d "$repo_root/cavefish-server" ]; then
        repo_root="$repo_root/cavefish-server"
      fi

      export WBPS_TEST_INPUT_ROOT="$repo_root/wbps/setup"
      export WBPS_TEST_OUTPUT_ROOT="$repo_root/output/tests"

      if [ -d "$repo_root/wbps/setup" ]; then
        export PATH="$repo_root/wbps/setup:$PATH"
        export BABYJUBJUB_KEYGEN="$repo_root/wbps/setup/babyjubjub-keygen"
      fi

      if [ -d "$repo_root/node_modules/.bin" ]; then
        export PATH="$repo_root/node_modules/.bin:$PATH"
      fi
    '';
  };

in

shell
