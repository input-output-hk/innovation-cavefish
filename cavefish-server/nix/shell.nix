{
  inputs,
  pkgs,
  lib,
  project,
  utils,
  ghc,
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

    "ghc984".cabal = project.projectVariants.ghc984.tool "cabal" "latest";
    "ghc984".cabal-fmt = project.projectVariants.ghc984.tool "cabal-fmt" "latest";
    "ghc984".haskell-language-server =
      project.projectVariants.ghc984.tool "haskell-language-server" "latest";
    "ghc984".stylish-haskell = project.projectVariants.ghc984.tool "stylish-haskell" "latest";
    "ghc984".fourmolu = project.projectVariants.ghc984.tool "fourmolu" "latest";
    "ghc984".hlint = project.projectVariants.ghc984.tool "hlint" "latest";

    "ghc9102".cabal = project.projectVariants.ghc9102.tool "cabal" "latest";
    "ghc9102".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest"; # cabal-fmt not buildable with ghc9102
    "ghc9102".haskell-language-server =
      project.projectVariants.ghc9102.tool "haskell-language-server" "latest";
    "ghc9102".stylish-haskell = project.projectVariants.ghc9102.tool "stylish-haskell" "latest";
    "ghc9102".fourmolu = project.projectVariants.ghc9102.tool "fourmolu" "latest";
    "ghc9102".hlint = project.projectVariants.ghc9102.tool "hlint" "latest";

    "ghc9122".cabal = project.projectVariants.ghc9122.tool "cabal" "latest";
    "ghc9122".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest"; # cabal-fmt not buildable with ghc9122
    "ghc9122".haskell-language-server =
      project.projectVariants.ghc9122.tool "haskell-language-server" "latest";
    "ghc9122".stylish-haskell = project.projectVariants.ghc9122.tool "stylish-haskell" "latest";
    "ghc9122".fourmolu = project.projectVariants.ghc9122.tool "fourmolu" "latest";
    "ghc9122".hlint = project.projectVariants.ghc9122.tool "hlint" "latest";
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
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = false;
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
        enable = false;
        package = tools.fourmolu;
        args = [
          "--mode"
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
        enable = false;
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
    pkgs.git-lfs
    pkgs.which
    pkgs.watchexec
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
      alias ll="ls -l --color=auto"
      alias haskell-language-server-wrapper="haskell-language-server"

      if [ ! -d "node_modules/snarkjs" ]; then
        npm install snarkjs@0.7.0 --save-dev
      fi
      if [ ! -f .git/hooks/pre-push ] || ! grep -q "git-lfs" .git/hooks/pre-push 2>/dev/null; then
        git lfs install
      fi
      echo "cavefish" | figlet -f cybermedium

      export PATH=$(pwd)/node_modules/.bin:$PATH
    '';
  };

in

shell
