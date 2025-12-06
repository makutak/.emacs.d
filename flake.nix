{
  description = "Emacs config";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
  let
    systems = [
      "x86_64-linux"   # Ubuntu
      "aarch64-darwin" # Apple Silicon Mac
    ];

    forAllSystems = f:
      builtins.listToAttrs (map (system: {
        name = system;
        value = f system;
      }) systems);
  in {
    devShells = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };

        emacsPkg = pkgs.emacs;
      in {
        default = pkgs.mkShell {
          buildInputs = [
            emacsPkg

            pkgs.git
            pkgs.ripgrep
            pkgs.jq
            pkgs.go
            pkgs.gopls
            pkgs.rustc
            pkgs.cargo
            pkgs.rust-analyzer
            pkgs.clang-tools
            pkgs.cscope
            pkgs.gnumake
            pkgs.ruff

            # org-roam-graph 用 (dot コマンド)
            pkgs.graphviz
          ];

          shellHook = ''
            echo "Nix Emacs shell for ${system}"
            echo "Emacs: ${emacsPkg}"
          '';
        };
      });

    # `nix run .#emacs` で Emacs を起動できるようにする
    apps = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        emacsPkg = pkgs.emacs;
      in {
        emacs = {
          type = "app";
          program = "${emacsPkg}/bin/emacs";
        };
      });
  };
}
