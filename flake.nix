{
  description = "Semantic Web with Prolog";

  inputs = {
    nixpkgs24.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs_unstable.url = "github:nixos/nixpkgs/1750f3c1c89488e2ffdd47cab9d05454dddfb734";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs24, nixpkgs_unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs24 = nixpkgs24.legacyPackages.${system};
        pkgs_unstable = nixpkgs_unstable.legacyPackages.${system};
        defaultShellHook = { shellName, after_shell_hook ? "", ... }: ''
            export PS1="\n\[\033[1;32m\][${shellName}: \w]\n\$\[\033[0m\] "
            export PURE="$([[ $IN_NIX_SHELL = 'pure' ]] && echo 1 || echo 0)"
            echo "PURE=$PURE"
            echo -n '>> Welcome to ${shellName}! <<'
          '' + after_shell_hook;
        myShell = input@{
            name,
            buildInputs ? [],
            toShellName ? n: "${n}-shell",
            shellHook ? defaultShellHook,
            after_shell_hook ? "",
          }: pkgs24.mkShell {
            name = toShellName name;

            buildInputs = buildInputs;

            shellHook = shellHook
              (input // {
                inherit defaultShellHook;
                shellName = toShellName name;
              });
          };
      in {
        devShells = {
          default = self.devShells.${system}.typst;

          typst = myShell {
            name = "typst";
            buildInputs = with pkgs24; [
              pkgs_unstable.typst
              newcomputermodern

              (aspellWithDicts (dicts: with dicts; [
                pt_BR
                en
              ]))

              poppler_utils
              zathura
            ];
            after_shell_hook = ''
              export TYPST_FONT_PATHS=${pkgs24.newcomputermodern.out}/share/fonts/
            '';
          };
        };
      });
}
