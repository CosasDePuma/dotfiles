{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }:
    let
      name = "dotfiles";
      forAllSystems = with nixpkgs.lib; genAttrs system.flakeExposed;
    in {
      packages = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; }; in {
          default = self.packages.${system}."${name}";
          "${name}" = pkgs.stdenvNoCC.mkDerivation {
            inherit name;
            src = ./.;

            dontBuild = true;
            dontConfigure = true;
            buildInputs = with pkgs; [ rsync ];

            installPhase = ''
              rsync -gortux --no-p    \
                --exclude ".git*"     \
                --exclude "flake.*"   \
                --exclude "LICENSE"   \
                --exclude "README.md" \
                ./ "$out"
            '';
          };
        });
    };
}