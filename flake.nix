{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "My personal online judge haskell solutions and tools";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };
  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
      ];
      ghcVersion = "ghc96";
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }
      );
    in
    {
      overlay = (
        final: prev: {
          haskellPackages = prev.haskell.packages.${ghcVersion};
          oj-hs = final.haskellPackages.callCabal2nix "oj-hs" ./. { };
        }
      );
      packages = forAllSystems (system: {
        oj-hs = nixpkgsFor.${system}.oj-hs;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.oj-hs);
      checks = self.packages;
      devShells = forAllSystems (
        system:
        let
          haskellPackages = nixpkgsFor.${system}.haskellPackages;
          pkgs = nixpkgsFor.${system};
        in
        {
          default = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.oj-hs ];
            withHoogle = true;
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              stylish-haskell
              hlint
              hpack
              tasty-discover

              pkgs.git
              pkgs.gh
              pkgs.jq
            ];
            # Change the prompt to show that you are in a devShell
            shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
          };
          ci = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.oj-hs ];
            withHoogle = false;
            buildInputs = with haskellPackages; [
              cabal-install
              stylish-haskell
              hlint

              pkgs.git
            ];
          };
          ci-tools = pkgs.mkShell {
            packages = [
              pkgs.bash
              pkgs.coreutils
              pkgs.gawk
              pkgs.gh
              pkgs.jq
            ];
          };
        }
      );
      devShell = forAllSystems (system: self.devShells.${system}.default);
    };
}
