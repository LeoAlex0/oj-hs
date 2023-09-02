{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "My personal online judge haskell solutions and tools";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      # ghcVersion = "ghc94";
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        # haskellPackages = prev.haskell.packages.${ghcVersion};
        oj-hs = final.haskellPackages.callCabal2nix "oj-hs" ./. { };
      });
      packages = forAllSystems (system: {
        oj-hs = nixpkgsFor.${system}.oj-hs;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.oj-hs);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.oj-hs ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
