{
  # This is a template created by `hix init`
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.utils.url = "github:ursi/flake-utils";
  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        # TODO support additional systems
        #  right now we can't afford to test every other system
        systems = [ "x86_64-linux" "aarch64-linux" ];
        overlays = [ inputs.haskell-nix.overlay ];
      }
      ({ pkgs, system, ... }:
        let
          hixProject = pkgs.haskell-nix.hix.project {
            src = ./.;
            evalSystem = "x86_64-linux";
          };
          flake = hixProject.flake { };
        in
        # Flake definition follows hello.cabal
        flake // {
          legacyPackages = pkgs;
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
    allow-import-from-derivation = "true";
  };

}
