{pkgs, ...}: {
  # name = "project-name";
  
  # We use the latest supported and cached version
  # from github:input-output-hk/haskell.nix
  compiler-nix-name = "ghc901";

  # Enable for cross-platform build
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
