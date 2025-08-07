{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  packages = with pkgs; [
    haskell.compiler.ghc912
    cabal-install
    (haskell-language-server.override { supportedGhcVersions = [ "912" ]; })
    # (haskellPackages.haskell-debug-adapter.override { supportedGhcVersions = [ "912" ]; })
    # (haskellPackages.haskell-dap.override { supportedGhcVersions = [ "912" ]; })
    # (haskellPackages.ghci-dap.override { supportedGhcVersions = [ "912" ]; })
  ];
  shellHook = ''
    echo Hello Haskell!
  '';
  TMPDIR = "/tmp";
}
