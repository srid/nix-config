{ pkgs, ... }:

{
  home.packages = with pkgs.haskellPackages; [

    # Some commonly used tools
    cachix
    pandoc
    hlint

    hoogle
    ormolu
    # haskell-language-server

    # For this shit: https://github.com/haskell/haskell-language-server/issues/171#issuecomment-647480950
    stack
  ];

  home.file = {
    # ghci
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
