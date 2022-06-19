{
  description = "pletbjerg.github.io personal blog";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { self, nixpkgs }: 
    let sys = "x86_64-linux";
        pkgs = import nixpkgs { system = sys; };
        pkgName = "pletbjerg-github-io";
    in {
      packages.${sys}.${pkgName} = 
        pkgs.haskellPackages.callCabal2nix pkgName self {};

      defaultPackage.${sys} = self.packages.${sys}.${pkgName} ;

      # To open up a hoogle server, type `hoogle server --local`
      devShells.${sys}.default = pkgs.haskellPackages.shellFor {
        packages = _ : [ self.packages.${sys}.${pkgName} ];
        withHoogle = true;
        buildInputs = [ 
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.cabal-install
        ];
        shellHook = ''
            export PS1="\\e[1;34m[nix-develop] \\e[0m"$PS1
        '';
      };

    };
}
