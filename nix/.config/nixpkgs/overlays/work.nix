self: super: {
    userPackages = super.userPackages or {} // {
        ansible = super.ansible_2_5;
        curl = super.curl;
        docker_compose = super.docker_compose;
        git = super.git;
        # slack = super.slack; # alsa issues
        vagrant = super.vagrant;
        vscode = super.vscode-with-extensions;
        # .override {
        #     vscodeExtensions =
        #         super.vscode-utils.extensionsFromVscodeMarketplace [
        #         {
        #             name = "csharp";
        #             publisher = "ms-vscode";
        #             version = "1.15.2";
        #             sha256 = "07iywxvrmdvfb2snszcia5w4j9c0dqgap9b485qyggxpq4wncncf";
        #         }
        #     ];
        # };

        # Language specific tools
        # In the future their will be specified at the project level (nix-shell)
        # .NET
        mono = super.mono;
        dotnet-sdk = super.dotnet-sdk;
        # Elm
        elm-make = super.elmPackages.elm-make;
        elm-package = super.elmPackages.elm-package;
        elm-reactor = super.elmPackages.elm-reactor;
        elm-repl = super.elmPackages.elm-repl;
        elm-format = super.elmPackages.elm-format.overrideAttrs (oldAttrs: {
            version = "0.6.1-alpha";
            src = super.fetchgit {
                url = "http://github.com/avh4/elm-format";
                sha256 = "0pypy8hfqn0vai5rwsidgcgf2n5k66gvgmp05p71r00xpj7gd2hg";
                rev = "24cbc66245289dd3ca5c08a14e86358dc039fcf3";
            };
        });
        elm-oracle = super.nodePackages.elm-oracle;
        elm-test = super.nodePackages.elm-test;
        # Haskell
        stack = super.stack; # broken in stack --docker
        hindent = super.haskellPackages.hindent;
        # Node.js
        nodejs = super.nodejs-6_x;
        tern = super.nodePackages.tern;
     };
}
