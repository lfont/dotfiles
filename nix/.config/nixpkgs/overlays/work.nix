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
        elm = super.elmPackages.elm;
        elm-format = super.elmPackages.elm-format;
        elm-oracle = super.nodePackages.elm-oracle;
        # Haskell
        stack = super.stack; # broken in stack --docker
        hindent = super.haskellPackages.hindent;
        # Node.js
        nodejs = super.nodejs-6_x;
        tern = super.nodePackages.tern;
     };
}
