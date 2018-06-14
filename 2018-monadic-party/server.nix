let
    nixos = import <nixpkgs/nixos> {
        configuration = {

            imports = [

                # NixOS defaults for Amazon EC2
                <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>

                # Our minimal configuration for launching new instances
                ./modules/bootstrap.nix

                # The main web server
                ./modules/nginx.nix

                # Our Haskell servers
                ./modules/party-scotty.nix
                ./modules/party-socket.nix
                ./modules/party-either.nix
                ./modules/party-count.nix

            ];

            config = {
                networking.hostName = "monadic-party";
                nixpkgs.config.allowUnfree = true;
                users.users.monadic-party = {};
            };

        };
    };
in
    nixos.system
