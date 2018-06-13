{ pkgs, ... }:
{
    config = {

        networking.firewall.allowedTCPPorts = [ 80 443 ];

        services.nginx = {
            enable = true;

            #recommendedGzipSettings = true;
            #recommendedOptimisation = true;
            #recommendedProxySettings = true;
            #recommendedTlsSettings = true;

            virtualHosts."monadic-party.chris-martin.org" = {

                # Enable SSL magically by automatically getting
                # an SSL certificate from Let's Encrypt
                enableACME = true;

                locations."/slides".root =
                    import ../slides { inherit pkgs; };

                locations."/scotty".proxyPass =
                    "http://localhost:8000";

                locations."/socket".proxyPass =
                    "http://unix:/run/party-socket.socket";

                locations."/either".proxyPass =
                    "http://unix:/run/party-either.socket";

            };
        };
    };
}
