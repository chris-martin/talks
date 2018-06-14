{ pkgs, ... }:
{
    config = {

        networking.firewall.allowedTCPPorts = [ 80 443 ];

        services.nginx = {
            enable = true;

            recommendedGzipSettings = true;
            recommendedOptimisation = true;
            recommendedProxySettings = true;
            recommendedTlsSettings = true;

            virtualHosts."monadic-party.chris-martin.org" = {

                # Enable SSL magically by automatically getting
                # an SSL certificate from Let's Encrypt
                enableACME = true;
                addSSL = true;

                locations."/".index = "index.html";
                locations."/".root = pkgs.runCommand "nginx-root" {}
                    ''
                        mkdir $out

                        ln -s ${pkgs.writeText "index.html" ''
                            <!doctype html>
                            <html>
                                <head></head>
                                <body>
                                    <h1>Haskell in the Cloud at the Monadic Party</h1>
                                    <p><a href="/slides">Slides</a></p>
                                    <p>Demos:</p>
                                    <ol>
                                        <li><a href="/scotty">Scotty</a></li>
                                        <li><a href="/socket">Activated socket</a></li>
                                        <li><a href="/either">Either port or socket</a></li>
                                    </ol>
                                </body>
                            </html>
                        ''} $out/index.html
                    '';

                locations."/slides".index = "index.html";
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
