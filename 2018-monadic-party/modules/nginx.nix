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
                                    <h3>Haskell in the Cloud at the Monadic Party</h3>
                                    <ul>
                                        <li><a href="/slides">Slides</a>
                                        <li><a href="https://github.com/chris-martin/talks/tree/master/2018-monadic-party">Code</a>
                                    </ul>
                                    <p>Demos:</p>
                                    <ol>
                                        <li><a href="/scotty">Scotty</a>
                                        <li><a href="/socket">Activated socket</a>
                                        <li><a href="/either">Either port or socket</a>
                                        <li><a href="/count">Counter</a>
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

                locations."/count".proxyPass =
                    "http://unix:/run/party-count.socket";

            };
        };
    };
}
