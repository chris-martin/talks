{ pkgs, ... }:

{
    config = {
        systemd.services.party-either = {
            enable = true;
            description = "Monadic Party - Everything";
            wantedBy = ["multi-user.target"];
            requires = ["party-either.socket" ];

            environment = {
                LC_ALL = "en_US.UTF-8";
                LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            };

            serviceConfig = {
                User = "monadic-party";
                Restart = "on-failure";
                ExecStart = "${import ../monadic-party { inherit pkgs; }}/bin/party-either";
            };
        };

        systemd.sockets.party-either = {
            wantedBy = ["sockets.target"];
            socketConfig = {
                ListenStream = "/run/party-either.socket";
            };
        };
    };
}
