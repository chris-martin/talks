{ pkgs, ... }:

{
    config = {
        systemd.services.party-count = {
            enable = true;
            description = "Monadic Party - Count";
            wantedBy = ["multi-user.target"];
            requires = ["party-either.socket" ];

            environment = {
                LC_ALL = "en_US.UTF-8";
                LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            };

            serviceConfig = {
                User = "monadic-party";
                Restart = "on-failure";
                ExecStart = "${import ../monadic-party { inherit pkgs; }}/bin/party-count";
            };
        };

        systemd.sockets.party-count = {
            wantedBy = ["sockets.target"];
            socketConfig = {
                ListenStream = "/run/party-count.socket";
            };
        };
    };
}
