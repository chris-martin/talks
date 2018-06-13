{ pkgs, ... }:

{
    config = {
        systemd.services.party-socket = {
            enable = true;
            description = "Monadic Party - Socket";
            wantedBy = ["multi-user.target"];
            requires = ["party-socket.socket"];

            environment = {
                LC_ALL = "en_US.UTF-8";
                LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            };

            serviceConfig = {
                User = "monadic-party";
                Restart = "on-failure";
                ExecStart = "${import ../monadic-party { inherit pkgs; }}/bin/party-socket";
            };
        };

        systemd.sockets.party-socket = {
            wantedBy = ["sockets.target"];
            socketConfig = {
                ListenStream = "/run/party-socket.socket";
            };
        };
    };
}
