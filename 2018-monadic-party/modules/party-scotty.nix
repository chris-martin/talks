{ pkgs, ... }:

{
    config = {
        systemd.services.party-scotty = {
            enable = true;
            description = "Monadic Party - Scotty";
            wantedBy = ["multi-user.target"];

            environment = {
                LC_ALL = "en_US.UTF-8";
                LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            };

            serviceConfig = {
                User = "monadic-party";
                Restart = "on-failure";
                ExecStart = "${import ../monadic-party { inherit pkgs; }}/bin/party-scotty";
            };
        };

    };
}
