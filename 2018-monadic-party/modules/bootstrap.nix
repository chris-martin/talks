{
    imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];

    config = {
        ec2.hvm = true;

        services.openssh.ports = [ 46861 ];

        networking.firewall.allowedTCPPorts = [ 46861 ];

        # Don't allow anyone to SSH using a password
        services.openssh.passwordAuthentication = false;

        # Let admins sudo without entering a password
        security.sudo.wheelNeedsPassword = false;

        # Let admins upload unsigned Nix packages
        nix.trustedUsers = [ "@wheel" ];

        users.users.chris = {
            isNormalUser = true;
            extraGroups = [ "wheel" ];

            openssh.authorizedKeys.keys = [
                "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFJ4UYsEh+JZQGCMdbNrKfjH1F3rwKBRewwgaehwnijBADYSJ8iwDZji09vVfCxSQSMjZJS54mEBtjcOBOpM7+mR585wI6jhsfdsNqNwzJdxV47Bi4jAkg7XlWf9IYv7EUhRzsKGdoSqefh/7bN6MPcJQ9ccHKqBxtmGJ6eHfgLmgnb8+ozwDlwQKz5QDtdEnt8TUqucUB4AOyReBV7GRnwkTyGCForb5nhTftuVi7GO1qApJKBIpYlC1gbuCWDX9CIl7IzfAMyng6u6Ty9x31ZWKA0sJzRIX5cw3e8Ct7sWzZB3O/2FOwjyYadqTRQdR472Dz/f6mqqIl1ioxzfXRfh33bREg2opLc6bnYaWTXY6aAc5/wUbC7z4CTKBGZJHxY5mrRSlpQ2Rn8EvgyyxgxokLdTZqoiKw/tSmE9Mlle5JGh+m8agGe41dszZxBf41j/ORE+N5p0k02fvUWuG0PL3aFE77qUbOgxxXOYMtBV0YiJPzeBXDGrkW1wqKC2voJ6PuCZWOHaLxDqkUDgAMYyGMKoj5C53OZneVeSMgZG+/lxygAduyBx/RfQYrt4WsPfPnhl95Kxx8PTYuFfLXmcMNMhZ7rYW+Thvo40W+VjiqTUSCxLHr16SFSOj2mGl0A29VPPHA3H+ckprCo8pldPo3AYrwkV/zHlyLjuEQfQ== renzo-1"
            ];
        };

        users.users.julie = {
            isNormalUser = true;
            extraGroups = [ "wheel" ];

            openssh.authorizedKeys.keys = [
                "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUA5QiRaMriKQrvA1H74UsJcnGieAcHYrHp27KBZjOhhKvinTkNyErY5JNFbgrCh3oC+6HeMSUOp+5qb2/hF0aO6kdYbGnYB3BchrbsPYFDccqW9eQONXdvYsa4mavfLzHIvXktkxVsd71CBRXqVFxxolYps99iJvFfhmgnn9Iz+zUIuOSRCPQ/Bcxbh3+fZHD2wlPpOZ352V3/utR6n6nlhK5W1Gq/mp5dmE32zCEOEse85xXQNxJpjdhIDZbu8PnNo2LuYIfRBFzIj/Gh6MIHJU3gQskt8MvKA+POdseflPLznxoDWYT3FqBO+agObr3FGnlnwpi1g9ym+U8T6SNFGUwR7cX5VkegLNSl7FTLMtaXoqUTKhBQH+ZIpNwlyepYnFo1BHR3IgsFDSaD/zLUjesBQIw6j+mtDCK9P3EnUcXo5v04OuGoyqTtAF4TTAz2kuC8ZsCs0cQEZRIoXqVIRcvPmlFr208o0SeGakCHVxIj6VnrQ+aHisVwuGAkq+Kb5mwE6b0xvuFGHo+bqHIUePymWQihbh0pZpYeSaJTLb2YG17HmE2rUFeO66CADmIMi2EWEI0xisT/e9FoqDcMnG/Z5sLBBDkerXwyfLA63zAkF0P3LtWX1Q0vYFhCn+VtMwE56qW/Z0sDdNiVwxn/a1GGh12gqXkNHBEePYjoQ== doriangray"
            ];
        };
    };
}
