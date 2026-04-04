package pkg_sig_alias_3 is
  generic (
    N : natural := 1
  );

  signal s : bit;
  alias a is s;
end package;

entity port_sink_4 is
  port (
    x : out bit
  );
end entity;

architecture test of port_sink_4 is
begin
end architecture;

entity top_alias_port_3 is
end entity;

architecture test of top_alias_port_3 is
  package p is new work.pkg_sig_alias_3;
begin
  u : entity work.port_sink_4
    port map (
      x => p.a
    );
end architecture;
