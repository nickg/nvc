package pack is
    generic ( g : integer );
    constant msg : string;
end package;

package body pack is
    constant msg : string := "hello";
end package body;

-------------------------------------------------------------------------------

entity sub is
    generic ( package p is new work.pack generic map (<>) );
    port ( o : out string );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        report p.msg;
        o <= p.msg;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity genpack16 is
end entity;

architecture test of genpack16 is
    package i is new work.pack generic map ( 5 );
    signal s : string(1 to 5);
begin

    u: entity work.sub
        generic map ( i )
        port map ( s );

    check: process is
    begin
        wait for 1 ns;
        assert s = "hello";
        wait;
    end process;

end architecture;
