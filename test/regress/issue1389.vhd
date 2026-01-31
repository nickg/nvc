entity sub is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of sub is
    function func (x : integer) return integer is
    begin
        if x > 0 then
            return x * 2;
        else
            return 0;
        end if;
    end function;
begin
    o <= func(i);
end architecture;

-------------------------------------------------------------------------------

entity issue1389 is
end entity;

architecture test of issue1389 is
    signal i, o1, o2 : integer;
begin

    u1: entity work.sub                 -- Enabled in spec file
        port map ( i, o1 );

    u2: entity work.sub                 -- Disabled in spec file
        port map ( i, o2 );

    i <= 5 after 1 ns;

end architecture;
