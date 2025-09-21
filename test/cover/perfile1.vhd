entity sub is
    port ( i1 : in integer;
           o1 : out bit );
end entity;

architecture test of sub is
begin
    p1: process (i1) is
    begin
        if i1 > 10 then
            o1 <= '1';
        else
            o1 <= '0';
        end if;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin

    u1: entity work.sub port map ( 1, open );
    u2: entity work.sub port map ( 101, open );

end architecture;
