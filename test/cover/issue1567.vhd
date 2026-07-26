entity sub is
    generic ( enabled : boolean );
    port ( i : in bit; o : out bit );
end entity;

architecture test of sub is
begin
    process (i) is
    begin
        if enabled then
            o <= not i;
        end if;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin
    u1: entity work.sub generic map ( false ) port map ( '0', open );
    u2: entity work.sub generic map ( true ) port map ( '0', open );
end architecture;
