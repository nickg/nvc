entity sub is
    generic ( G : integer );
end entity;

architecture test of sub is
    signal x : integer;
begin
    p: process is
    begin
        if x = 1 then
            report "one";
        elsif g = 2 then
            report "two";
        else
            report "other";
        end if;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity issue812 is
end entity;

architecture test of issue812 is
begin

    u: entity work.sub
        generic map ( 5 );

end architecture;
