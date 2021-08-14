entity sub is
    generic (
        N : integer );
end entity;

architecture test of sub is
    signal x, y : integer;
begin

    p1: process is
    begin
        if N > 3 then
            x <= 5;
        else
            y <= 6;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity iffold is
end entity;

architecture test of iffold is
begin

    sub_i: entity work.sub
        generic map ( N => 4 );

end architecture;
