entity arrayop1 is
end entity;

architecture test of arrayop1 is
begin

    p1: process is
        variable x : bit_vector(1 to 3);
    begin
        assert x < "000";
        wait;
    end process;

end architecture;
