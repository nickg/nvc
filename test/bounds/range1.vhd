entity range1 is
end entity;

architecture test of range1 is
begin

    p1: process is
        variable x : bit_vector(1 to 10);
    begin
        assert x'range(2+0)'left = 5;     -- Error
        assert x'reverse_range(-1)'right = 6;  -- Error
        wait;
    end process;

end architecture;
