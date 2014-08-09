entity dyn_agg is
end entity;

architecture test of dyn_agg is

    constant WIDTH : integer := 20;
    constant ITERS : integer := 14;

    signal s : bit_vector(WIDTH - 1 downto 0);

    function func(constant W : integer) return bit_vector is
        variable r : bit_vector(W - 1 downto 0);
    begin
        return r;
    end function;
begin

    process is
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s <= func(WIDTH);
            end loop;
        end loop;
        wait;
    end process;

end architecture;
