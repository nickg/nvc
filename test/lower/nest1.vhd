entity nest1 is
end entity;

architecture test of nest1 is
begin

    line_7: process is
        variable x : integer := 2;
        variable y : bit_vector(7 downto 0);

        impure function add_to_x(y : integer) return integer is

            impure function do_it return integer is
            begin
                return x + y;
            end function;

        begin
            return do_it;
        end function;

    begin
        assert add_to_x(5) = 7;
        wait;
    end process;

end architecture;
