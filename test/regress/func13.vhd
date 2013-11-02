entity func13 is
end entity;

architecture test of func13 is
begin

    process is
        variable x : integer;
        variable y : bit_vector(7 downto 0);

        impure function add_to_x(y : integer) return integer is

            impure function do_it return integer is
            begin
                return x + y;
            end function;

        begin
            return do_it;
        end function;

        impure function get_bit(n : integer) return bit is
        begin
            return y(n);
        end function;

    begin
        x := 2;
        assert add_to_x(5) = 7;
        x := 3;
        assert add_to_x(5) = 8;
        y := X"00";
        assert get_bit(0) = '0';
        wait;
    end process;

end architecture;
