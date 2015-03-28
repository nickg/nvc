entity func13 is
end entity;

architecture test of func13 is
    signal five : integer := 5;
    signal zero : integer := 0;
begin

    process is
        variable x : integer;
        variable y : bit_vector(7 downto 0);

        impure function add_to_x(y : integer) return integer is

            impure function do_it return integer is
            begin
                report integer'image(x) & " + " & integer'image(y);
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
        assert add_to_x(five) = 7;
        x := 3;
        assert add_to_x(five) = 8;
        y := X"00";
        assert get_bit(zero) = '0';
        wait;
    end process;

end architecture;
