entity condexpr is
end entity;

architecture test of condexpr is
    constant c0 : integer := 5;
    constant c1 : integer := 1 when c0 > 2 else 2;  -- OK
    constant c2 : integer := 1 when 1 else 1;  -- Error
    constant c3 : integer := 1 when true else 1.0;  -- Error
    signal s1 : bit := '1' when c2 > 4 else '0';  -- OK
begin

    process is
        variable v1 : real := 1.0 when c0 = 2 else 5.15;  -- OK
    begin
        v1 := 1.0 when c2 > 2 else unaffected when c2 < 10
              else unaffected;          -- OK
        wait;
    end process;

    process is
        function foo (x : in integer) return integer is
        begin
            return 5 when x > 2 else 3;  -- OK
            return 5.0 when x > 2 else 3.4;  -- Error
            return when x < 2;          -- Error
        end function;

        procedure bar (x : out integer) is
        begin
            return when c0 > 2;         -- OK
            return when c0;             -- Error
        end procedure;
    begin
        return when s1 = '1';           -- Error
    end process;

end architecture;
