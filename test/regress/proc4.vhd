entity proc4 is
end entity;

architecture rtl of proc4 is

    procedure test1(x : inout integer) is
    begin
        x := x + 1;
    end procedure;

    procedure test2(x : inout bit_vector) is
    begin
        for i in x'range loop
            x(i) := not x(i);
        end loop;
    end procedure;

    procedure test3(x : inout bit_vector(3 downto 0)) is
    begin
        for i in 3 downto 0 loop
            x(i) := not x(i);
        end loop;
    end procedure;

    procedure test4(signal x : inout integer) is
    begin
        x <= x + 1;
    end procedure;

    procedure test5(signal x : inout bit_vector; l : out integer) is
    begin
        l := x'length;
        for i in x'range loop
            x(i) <= not x(i);
        end loop;
    end procedure;

    signal s : integer;
    signal k : bit_vector(1 downto 0);

begin

    process is
        variable v : integer;
        variable b : bit_vector(3 downto 0);
    begin
        k <= "10";
        v := 5;
        test1(v);
        assert v = 6;
        b := "1010";
        test2(b);
        assert b = "0101";
        test3(b);
        assert b = "1010";
        s <= 5;
        wait for 1 ns;
        test4(s);
        wait for 1 ns;
        assert s = 6;
        test5(k, v);
        wait for 1 ns;
        assert v = 2;
        assert k = "01";
        wait;
    end process;

end architecture;
