library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity synopsys2 is
end entity;

architecture test of synopsys2 is
begin

    test_unsigned_plus: process is
        variable x, y : unsigned(15 downto 0);
        variable a, b : unsigned(0 to 15);
    begin
        x := X"0005";
        y := X"0001";
        assert x + y = 6;

        x := X"6005";
        y := X"0001";
        assert x + y(1 downto 0) = 24582;
        assert x(3 downto 0) + y(1 downto 0) = 6;
        assert x(5 downto 0) + y = 6;

        x := X"U000";
        assert x + y = std_logic_vector'(X"XXXX");

        x := X"ffff";
        y := X"0002";
        assert x + y = 1;

        a := X"001f";
        b := X"0010";
        assert a + b = 47;
        assert a + y = 33;

        assert a + '0' = a;
        assert '1' + a = 32;

        wait;
    end process;

    test_unsigned_minus: process is
        variable x, y : unsigned(15 downto 0);
        variable a, b : unsigned(0 to 15);
    begin
        x := X"0005";
        y := X"0001";
        assert x - y = 4;

        x := X"6005";
        y := X"0001";
        assert x - y(1 downto 0) = 24580;
        assert x(3 downto 0) - y(1 downto 0) = 4;
        assert x(5 downto 0) - y = 4;

        x := X"U000";
        assert x - y = std_logic_vector'(X"XXXX");

        x := X"0000";
        y := X"0002";
        assert x - y = 65534;

        a := X"001f";
        b := X"0010";
        assert a - b = 15;
        assert b - y = 14;

        wait;
    end process;

    test_unsigned_mul: process is
        variable x, y : unsigned(15 downto 0);
        variable a, b : unsigned(0 to 15);
    begin
        x := X"0005";
        y := X"0002";
        assert x * y = 10;

        x := 16d"0122";
        y := 16d"0005";
        assert x * y = 610;

        y := 16d"2512";
        assert x * y = std_logic_vector'(X"0004ad20");

        a := X"001f";
        b := X"0010";
        assert a * b = std_logic_vector'(X"000001f0");
        assert x * a(8 to 15) = std_logic_vector'(X"000ec6");

        wait;
    end process;

    test_signed_mul: process is
        variable x, y : signed(15 downto 0);
        variable a, b : signed(0 to 15);
    begin
        x := X"0005";
        y := X"0002";
        assert x * y = 10;

        x := 16d"0122";
        y := 16d"0005";
        assert x * y = 610;

        y := 16d"2512";
        assert x * y = std_logic_vector'(X"0004ad20");

        a := X"001f";
        b := X"0010";
        assert a * b = std_logic_vector'(X"000001f0");
        assert x * a(8 to 15) = std_logic_vector'(X"000ec6");

        x := X"0005";
        y := X"ffff";
        assert x * y = -5;

        wait;
    end process;

    test_signed_plus: process is
        variable x, y : signed(15 downto 0);
        variable a, b : signed(0 to 15);
    begin
        x := X"0005";
        y := X"0001";
        assert x + y = 6;

        x := X"6005";
        y := X"0001";
        assert x + y(1 downto 0) = 24582;
        assert x(3 downto 0) + y(1 downto 0) = 6;
        assert x(5 downto 0) + y = 6;

        x := X"U000";
        assert x + y = std_logic_vector'(X"XXXX");

        x := X"ffff";
        y := X"0002";
        assert x + y = 1;

        a := X"001f";
        b := X"0010";
        assert a + b = 47;
        assert a + y = 33;

        assert a + '0' = a;
        assert '1' + a = 32;

        wait;
    end process;

    test_unsigned_eql: process is
        variable x, y : unsigned(15 downto 0);
        variable a, b : unsigned(0 to 15);
    begin
        x := X"0005";
        y := X"0002";
        assert not (x = y);

        y := X"f005";
        assert not (x = y);
        assert x = y(7 downto 0);

        a := X"0005";
        b := X"0002";
        assert not (a = b);

        b := X"f005";
        assert not (a = b);
        assert a = b(8 to 15);

        wait;
    end process;

    test_signed_eql: process is
        variable x, y : signed(15 downto 0);
        variable a, b : signed(0 to 15);
    begin
        x := X"0005";
        y := X"0002";
        assert not (x = y);

        y := X"f005";
        assert not (x = y);
        assert x = y(7 downto 0);

        a := X"fff5";
        b := X"fff2";
        assert not (a = b);

        b := X"0ff5";
        assert not (a = b);
        assert a = b(8 to 15);

        wait;
    end process;

end architecture;
