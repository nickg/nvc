entity pcall2 is
end entity;

architecture test of pcall2 is
    signal x, y, z, s : bit_vector(1 to 3);

    procedure proc2(signal x : out bit_vector) is
    begin
        x <= "00";
    end procedure;
begin

    p1: process is
        procedure proc1 is
        begin
            x(3) <= '1';
        end procedure;
    begin
        proc1;
        wait;
    end process;

    p2: process is
    begin
        proc2(y(1 to 2));
        wait;
    end process;

    p3: process is
        procedure proc3(x : in integer) is
        begin
            z(x) <= '1';
        end procedure;
    begin
        proc3(2);
    end process;

    p4: process is
        procedure proc4(signal ss : out bit_vector) is
        begin
            ss(1) <= '1';
        end procedure;
    begin
        proc4(s);
    end process;

end architecture;
