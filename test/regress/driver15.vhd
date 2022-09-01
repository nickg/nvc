entity driver15 is
end entity;

architecture test of driver15 is
    function resolved (x : bit_vector) return bit is
        variable r : bit := '0';
    begin
        for i in x'range loop
            r := r or x(i);
        end loop;
        report "result is " & bit'image(r);
        return r;
    end function;

    subtype rbit is resolved bit;

    signal s : rbit;
begin

    p1: s <= '0';

    p2: process is
    begin
        wait for 1 ns;
        s <= '1';
        wait;
    end process;

end architecture;
