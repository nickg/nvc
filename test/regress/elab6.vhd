entity elab6 is
end entity;

architecture test of elab6 is

    type int_vec is array (integer range <>) of integer;

    signal s : int_vec(1 to 10);

begin

    test_g: for i in s'range generate
        signal tmp : integer;
    begin
        tmp <= i * 2;
        s(i) <= tmp;
    end generate;

    process is
    begin
        wait for 1 ns;
        for i in s'range loop
            assert s(i) = i * 2;
        end loop;
        wait;
    end process;

end architecture;
