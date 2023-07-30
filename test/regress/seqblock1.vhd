entity seqblock1 is
end entity;

architecture test of seqblock1 is
begin

    process is
        variable a : natural;
    begin
        for i in 1 to 100 loop
            a := i;
            block is
                constant c : natural := a + 1;
                variable a : integer := 99;
            begin
                wait for 1 ns;
                assert c = i + 1;
                assert a = 99;
            end block;
        end loop;
        wait;
    end process;

end architecture;
