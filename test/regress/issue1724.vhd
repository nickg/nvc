entity issue1724 is
end entity;

architecture test of issue1724 is
begin
    g: for i in 0 to 1 generate
        signal s : integer := i;
    begin
        process is
            alias a is <<signal s : integer>>;
        begin
            wait for 1 ns;
            assert a = i;
            wait;
        end process;
    end generate;
end architecture;
