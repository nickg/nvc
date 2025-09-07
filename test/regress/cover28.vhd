entity cover28 is
end entity;

architecture test of cover28 is

begin

-- coverage off
gen_1 : for i in 0 to 0 generate
begin

    process
    begin
        wait for 1 ns;
        report "Dummy statement";
        wait for 1 ns;
        wait;
    end process;

end generate;
-- coverage on

end architecture;