entity cond4 is
end entity;

architecture test of cond4 is
begin

    process is
    begin

    wait for 1 ns;

`if USER_DIRECTIVE = "VALID_VALUE" then
    wait for 1 ns;
`else
    report "FIRST DIRECTIVE WAS NOT DEFINED OK!"
    severity failure;
`end

    wait for 1 ns;
    wait;
    end process;

end architecture;
