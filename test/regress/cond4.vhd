entity cond4 is
end entity;

architecture test of cond4 is
begin

    process is
    begin

    wait for 1 ns;

`if USER_DIRECTIVE = "VALID_VALUE" then
    report "DIRECTIVE WAS DEFINED OK!";
`end

    wait for 1 ns;
    wait;
    end process;

end architecture;
