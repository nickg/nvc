entity pragma is
end entity;

-- lint_on hello
architecture test of pragma is
    signal x : integer;
begin

    process is
    begin
        -- lint_off foo bar
        x <= 5;
        wait;
    end process;

end architecture;
