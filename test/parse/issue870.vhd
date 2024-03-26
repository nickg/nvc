entity ent is
generic (
    SEED1 : natural := 500
);
port (
    clk : in bit
);
end entity ent;

architecture arch of ent is
begin
    process(clk)
        variable seed1 : natural := SEED1;  -- OK (relaxed rules)
    begin
    end process;
end architecture arch;
