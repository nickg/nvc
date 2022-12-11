entity cover9 is
end entity;

architecture test of cover9 is

    type t_mem is array (31 downto 0) of
        bit_vector(7 downto 0);
    signal mem   : t_mem;

    signal clk   : bit;
    signal we    : bit;
    signal wdata : bit_vector(7 downto 0);

begin

    mem_gen_row : for row in 0 to 31 generate
        mem_proc : process(clk)
        begin
            mem(row) <= wdata;
        end process;
    end generate;

end architecture;
