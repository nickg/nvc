package pack is
    type unsigned is array (natural range <>) of bit;
end package;

use work.pack.all;

entity test2 is
  generic (
    SLAVE_ADDR : bit_vector(6 downto 0)
  );
end entity test2;
architecture beh of test2 is
signal addr_reg : bit_vector(6 downto 0) := (others => '0');
begin
    addr_reg <= SLAVE_ADDR;
end architecture beh;

use work.pack.all;

entity issue518 is
  generic (
    GC_SLAVE_ADDR : unsigned(6 downto 0) := "0101010"
  );
end entity;
architecture beh of issue518 is
begin
  i_test : entity work.test2
      generic map(
      SLAVE_ADDR => bit_vector(GC_SLAVE_ADDR)
    );

  process
  begin
    wait;
  end process;

end architecture;
