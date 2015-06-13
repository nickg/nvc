entity issue200 is
end entity;

architecture a of issue200 is
begin
  main : process
    -- Static error
    variable bv : bit_vector(-1 downto 0) := (others => '0');
  begin
    report integer'image(bv'length);
    wait;
  end process;
end architecture;
