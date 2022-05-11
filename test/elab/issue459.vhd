entity test is
    generic ( g : integer_vector );
  port(
    sig1 : bit_vector
  );
end entity test;
architecture beh of test is
begin
  g_gen : if sig1'length = 10 generate
    process
    begin
      wait;
    end process;
  end generate;

  g_gen2 : if g'length = 3 generate
    process
    begin
      wait;
    end process;
  end generate;
end architecture beh;

-------------------------------------------------------------------------------

entity issue459 is
end entity;

architecture test of issue459 is
    signal x : bit_vector(1 to 10);
begin

    u: entity work.test
        generic map ( (1, 2, 3 ) )
        port map ( x );

end architecture;
