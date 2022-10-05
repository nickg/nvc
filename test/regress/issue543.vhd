entity test2 is
  port(
    sig1 : in bit;
    sig2 : in bit_vector(2 downto 0)
    );
end entity test2;
architecture beh of test2 is
begin

  process is
  begin
    wait for 0 ns;
    assert sig1 = '1';
    assert sig2 = "010";
    wait;
  end process;
end architecture beh;

entity issue543 is
end entity issue543;
architecture beh of issue543 is
  type t_rec is record
    sig1 : bit;
    sig2 : bit_vector;
  end record;
  signal sig_if : t_rec(sig2(2 downto 0));
begin
  i_test2 : entity work.test2
  port map(
    sig1 => sig_if.sig1,
    sig2 => sig_if.sig2
  );
  process
  begin
    sig_if.sig1 <= '1';
    sig_if.sig2 <= "010";
    wait for 1 ns;
    report "OK";
    wait;
  end process;

end architecture beh;
