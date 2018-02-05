entity submodule is
  port (
    sig : in bit);
end entity;

architecture a of submodule is
begin
  main : process
  begin
    wait for 1 ns;
    assert sig = '1';
    report "Success";
    wait;
  end process;
end;

entity issue340 is
end entity;

architecture a of issue340 is
  signal sig_vector : bit_vector(0 to 1) := "00";
  alias sig_bit_alias : bit is sig_vector(0);

  signal sig : bit := '0';
  alias sig_alias : bit is sig;

  procedure drive(signal value : out bit) is
  begin
    value <= '1';
  end;
begin

  main : process
  begin
    drive(sig_alias);
    drive(sig_bit_alias);
    wait for 1 ns;
    assert sig_vector(0) = '1';
    assert sig = '1';
    assert sig_alias = '1';
    assert sig_bit_alias = '1';
    report "Success";
    wait;
  end process;

  submodule0_inst : entity work.submodule
    port map (
      sig => sig_alias);

  submodule1_inst : entity work.submodule
    port map (
      sig => sig_bit_alias);

  submodule2_inst : entity work.submodule
    port map (
      sig => sig);

  submodule3_inst : entity work.submodule
    port map (
      sig => sig_vector(0));
end;
