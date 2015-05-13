entity ent is
  generic (
      config : string := "config0";
      bits   : bit_vector := "10101" );
end entity;

architecture a of ent is
  signal sig : integer;
begin
  gen_cfg1 : if config = "config1" generate
    bad: sig <= 0;
  end generate;

  gen_cfg2 : if bits /= "00000" generate
    good: sig <= 1;
  end generate;
end architecture;
