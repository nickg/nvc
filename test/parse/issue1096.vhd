entity ent is
  generic (A: integer := 0);
end entity ent;

--------------------------------------------------------------------------------

entity top is
end entity top;

architecture arch of top is
  component ent is
    generic (A: integer := 1);
  end component ent;
  for inst : ent use entity work.ent generic map (A => 2);
begin
  inst : component ent generic map (A => 3);
end architecture arch;

--------------------------------------------------------------------------------

configuration conf of top is
  for arch
    for others : ent                    -- OK (not yet supported)
      generic map (A => 4);
    end for;
  end for;
end configuration conf;

