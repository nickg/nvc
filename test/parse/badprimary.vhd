entity top is
end entity;

architecture a of top is

    component foo is
    end component;

begin

    sub_i: component foo;

end architecture;

-------------------------------------------------------------------------------

architecture bad of not_here is         -- Error
begin
end architecture;

-------------------------------------------------------------------------------

configuration cfg of top is
  for a
    for sub_i: foo
      use entity work.not_here(bad);
    end for;
  end for;
end configuration;
