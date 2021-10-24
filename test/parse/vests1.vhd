entity something is
end entity;

architecture arch of something is
begin

end architecture;

configuration testbench of something is
    for arch
    end for;
end;

entity c01s03b01x00p12n01i00863ent is
end entity;

architecture c01s03b01x00p12n01i00863arch of c01s03b01x00p12n01i00863ent is
begin
  K:block
    component test
      port(
        sigin1  : in  boolean ;
        sigout1 : out boolean ;
        sigin2  : in  bit ;
        sigout2 : out bit ;
        sigin4  : in  severity_level ;
        sigout4 : out severity_level ;
        sigin5  : in  integer ;
        sigout5 : out integer ;
        sigin6  : in  real ;
        sigout6 : out real ;
        sigin7  : in  time ;
        sigout7 : out time ;
        sigin8  : in  natural ;
        sigout8 : out natural ;
        sigin9  : in  positive ;
        sigout9 : out positive
        );
    end component;

  BEGIN
      T5: component test;
      G: for i in 0 to 3 generate
          T1: component test;
      end generate;
  end block;
end architecture;

configuration c01s03b01x00p12n01i00863cfg of c01s03b01x00p12n01i00863ent is
  for c01s03b01x00p12n01i00863arch
    for K
      for T5:test use configuration work.testbench;
      end for;
      for G(3)
        for T1:test
          use configuration work.testbench;
        end for;
      end for;
      for G(0 to 2)
        for all:test
          use configuration work.testbench;
        end for;
      end for;
    end for;
  end for;
end;
