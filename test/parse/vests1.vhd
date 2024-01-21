entity something is
     port(
        sigin1  : in  boolean := false;
        sigout1 : out boolean ;
        sigin2  : in  bit := '0';
        sigout2 : out bit ;
        sigin4  : in  severity_level := note ;
        sigout4 : out severity_level ;
        sigin5  : in  integer := 0 ;
        sigout5 : out integer ;
        sigin6  : in  real := 0.0;
        sigout6 : out real ;
        sigin7  : in  time := 0 fs;
        sigout7 : out time ;
        sigin8  : in  natural := 0 ;
        sigout8 : out natural ;
        sigin9  : in  positive := 1 ;
        sigout9 : out positive
        );
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
        sigin1  : in  boolean := false;
        sigout1 : out boolean ;
        sigin2  : in  bit := '0';
        sigout2 : out bit ;
        sigin4  : in  severity_level := note ;
        sigout4 : out severity_level ;
        sigin5  : in  integer := 0 ;
        sigout5 : out integer ;
        sigin6  : in  real := 0.0;
        sigout6 : out real ;
        sigin7  : in  time := 0 fs;
        sigout7 : out time ;
        sigin8  : in  natural := 0 ;
        sigout8 : out natural ;
        sigin9  : in  positive := 1 ;
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
