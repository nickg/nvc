package RandomBasePkg is
  subtype NULL_RANGE_TYPE is integer range 0 downto 1 ;
  type RandomSeedType is array (1 to 2) of integer ;

  procedure Uniform (Result : out real ;  Seed : inout RandomSeedType) ;

  type RandomDistType is (NONE, UNIFORM, FAVOR_SMALL, FAVOR_BIG, NORMAL, POISSON) ;

  type RandomParmType is record
    Distribution : RandomDistType ;
    Mean         : Real ; -- also used as probability of success
    StdDeviation : Real ; -- also used as number of trials for binomial
  end record ;

  function Scale (A, Min, Max : real) return real ;
  function Scale (A : real ; Min, Max : integer) return integer ;
end RandomBasePkg ;

-------------------------------------------------------------------------------

use work.RandomBasePkg.all ;

package RandomPkg is
  type NaturalVBoolType is array (boolean range <>) of natural;
  type NaturalVBitType  is array (bit range <>) of natural;

  type RandomPType is protected
    impure function Uniform (Min, Max : in real) return real ;
    impure function Uniform (Min, Max : integer) return integer ;
    impure function Uniform (Min, Max : integer ; Exclude : integer_vector) return integer ;
  end protected RandomPType ;

end RandomPkg ;

package body RandomPkg is

  type RandomPType is protected body
    impure function Uniform return real;

    impure function Uniform return real is
      variable rRandom : real ;
    begin
      return rRandom ;
    end function Uniform ;

    impure function LocalUniform (Min, Max : in real) return real is
    begin
      return scale(Uniform, Min, Max) ;  -- OK
    end function LocalUniform ;

  end protected body RandomPType ;

end RandomPkg ;
