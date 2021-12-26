package RandomPkg is

  -- Weight vectors not indexed by integers
  type NaturalVBoolType is array (boolean range <>) of natural;
  type NaturalVBitType  is array (bit range <>) of natural;

  function Scale (A : real ; Min, Max : integer) return integer ;
  function Scale (A, Min, Max : real) return real ;

  --- ///////////////////////////////////////////////////////////////////////////
  --- ///////////////////////////////////////////////////////////////////////////
  ---
  ---  RandomPType
  ---
  --- ///////////////////////////////////////////////////////////////////////////
  --- ///////////////////////////////////////////////////////////////////////////
  type RandomPType is protected

    ------------------------------------------------------------
    --
    -- Uniform
    -- Generate a random number with a Uniform distribution
    --
    ------------------------------------------------------------
    impure function Uniform (Min, Max : in real) return real ;
    impure function Uniform (Min, Max : integer) return integer ;
    impure function Uniform (Min, Max : integer ; Exclude : integer_vector) return integer ;

  end protected RandomPType ;

end RandomPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body RandomPkg is

  --- ///////////////////////////////////////////////////////////////////////////
  --- RandomPType Body
  --- ///////////////////////////////////////////////////////////////////////////
  type RandomPType is protected body

    --- ///////////////////////////////////////////////////////////////////////////
    ---
    --- Base Call to Uniform.   Use this one rather than RandomBasePkg
    ---
    --- ///////////////////////////////////////////////////////////////////////////
    -----------------------------------------------------------------
    impure function Uniform return real is
    -----------------------------------------------------------------
      variable rRandom : real ;
    begin
      return rRandom ;
    end function Uniform ;


    --- ///////////////////////////////////////////////////////////////////////////
    ---
    --- Base Randomization Distributions
    ---
    --- ///////////////////////////////////////////////////////////////////////////
    ------------------------------------------------------------
    --
    -- Uniform
    -- Generate a random number with a Uniform distribution
    --
    ------------------------------------------------------------
    impure function LocalUniform (Min, Max : in real) return real is
    ------------------------------------------------------------
    begin
      return scale(Uniform, Min, Max) ;  -- No error here
    end function LocalUniform ;

  end protected body RandomPType ;

end RandomPkg ;
