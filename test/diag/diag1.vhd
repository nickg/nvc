entity diag1 is
end entity;

architecture test of diag1 is
    ---------------------------------------------------------------------------
    -- All three of the following lines should appear in the error message
    signal x : integer;
    constant k : integer := 2;
    signal x : integer;                 -- Error

    ---------------------------------------------------------------------------
    -- There should be a gap between the two error lines here
    signal y : integer;
    -- One
    -- Two
    -- Three
    -- Four
    signal y : integer;                 -- Error

    /* Requires a later standard */

    ---------------------------------------------------------------------------
    constant b : boolean := '1' = '1';   -- Error
begin

end architecture;
