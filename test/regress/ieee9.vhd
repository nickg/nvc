-- https://github.com/ghdl/ghdl/issues/2097

library IEEE;
context IEEE.IEEE_std_context;

package my_fixed_pkg is new IEEE.fixed_generic_pkg;

--!

library IEEE;
context IEEE.IEEE_std_context;

library work;
use work.my_fixed_pkg.all;

entity ieee9 is
end;

architecture arch of ieee9 is

begin

  process
    subtype stype is sfixed(7 downto -8);

    -- Subtype not allowed as size_res argument of to_sfixed:
    -- constant input  : std_logic_vector (stype'length-1 downto 0) := to_slv(to_sfixed(-9.96484375, stype));
    -- Therefore, a variable needs to be created:
    variable fmt : stype;
    constant input  : std_logic_vector (stype'length-1 downto 0) := to_slv(to_sfixed(-9.96484375, fmt));

    variable sfmt : fmt'subtype;

    procedure report_sfixed(arg: sfixed) is begin report to_string(to_real(arg)); end procedure;

  begin
    report_sfixed(stype(input));
    report_sfixed(stype(signed(input)));

    -- CRASH
    report_sfixed(fmt'subtype(input));
    -- CRASH
    report_sfixed(fmt'subtype(signed(input)));

    -- CRASH
    --report to_string(fmt'subtype);

    -- However, sfmt, which is declared using fmt'subtype, does work
    sfmt := stype(input);
    report_sfixed(sfmt);

    wait;
  end process;

end;
