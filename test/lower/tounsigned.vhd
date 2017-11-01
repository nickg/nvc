package p is
    type UNSIGNED is array (NATURAL range <>) of bit;

    function TO_UNSIGNED (ARG, SIZE: NATURAL) return UNSIGNED;
end package;

package body p is
    constant NAU: UNSIGNED(0 downto 1) := (others => '0');

  function TO_UNSIGNED (ARG, SIZE: NATURAL) return UNSIGNED is
    variable RESULT: UNSIGNED(SIZE-1 downto 0);
    variable I_VAL: NATURAL := ARG;
  begin
    if (SIZE < 1) then return NAU;
    end if;
    for I in 0 to RESULT'LEFT loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := '0';
      else RESULT(I) := '1';
      end if;
      I_VAL := I_VAL/2;
    end loop;
    return RESULT;
  end TO_UNSIGNED;
end package body;
