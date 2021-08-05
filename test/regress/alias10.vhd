entity alias10 is
end entity;

package p is

    signal s : bit_vector(1 to 3);
    alias t : bit_vector(3 downto 1) is s;

end package;

use work.p.all;

architecture test of alias10 is

  function XSLL (ARG : BIT_VECTOR; COUNT : NATURAL)
    return BIT_VECTOR
  is
    constant ARG_L  : INTEGER      := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L downto COUNT) := XARG(ARG_L-COUNT downto 0);
    end if;
    return RESULT;
  end function XSLL;

begin

    process is
        variable v : bit_vector(3 downto 0);
    begin
        v := "0101";
        assert XSLL(v, 1) = "1010";
        wait for 1 ns;
        v := "0011";
        assert XSLL(v, 2) = "1100";

        s <= "001";
        wait for 1 ns;
        assert t = "001";

        t <= "101";
        wait for 1 ns;
        assert s = "101";

        wait;
    end process;

end architecture;
