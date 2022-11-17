entity operator6 is
end entity;

architecture test of operator6 is

  function "=" (L: bit; R: bit) return bit is
  begin
      report "custom bit =" severity failure;
    return L xnor R;
  end function "=";

  function "/=" (L: bit; R: bit) return bit is
  begin
      report "custom bit /=" severity failure;
    return L xor R;
  end function "/=";

  function "=" (L: bit_vector; R: bit_vector) return bit is
  begin
      report "custom bit_vector =" severity failure;
    if L = R then
      return '1';
    else
      return '0';
    end if;
  end function "=";

  function "/=" (L: bit_vector; R: bit_vector) return bit is
  begin
      report "custom bit_vector /=" severity failure;
    if L /= R then
      return '1';
    else
      return '0';
    end if;
  end function "/=";

  constant ID_FLAG_C_SHF : bit_vector(1 to 3) := "100";
  constant ID_RMW_SHF : bit := '1';

  -- ** Note: 3972750ns+1: Report Note: bad else: s1_id_flag_c = 4 s1_id_rmw = '0'

  signal s1_id_flag_c : bit_vector(1 to 3) := ID_FLAG_C_SHF;
  signal s1_id_rmw : bit := '0';

begin

    p1: process is
    begin
--        report "bad else: s1_id_flag_c = " & integer'image(to_integer(unsigned(s1_id_flag_c))) & " s1_id_rmw = " & bit'image(to_bit(s1_id_rmw));
        if s1_id_flag_c = ID_FLAG_C_SHF then
            report "s1_id_flag_c = ID_FLAG_C_SHF";
        end if;
        if s1_id_rmw /= ID_RMW_SHF then
            report "s1_id_rmw /= ID_RMW_SHF";
        end if;
        assert s1_id_flag_c = ID_FLAG_C_SHF and s1_id_rmw /= ID_RMW_SHF;
        wait;
    end process;

end architecture;
