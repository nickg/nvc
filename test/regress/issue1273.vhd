entity issue1273 is
end entity;

architecture tb of issue1273 is

  type t_int_record is record
    im : integer;
  end record;

  type t_int_record_uarray is array (natural range <>) of t_int_record;

  type t_int_record_uarray_array is array(natural range 10 downto 0) of t_int_record_uarray;

  type t_test_signals is record
    fwd_bx_dc_fine_nco_phasor : t_int_record_uarray_array;
  end record;

  signal test_signals : t_test_signals(
    fwd_bx_dc_fine_nco_phasor(open)(1 downto 0)
  );

begin

    process is
    begin
        assert test_signals.fwd_bx_dc_fine_nco_phasor'left = 10;
        assert test_signals.fwd_bx_dc_fine_nco_phasor'right = 0;
        assert test_signals.fwd_bx_dc_fine_nco_phasor'element'left = 1;
        assert test_signals.fwd_bx_dc_fine_nco_phasor'element'right = 0;

        test_signals.fwd_bx_dc_fine_nco_phasor(5)(1) <= (im => 1);
        wait for 1 ns;
        assert test_signals.fwd_bx_dc_fine_nco_phasor(5)(1).im = 1;

        wait;
    end process;
end architecture;
