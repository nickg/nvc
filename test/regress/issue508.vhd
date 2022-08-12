entity issue508 is
end entity;
architecture beh of issue508 is
  type t_slv_array is array (natural range <>) of bit_vector;
  signal sig1 : t_slv_array(1 downto 0)(2 downto 0);

  type rec is record
    x, y : integer;
  end record;
  type recv is array (natural range <>) of rec;
  type recvv is array (natural range <>) of recv;

  function el_len(sig : t_slv_array) return natural is
  begin
    return sig'element'length;
  end function;

  function el_len(sig : recvv) return natural is
  begin
    return sig'element'length;
  end function;

begin

  process
    variable v : t_slv_array(1 to 2)(5 to 6);
    variable vv : recvv(1 to 3)(1 to 7);
  begin
    assert el_len(sig1) = 3 severity failure;
    assert el_len(v) = 2 severity failure;
    assert el_len(vv) = 7 severity failure;
    wait;
  end process;

end architecture beh;
