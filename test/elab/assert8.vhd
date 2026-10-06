entity assert8_buf is
  port (i : in bit; o : out bit);
end entity;

architecture a of assert8_buf is
begin
  o <= i;
end architecture;

entity assert8 is
  generic (g : integer := 1);
  port (i : in bit; o : out bit);
end entity;

architecture a of assert8 is
  component assert8_buf is
    port (i : in bit; o : out bit);
  end component;

  function check_g return boolean is
  begin
    assert g < 10 report "G = " & integer'image(g) & " is too large"
      severity failure;
    return true;
  end function;

  constant g_ok : boolean := check_g;
begin
  buf_i : assert8_buf port map (i => i, o => o);
end architecture;
