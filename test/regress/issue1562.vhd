entity issue1562 is
end entity;

architecture a of issue1562 is
  type RecType is record
    Valid : bit;
    Ready : bit;
    Resp  : bit_vector(1 downto 0);
    User  : bit_vector;
    ID    : bit_vector;
  end record;

  signal Axi   : RecType(ID(5 downto 0), User(-1 downto 0));

  function pop return bit_vector is
    variable result : bit_vector(8 downto 0) := (others => '0');
  begin
    return result;
  end function;
begin
  WrRespHandler: process
    variable Local : Axi'subtype;
  begin
    (Local.Resp, Local.ID, Local.User) := pop;
    wait;
  end process;
end architecture;
