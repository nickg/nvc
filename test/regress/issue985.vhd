entity issue985 is
  generic (
    DATA_WIDTH: integer := 4;
    FIFO_SIZE: integer := 36
  );
end entity;

architecture sim of issue985 is
  function GetWidth (
    rdwr_width : in integer
    ) return integer is
    variable func_width : integer;
  begin
    return rdwr_width;
  end;
  constant mem_width : integer := GetWidth(DATA_WIDTH);
  type Two_D_array_type is array (1 downto 0) of bit_vector((mem_width - 1) downto 0);
  signal mem : Two_D_array_type;
begin
  process
  begin
    mem(0) <= (others => '1');
    wait for 1 ns;
    mem(0) <= (others => '0');
    mem(1) <= (0 => '1', others => '0');
    wait for 1 ns;
    std.env.finish;
  end process;
end architecture;
