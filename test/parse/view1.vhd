entity view1 is end entity;

architecture rtl of view1 is
  type st_t is record
    data : bit_vector;
  end record;

  view st_source_v of st_t is
    data : out;
  end view;

  signal d : st_t(data(7 downto 0));
begin
  b1 : block
  port (s : view st_source_v(data(7 downto 0)));
  port map (s => d);
  begin
  end block;
end architecture;
