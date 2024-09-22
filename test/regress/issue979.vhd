entity foo is
  port (
    rd_data : out bit
  );
end entity foo;

architecture rtl of foo is
begin
  rd_data <= '0';
end architecture;

entity issue979 is
end entity issue979;

architecture tb of issue979 is
  signal wrb_data : bit;
begin
  process
  begin
    std.env.finish;
  end process;

  foo_inst: entity work.foo
  port map (
    rd_data => wrb_data
  );
end architecture tb;
