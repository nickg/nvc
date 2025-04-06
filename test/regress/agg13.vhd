entity agg13 is
end entity;

architecture test of agg13 is
  impure function color_start return string is
  begin
      return ('[',
              integer'image(1), ';',
              integer'image(2), ';',
              integer'image(3), 'm');
  end;

begin

    process is
    begin
        report color_start;
        assert color_start = "[1;2;3m";
        wait;
    end process;

end architecture;
