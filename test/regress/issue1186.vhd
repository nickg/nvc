use std.env.stop;

entity issue1186 is
end entity;

architecture tb of issue1186 is

  type std8  is array(natural range<>) of bit_vector( 7 downto 0);

  constant x : std8(0 to 3) := (0=>x"55", 1=>x"AA", 2=>x"55",3=>x"AA");

  function f_make(
    pyld       : std8 := (0=>x"00")) return std8
  is
    variable hdr        : std8(0 to 11) := (others=>(others=>'0'));
    -- variable hdr_pyld   : std8(0 to hdr'length+pyld'length-1) := hdr & pyld; also doesn't work
  begin
    return hdr & pyld;
    -- return hdr_pyld;
  end;

  procedure pkt_report(
    constant data    : in    std8) is
  begin
    for i in 0 to data'length-1 loop
      report to_string(data(i)) severity note;
    end loop;
  end;

begin

  test_runner : process
  begin
    pkt_report(f_make(x));
    stop;
  end process;

end architecture;
