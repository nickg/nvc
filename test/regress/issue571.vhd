package pack is
  type    sl2d_t is array(natural range <>, natural range <>) of bit;
  type    slv_7_0_t is array(natural range <>) of bit_vector(7 downto 0);

    constant size_log2 : integer := 15;

  subtype ram_bank_t is slv_7_0_t(0 to (2**size_log2) - 1);
  type ram_t is array(0 to 0) of ram_bank_t;

  constant ram_init : ram_t :=
    ( -- 4 banks...
      0 => ( -- 32768 bytes per bank...
        X"11",X"22",X"33",X"44",X"55",X"66",X"77",X"88",others => X"00"));

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity dpram is
    generic (
        width      : integer;
        depth_log2 : integer;
        init       : sl2d_t := (0 downto 1 => (0 downto 1 => '0')) );
end entity;

architecture test of dpram is
  subtype         ram_word_t is bit_vector(width-1 downto 0);
  type            ram_t is array(natural range <>) of ram_word_t;

  function ram_init return ram_t is
    variable r : ram_t(0 to (2**depth_log2)-1);
  begin
      r := (others => (others => '0'));
      if init'high = r'high then
      for i in 0 to r'length-1 loop
        for j in 0 to width-1 loop
          r(i)(j) := init(i, j);
        end loop;
      end loop;
    end if;
    return r;
  end function ram_init;

  shared variable ram : ram_t(0 to (2**depth_log2)-1) := ram_init;
begin

    check: process is
    begin
        assert ram(0) = X"11";
        assert ram(1) = X"22";
        assert ram(2) = X"33";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue571 is
end entity;

use work.pack.all;

architecture test of issue571 is

    function rambank2sl2d (constant x : ram_bank_t) return sl2d_t is
    variable r : sl2d_t(0 to (2**(size_log2))-1, 7 downto 0);
  begin
    for i in 0 to r'length-1 loop
      for j in 0 to 7 loop
        r(i,j) := x(i)(j);
      end loop;
    end loop;
    return r;
  end function rambank2sl2d;

begin

    g: for i in 0 to 0 generate

        u: entity work.dpram
            generic map (
                width => 8,
                depth_log2 => size_log2,
                init => rambank2sl2d(ram_init(i)) );

    end generate;

end architecture;
