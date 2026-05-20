package pack is
  type t_slvv is array(natural range <>) of bit_vector;
end package;


use work.pack.all;

entity inner is
  port (
    signal content : out t_slvv
  );
end entity;

architecture rtl of inner is
  constant WORDS : natural := 256;
  constant BITS  : natural := 32;

  signal ram : t_slvv(0 to WORDS - 1)(BITS - 1 downto 0) := (others => (others => '0'));
begin
  content <= ram;
end architecture;


use work.pack.all;

entity issue1543 is
end entity;

architecture rtl of issue1543 is
  signal content : t_slvv(4 to 259)(63 downto 32);
begin
  inner : entity work.inner
    port map (
      content => content
    );

  blk : block
    alias WORDS is << constant .issue1543.inner.WORDS : natural >>;
    alias BITS  is << constant .issue1543.inner.BITS  : natural >>;

    alias unconstrained is << signal .issue1543.inner.ram : t_slvv >>;
    alias constrained   is << signal .issue1543.inner.ram : t_slvv(0 to WORDS-1)(BITS-1 downto 0) >>;
--  alias error         is << signal .issue1543.inner.ram : t_slvv(0 to 256)(31 downto 0) >>;           -- => object RAM length 256 does not match external name subtype indication length 257

    constant WIDTH : natural := unconstrained'element'length;
  begin
    assert WORDS = 256 report "WORDS: " & to_string(WORDS);
    assert BITS = 32 report "BITS:  " & to_string(BITS);
    assert WIDTH = 32 report "WIDTH: " & to_string(WIDTH);
    assert unconstrained(7) = "00000000000000000000000000000000" report "ram(7)  = 0b" & to_string(unconstrained(7)) & " unconstrained";
    assert constrained(7) = "00000000000000000000000000000000" report "ram(7)     = 0b" & to_string(constrained(7))   & " constrained";

    postponed assert content(7) = "00000000000000000000000000000000" report "content(7) = 0b" & to_string(content(7))                severity note;
  end block;
end architecture;
