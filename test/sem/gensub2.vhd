package generic_ff_pack is
  procedure FF
    generic (
      type T)
    parameter (
      signal   q         : out   T;
      constant d         : in    T;
      constant INIT_VAL  : in    T;
      constant rst       : in    bit;
      signal   clk       : in    bit;
      constant en        : in    bit := '1');
end package generic_ff_pack;

package body generic_ff_pack is
  procedure FF
    generic (
      type T)
    parameter (
      signal   q         : out T;
      constant d         : in  T;
      constant INIT_VAL  : in  T;
      constant rst       : in  bit;
      signal   clk       : in  bit;
      constant en        : in  bit := '1') is
  begin
    if (clk'event and clk = '1') then
      if (rst /= '0') then
        q <= INIT_VAL;
      elsif (en = '1') then
        q <= d;
      end if;
    end if;
  end procedure FF;
end package body;

use work.generic_ff_pack.all;

entity generic_ff_tb is
  port (
    clkIn       : in    bit;
    rstIn       : in    bit;
    enIn        : in    bit;
    valIn       : in    bit_vector(7 downto 0);
    valOut      :   out bit_vector(7 downto 0));
end entity generic_ff_tb;

architecture behav of generic_ff_tb is
  procedure ff_byte is new FF generic map (T => bit_vector(7 downto 0));
  constant INIT_VAL : bit_vector(7 downto 0) := (others=>'0');
begin
  ff_byte(valOut, valIn, INIT_VAL, rstIn, clkIn, enIn);
end architecture behav;
