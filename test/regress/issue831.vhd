-- fft_types --------------------------------------------------------------------------
library ieee;
use ieee.math_complex.all;
use std.textio.all;

package fft_types is
  subtype fft_data_type is complex;
  type fft_data_vector is array (natural range <>) of fft_data_type;
  pure function "+" (a, b : fft_data_vector) return fft_data_vector;
  pure function "*" (a, b : fft_data_vector) return fft_data_vector;
end package fft_types;

package body fft_types is
  pure function "+" (a, b : fft_data_vector) return fft_data_vector is
    variable res : fft_data_vector (a'range);
  begin
    for i in a'range loop
      res(i) := a(i) + b(i);
    end loop;
    return res;
  end function;

  pure function "*" (a, b : fft_data_vector) return fft_data_vector is
    variable res : fft_data_vector (a'range);
  begin
    for i in a'range loop
      res(i) := a(i) * b(i);
    end loop;
    return res;
  end function;
end package body fft_types;

-- fft --------------------------------------------------------------------------------
library ieee;
use ieee.math_complex.all;
use work.fft_types.all;

entity fft is
  generic (
    POW : natural  := 4;
    DIM : positive := 2**POW);
  port (
    x, w : in  fft_data_vector (0 to DIM-1);
    y    : out fft_data_vector (0 to DIM-1) := (others => fft_data_type'(0.0,0.0)));
end entity fft;

architecture recursive of fft is
  subtype half_array is fft_data_vector (0 to DIM/2-1);
  signal even, odd, x_e, x_o, w_e : half_array := (others => fft_data_type'(0.0,0.0));
  alias w_b : half_array is w(0 to DIM/2-1);
  alias w_t : half_array is w(DIM/2 to DIM-1);
begin
  stage :
  if base_case: POW = 1 generate
    y(0) <= x(0) + x(1);
    y(1) <= x(0) - x(1);
  else general_case: generate
    split_even_odd :
    for i in half_array'range generate
      x_e(i) <= x(2*i);
      x_o(i) <= x(2*i+1);
      w_e(i) <= w(2*i);
    end generate;

    even_fft :
      entity fft
        generic map (
          POW => POW-1)
        port map (
          x => x_e, w => w_e, y => even);

    odd_fft :
      entity fft
        generic map (
          POW => POW-1)
        port map (
          x => x_o, w => w_e, y => odd);

    y(0 to DIM/2-1)   <= even + odd*w_b;
    y(DIM/2 to DIM-1) <= even + odd*w_t;
  end generate stage;
end architecture recursive;

-------------------------------------------------------------------------------

use work.fft_types.all;

entity issue831 is
  generic (
    POW : natural  := 4;
    DIM : positive := 2**POW);
end entity;

architecture test of issue831 is
    signal x, w, y : fft_data_vector(0 to DIM-1);
begin

    u: entity work.fft
        port map ( x, w, y );

    check: process is
    begin
        x <= (others => fft_data_type'(0.0,0.0));
        w <= (others => fft_data_type'(0.0,0.0));
        wait for 1 ns;
        assert y(0).re = 0.0 report to_string(y(0).re);
        x <= (others => fft_data_type'(1.0,0.0));
        w <= (others => fft_data_type'(0.0,1.0));
        wait for 1 ns;
        assert y(0).re = -4.0 report to_string(y(0).re);
        wait;
    end process;

end architecture;
