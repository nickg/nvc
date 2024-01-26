library ieee;
use ieee.math_real.all;
use ieee.float_pkg.all;

package fft_pkg is
  subtype num_type is float32;
  type data_type is record
    re, im : num_type;
  end record;
  type data_vector is array (natural range <>) of data_type;
  function "+" (a, b : data_type) return data_type;
  function "*" (a, b : data_type) return data_type;
  function even_elem(x : data_vector) return data_vector;
  function odd_elem(x : data_vector) return data_vector;
  function weight(n : positive) return data_vector;
end package fft_pkg;

package body fft_pkg is
  function "+" (a, b : data_type) return data_type is
  begin
    return data_type'(a.re + b.re, a.im + b.im);
  end function;

  function "*" (a, b : data_type) return data_type is
  begin
    return data_type'(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re);
  end function;

  function even_elem(x : data_vector) return data_vector is
    variable res : data_vector (0 to x'length/2-1);
  begin
    for i in res'range loop
      res(i) := x(2*i);
    end loop;
    return res;
  end function;

  function odd_elem(x : data_vector) return data_vector is
    variable res : data_vector (0 to x'length/2-1);
  begin
    for i in res'range loop
      res(i) := x(2*i+1);
    end loop;
    return res;
  end function;

  function weight(n : positive) return data_vector is
    variable w : data_vector (0 to n-1);
    variable omega : real;
  begin
    omega := math_2_pi/real(n);
    for k in w'range loop
      w(k) := data_type'(to_float(cos(omega*real(k))), to_float(-sin(omega*real(k))));
    end loop;
    return w;
  end function;
end package body fft_pkg;

library ieee;
use ieee.float_pkg.all;
use work.fft_pkg.all;

entity fft is
  generic (
    POW : natural;
    DIM : positive := 2**POW;
    w: data_vector (0 to DIM-1) := weight(DIM));
  port (
    x : in  data_vector (0 to DIM-1);
    y : out data_vector (0 to DIM-1));
end entity fft;

architecture recursive of fft is
begin
  stage_n :
  if POW > 0 generate
    subtype half_array is data_vector (0 to DIM/2-1);
    signal even, odd : half_array;
    alias w_b : half_array is w(0 to DIM/2-1);
    alias w_t : half_array is w(DIM/2 to DIM-1);
  begin
    even_fft :
      entity fft
        generic map (
          POW => POW-1, w => even_elem(w))
        port map (
          x => even_elem(x), y => even);
    odd_fft :
      entity fft
        generic map (
          POW => POW-1, w => even_elem(w))
        port map (
          x => odd_elem(x), y => odd);
    butterfly :
    process (all) is
    begin
      for i in half_array'range loop
        y(i)       <= even(i) + odd(i) * w_b(i);
        y(i+DIM/2) <= even(i) + odd(i) * w_t(i);
      end loop;
    end process;
  end generate stage_n;
  stage_0 :
  if POW = 0 generate
    y <= x;
  end generate stage_0;
end architecture recursive;

library ieee;
use ieee.math_real.all;
use ieee.math_complex.all;
use ieee.fixed_float_types.all;
use ieee.float_pkg.all;
use work.fft_pkg.all;

entity issue843 is
end entity issue843;

architecture test of issue843 is
  constant POW : natural := 2;
  constant DIM : positive := 2**POW;
  signal x, y : data_vector (0 to DIM-1) := (others => data_type'(to_float(0.0), to_float(0.0)));
  type complex_vector is array (0 to DIM-1) of complex;
  constant stimul : complex_vector := ((0.0, 0.0), ( 1.0, 1.0), ( 2.0,  2.0), (3.0,  3.0));
  constant result : complex_vector := ((6.0, 6.0), (-4.0, 0.0), (-2.0, -2.0), (0.0, -4.0));
  constant abstol : real := 1.0e-6;
begin
  fft_inst :
    entity work.fft(recursive)
      generic map (POW => POW)
      port map (x => x, y => y);
  test_proc :
    process is
      alias s is to_string [integer return string];
      alias s is to_string [real return string];
    begin
      for i in x'range loop
        x(i) <= data_type'(to_float(stimul(i).re), to_float(stimul(i).im));
      end loop;
      wait for 1 ps;
      for i in y'range loop
        assert abs(to_real(y(i).re) - result(i).re) < abstol and abs(to_real(y(i).im) - result(i).im) < abstol
          report "Inconsistent result detected: y(" & s(i) & ") = (" & s(to_real(y(i).re)) & "," & s(to_real(y(i).im)) & ")"  severity failure;
      end loop;
      wait;
    end process;
end architecture test;
