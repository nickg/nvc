entity array17 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of array17 is

    type t_slv_array is array (natural range <>) of std_logic_vector;
    subtype t_byte_array is t_slv_array(open)(7 downto 0);

    type t_byte_endianness is (LOWER_BYTE_LEFT, LOWER_BYTE_RIGHT, LOWER_WORD_LEFT, LOWER_WORD_RIGHT, FIRST_BYTE_LEFT, FIRST_BYTE_RIGHT);

    -- From UVVM methods_pkg.vhd

  function convert_slv_to_byte_array(
    constant slv             : std_logic_vector;
    constant byte_endianness : t_byte_endianness
  ) return t_byte_array is
    variable v_num_bytes   : integer := slv'length / 8 + 1; -- +1 in case there's a division remainder
    alias normalized_slv   : std_logic_vector(slv'length-1 downto 0) is slv;
    variable v_byte_array  : t_byte_array(0 to v_num_bytes - 1);
    variable v_slv_idx     : integer := normalized_slv'high;
    variable v_slv_idx_min : integer;
  begin
    -- Adjust value if there was no remainder
    if (slv'length rem 8) = 0 then
      v_num_bytes := v_num_bytes - 1;
    end if;

    for byte_idx in 0 to v_num_bytes - 1 loop
      for bit_idx in 7 downto 0 loop
        if v_slv_idx = -1 then
          v_byte_array(byte_idx)(bit_idx) := 'Z'; -- Pads 'Z'
        else
          if (byte_endianness = LOWER_BYTE_LEFT) or (byte_endianness = FIRST_BYTE_LEFT) then
            v_byte_array(byte_idx)(bit_idx) := normalized_slv(v_slv_idx);
          else                          -- LOWER_BYTE_RIGHT or FIRST_BYTE_RIGHT
            v_slv_idx_min                   := MINIMUM(8 * byte_idx + bit_idx, normalized_slv'high); -- avoid indexing outside the slv
            v_byte_array(byte_idx)(bit_idx) := normalized_slv(v_slv_idx_min);
          end if;
          v_slv_idx := v_slv_idx - 1;
        end if;
      end loop;
    end loop;
    return v_byte_array(0 to v_num_bytes - 1);
  end function;

begin

    p1: process is
        variable v : std_logic_vector(31 downto 0) := X"12345678";
    begin
        assert convert_slv_to_byte_array(v, LOWER_BYTE_LEFT) = (X"12", X"34", X"56", X"78");
        assert convert_slv_to_byte_array(v, LOWER_BYTE_RIGHT) = (X"78", X"56", X"34", X"12");
        wait;
    end process;

end architecture;
