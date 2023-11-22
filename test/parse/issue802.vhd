package my_logic is

  type STD_ULOGIC is ( 'U',             -- Uninitialized
                       'X',             -- Forcing  Unknown
                       '0',             -- Forcing  0
                       '1',             -- Forcing  1
                       'Z',             -- High Impedance
                       'W',             -- Weak     Unknown
                       'L',             -- Weak     0
                       'H',             -- Weak     1
                       '-'              -- Don't care
                       );

  function "??" (x : std_ulogic) return boolean;

  subtype std_logic is std_ulogic;
end package;

-- types

use work.my_logic.all;

package fifo_types is
  alias logic is std_ulogic;
  alias "??" is "??" [std_ulogic return boolean];
--  alias logic_vec is std_ulogic_vector;
end package;
use work.fifo_types.all;

-- fifo
entity fifo is
  generic (constant DEPTH_W : positive := 8);
  port (ireset, irclk, rd_en, iwclk, wr_en : in logic);
end entity;

architecture rtl of fifo is
begin

  write_proc : process (iwclk, ireset) is
  begin
    if ireset then
    end if;
  end process;

end architecture;
