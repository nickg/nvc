entity mcase is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mcase is
begin

    p1: process is
        variable x : std_logic;
        variable y : std_logic_vector(1 to 3);
        variable z : integer;
    begin
        case? x is                      -- OK
            when '1' => null;
            when '-' => null;
        end case?;
        case? z is
            when 1 => null;             -- Error
        end case?;
        case? y is                      -- OK
            when "---" => null;
        end case?;
        wait;
    end process;

    p2 : process is
        type my_vec is array (natural range <>) of std_logic;
        type my_vec2 is array (natural range <>, natural range <>) of std_logic;
        variable x : my_vec(1 to 1);
        variable y : my_vec2(1 to 1, 1 to 1);
    begin
        case? x is                      -- OK
            when "1" => null;
            when "-" => null;
        end case?;
        case? y is                      -- Error
            when others => null;
        end case?;
        wait;
    end process;

end architecture;
