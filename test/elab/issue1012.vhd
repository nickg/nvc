library ieee;
use ieee.std_logic_1164.all;

entity issue1012 is
end entity;

architecture test of issue1012 is
    signal addr : std_logic_vector(-1 downto 0);
begin

    i_sub_block : block is
        generic ( WIDTH : natural );
        generic map ( WIDTH => 0 );
        port ( addr : in std_logic_vector(WIDTH - 1 downto 0) );
        port map ( addr  => addr );     -- Error
    begin
        -- Should not create toggle coverage for null array
    end block;

end architecture;
