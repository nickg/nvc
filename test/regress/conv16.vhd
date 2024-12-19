package pack is
    type t_conf is record
        width : natural;
    end record;
    function init return t_conf;
end package;

package body pack is
    function init return t_conf is
    begin
        return (width => 5);
    end function;
end package body;

-------------------------------------------------------------------------------

entity sub is
    generic ( g : integer );
end entity;

use work.pack.all;

architecture test of sub is
    signal s : bit_vector(1 to g) := (others => '1');

    constant conf : t_conf := init;

    type t_int_array is array (1 to conf.width) of integer;
    type t_real_array is array (1 to conf.width) of real;

    signal t : t_int_array := (others => 0);
begin

    t <= (3 => 1, others => 5) after 1 ns;

    b: block is
        port ( i : in t_real_array );
        port map ( i => t_real_array(t) );
    begin
        check: process is
        begin
            assert i(3) = 0.0;
            wait for 1 ns;
            assert i(3) = 1.0;
            wait;
        end process;
    end block;
end architecture;

-------------------------------------------------------------------------------

entity conv16 is
end entity;

architecture test of conv16 is
begin

    u1: entity work.sub
        generic map ( 10 );

    u2: entity work.sub
        generic map ( 100 );

end architecture;
