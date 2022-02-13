package fold1_pkg is
    type int_vec_2d is array (natural range <>, natural range <>) of integer;
end package;

-------------------------------------------------------------------------------

use work.fold1_pkg.all;

entity sub is
    generic ( G : int_vec_2d );
end entity;

architecture test of sub is
    constant k : int_vec_2d := G;
begin

    main: process is
    begin
        assert G(1, 2) = 6;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity fold1 is
end entity;

use work.fold1_pkg.all;

architecture test of fold1 is

    function get_vec return int_vec_2d is
    begin
        return ( (1, 2, 3),
                 (4, 5, 6) );
    end function;

begin

    u: entity work.sub generic map ( get_vec );

end architecture;
