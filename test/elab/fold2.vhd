package fold2_pkg is
    type int_vec_2d is array (natural range <>, natural range <>) of integer;
end package;

-------------------------------------------------------------------------------

use work.fold2_pkg.all;

entity sub is
    generic ( G : int_vec_2d(1 to 2, 1 to 3) );
end entity;

architecture test of sub is
    constant k : int_vec_2d := G;
begin

    main: process is
    begin
        assert G(1, 2) = 2;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity fold2 is
end entity;

use work.fold2_pkg.all;

architecture test of fold2 is

    subtype int_vec_2d_2x3 is int_vec_2d(1 to 2, 1 to 3);

    function get_vec return int_vec_2d_2x3 is
    begin
        return ( (1, 2, 3),
                 (4, 5, 6) );
    end function;

begin

    u: entity work.sub generic map ( get_vec );

end architecture;
