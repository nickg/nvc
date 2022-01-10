entity sub1 is
    generic (
        x : integer := 5;
        y : bit_vector(1 to 3) );
end entity;

package pack is
    type rec is record
        x, y : integer;
    end record;
end package;

use work.pack.all;

entity sub2 is
    generic ( r : rec );
end entity;

entity genmap is
end entity;

use work.pack.all;

architecture test of genmap is
begin

    u1: entity work.sub1
        generic map ( y => ('1', '1', '0'), x => 2 );

    u2: entity work.sub1
        generic map ( y => "101" );

    u3: entity work.sub1
        generic map (
            0, y(1) => '1', y(2) => '0', y(3) => '1' );

    u4: entity work.sub2
        generic map ( r.y => 3, r.x => 2 );

end architecture;
