package pack is
    type rec is record
        x : bit_vector(1 to 3);
        y : integer;
    end record;

    type rec_array is array (natural range <>) of rec;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( n : positive );
end entity;

architecture test of sub is

    signal s : rec_array(1 to n);
begin

    p1: process is
    begin
        assert s = ( 1 to n => (x => "000", y => integer'low) );
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity record37 is
end entity;

architecture test of record37 is
begin

    u1: entity work.sub generic map ( 4 );
    u2: entity work.sub generic map ( 5 );

end architecture;
