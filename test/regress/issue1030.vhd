package pack is
    generic ( N : natural );

    type t_rec is record
        x : bit;
        y : bit_vector(7 downto 0);
        z : bit_vector(1 to N);
    end record;

    type t_rec2 is record
        f : t_rec;
    end record;

    type t_array is array (natural range <>) of t_rec2;

    impure function get_width return integer;
end package;

package body pack is
    impure function get_width return integer is
    begin
        return N;
    end function;
end package body;

package pack8 is new work.pack generic map (8);

entity issue1030 is
end entity;

use work.pack8.all;

architecture test of issue1030 is
    signal s : t_array(1 to 2);
begin

    b: block is
        port ( p : in bit_vector(get_width - 1 downto 0) );
        port map ( s(1).f.z );
    begin
    end block;

    s(1) <= (f => ('1', X"01", X"02")) after 1 ns;

end architecture;
