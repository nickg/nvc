package pack is
    type rec is record
        x, y : integer;
    end record;

    type rec_vec is array (integer range <>) of rec;

    constant c : bit_vector(3 downto 0) := X"f";
    constant d : rec_vec(1 to 2);
end package;

entity top is
end entity;

use work.pack.all;

architecture test of top is
    signal x : bit := not c(1);
    signal y : integer := d(1).x + 1;
begin

end architecture;
