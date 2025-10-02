package pack is
    type t_rec is record
        x : natural;
        y : string;
    end record;

    type t_byte_array is array (natural range <>) of bit_vector(7 downto 0);
    type t_int_array is array (1 downto 0) of integer range 3 downto 0;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity vhpi11 is
    generic (
        g0 : t_rec := (55, "hello") );
end entity;

architecture test of vhpi11 is
    signal s : t_rec(y(1 to 3));        -- See issue #866
    signal t : t_byte_array(1 to 3);
    signal u : t_int_array;
    signal v : natural range 0 to 1;    -- See issue #1310
begin
end architecture;
