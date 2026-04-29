package pack is
    type cplx_t is record
        r : bit_vector;
        i : bit_vector;
    end record;
    type cplx_arr_t is array (natural range <>) of cplx_t;
    type cplx_2d_t  is array (natural range <>) of cplx_arr_t;
    type cplx_3d_t  is array (natural range <>) of cplx_2d_t;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity vhpi20 is
end entity;

architecture test of vhpi20 is
    signal x : cplx_3d_t(0 to 1)(0 to 1)(0 to 1)
                        (r(7 downto 0), i(7 downto 0));
begin
end architecture;
