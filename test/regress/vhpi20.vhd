package pack is
    type cplx_t is record
        r : bit_vector;
        i : bit_vector;
    end record;

    -- Pre-VHDL-2008: nested array types (array of array of ...)
    type cplx_arr_t is array (natural range <>) of cplx_t;
    type cplx_2d_t  is array (natural range <>) of cplx_arr_t;
    type cplx_3d_t  is array (natural range <>) of cplx_2d_t;

    -- VHDL-2008: native multi-dimensional array types
    type cplx_mat_t  is array (natural range <>, natural range <>) of cplx_t;
    type slv_arr_t   is array (natural range <>) of bit_vector;
    type slv_mat_t   is array (natural range <>, natural range <>) of bit_vector;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity vhpi20 is
end entity;

architecture test of vhpi20 is
    -- Pre-VHDL-2008 nested 3-D array of record
    signal x : cplx_3d_t(0 to 1)(0 to 1)(0 to 1)
                        (r(7 downto 0), i(7 downto 0));
    -- Native VHDL-2008 2-D array of record
    signal y : cplx_mat_t(0 to 1, 0 to 1)(r(7 downto 0), i(7 downto 0));
    -- Pre-VHDL-2008 1-D array of raw SLV (homogeneous)
    signal z : slv_arr_t(0 to 3)(7 downto 0);
    -- Native VHDL-2008 2-D array of raw SLV (homogeneous)
    signal w : slv_mat_t(0 to 1, 0 to 1)(7 downto 0);
begin
end architecture;
