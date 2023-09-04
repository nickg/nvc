entity issue756 is
end entity;

architecture test of issue756 is
    type t_bv_array is array (natural range <>) of bit_vector;
    type t_bv_array_ptr is access t_bv_array;

    impure function get_size return integer is
    begin
        return 10;
    end function;
begin

    tb: process is
        variable ptr : t_bv_array_ptr;
    begin
        ptr := new t_bv_array(0 to get_size)(7 downto 0);
        wait;
    end process;

end architecture;
