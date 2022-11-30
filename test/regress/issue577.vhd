entity issue577 is
end entity;

architecture test of issue577 is

    type mem_t is array (natural range <>) of bit_vector;
    type mem_ptr_t is access mem_t;

    function init_mem return mem_t is
        variable mem : mem_t(1 to 3)(1 to 5);
    begin
        return mem;
    end function;

begin

    p1: process is
        variable ptr : mem_ptr_t;
    begin
        ptr := new mem_t'(init_mem);
        ptr(2)(2) := '1';
        wait for 1 ns;
        assert ptr(2)(2) = '1';
        assert ptr(2)(3) = '0';
        wait;
    end process;

end architecture;
