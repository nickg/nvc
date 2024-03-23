package alloc is
    procedure test_rand_small;
    procedure test_rand_large;
end package;

library ieee;
use ieee.math_real.all;

package body alloc is

    shared variable seed1 : positive := 12456;
    shared variable seed2 : positive := 626256;

    type bv_ptr is access bit_vector;

    type list;
    type list_ptr is access list;

    type list is record
        chain : list_ptr;
        data  : bv_ptr;
    end record;

    type bv_ptr_array is array (natural range <>) of bv_ptr;
    type bv_ptr_array_ptr is access bv_ptr_array;

    shared variable global, delete : list_ptr;
    shared variable count : natural := 0;

    procedure do_rand_test (max_size : real; max_count : positive) is
        variable r : real;
        variable p : list_ptr;
        variable n : natural;
    begin
        for i in 1 to 10 loop
            p := new list;
            p.chain := global;
            uniform(seed1, seed2, r);
            p.data := new bit_vector(1 to integer(r * max_size));

            global := p;
            count := count + 1;

            if count > max_count then
                n := integer(r * real(max_count - 1));
                p := global;
                for j in 1 to n loop
                    p := p.chain;
                end loop;
                p.chain := p.chain.chain;
                count := count - 1;
            end if;
        end loop;
    end procedure;

    procedure test_rand_small is
    begin
        do_rand_test(1000.0, 500);
    end procedure;

    procedure test_rand_large is
    begin
        do_rand_test(100000.0, 100);
    end procedure;

end package body;
