package alloc is
    procedure test_rand;
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

    shared variable global : list_ptr;
    shared variable count : natural;

    procedure test_rand is
        variable r : real;
        variable p : list_ptr;
        variable n : natural;
    begin
        for i in 1 to 10 loop
            p := new list;
            p.chain := global;
            uniform(seed1, seed2, r);
            p.data := new bit_vector(1 to integer(r * 100000.0));

            global := p;
            count := count + 1;

            if count > 100 then
                n := integer(r * 99.0);
                p := global;
                for j in 1 to n loop
                    p := p.chain;
                end loop;
                p.chain := p.chain.chain;
                count := count - 1;
            end if;
        end loop;
    end procedure;

end package body;
