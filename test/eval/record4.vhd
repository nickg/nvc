package pack5 is
    type int_vector is array (natural range <>) of integer;

    type pair is record
        first  : integer;
        second : integer;
    end record;

    type pair_ptr is access pair;

    type pair_vector is array (natural range <>) of pair;

    type pair_vector_ptr is access pair_vector;

    function func1 (x : integer) return integer;
    function func2 (x : integer) return integer;
    function func3 (x : integer) return integer;

end package;

package body pack5 is

    function func1 (x : integer) return integer is
        variable p : pair_vector_ptr;
        variable result : integer := 0;
    begin
        p := new pair_vector(1 to x);
        for i in 1 to x loop
            p.all(i).first := i;
            p.all(i).second := x;
        end loop;
        for i in p.all'range loop
            result := result + p(i).first + p(i).second;
        end loop;
        return result;
    end function;

    function func2 (x : integer) return integer is
        variable p : pair_vector(1 to x);
        variable result : integer := 0;
    begin
        for i in 1 to x loop
            p(i).first := i;
            p(i).second := x;
        end loop;
        for i in p'range loop
            result := result + p(i).first + p(i).second;
        end loop;
        return result;
    end function;

    function func3 (x : integer) return integer is
        variable p : pair_ptr;
    begin
        p := new pair;
        p.first := x;
        p.second := x * 2;
        return p.first + p.second;
    end function;

end package body;
