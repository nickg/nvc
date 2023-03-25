package simple is
    procedure test_add2;
    procedure test_fact;
    procedure test_sum;
    procedure test_int_image;
    procedure test_sqrt;
    procedure test_copy;
end package;

package body simple is

    function add2 (n : integer) return integer is
    begin
        return n + 2;
    end function;

    procedure test_add2 is
        variable sum : integer := 0;
    begin
        for i in 1 to 100 loop
            sum := sum + add2(i);
        end loop;
        assert sum = 5250;
    end procedure;

    ---------------------------------------------------------------------------

    function fact (n : integer) return integer is
        variable r : integer := 1;
    begin
        for i in 1 to n loop
            r := r * i;
        end loop;
        return r;
    end function;

    procedure test_fact is
        variable sum : integer := 0;
    begin
        for i in 1 to 12 loop
            sum := sum + fact(i);
        end loop;
        assert sum = 522956313;
    end procedure;

    ---------------------------------------------------------------------------

    type int_vector is array (natural range <>) of integer;

    function sum(a : int_vector) return integer is
        variable result : integer := 0;
    begin
        for i in a'range loop
            result := result + a(i);
        end loop;
        return result;
    end function;

    procedure test_sum is
        variable dummy : integer;
        variable arr   : int_vector(1 to 1000);
    begin
        for i in 1 to arr'length loop
            arr(i) := i;
        end loop;
        for i in 1 to 100 loop
            dummy := dummy + sum(arr);
        end loop;
    end procedure;

    ---------------------------------------------------------------------------

    procedure test_int_image is
        variable s : string(1 to 3);
    begin
        assert integer'image(4) = "4";
        for i in 1 to 100 loop
            assert integer'image(i)'length <= 3;
        end loop;
    end procedure;

    ---------------------------------------------------------------------------

    function sqrt (n, limit : real) return real is
        variable x    : real := n;
        variable root : real;
    begin
        loop
            root := 0.5 * (x + (n / x));
            exit when abs(root - x) < limit;
            x := root;
        end loop;
        return root;
    end function;

    procedure test_sqrt is
        variable sum : real := 0.0;
    begin
        assert abs(sqrt(4.0, 0.0001) - 2.0) < 0.0001;
        for i in 1 to 100 loop
            sum := sum + sqrt(real(i), 0.0001);
        end loop;
        assert integer(sum) = 671;
    end procedure;

    ---------------------------------------------------------------------------

    procedure do_copy (dest : out bit_vector; src : in bit_vector) is
    begin
        dest := src;
    end procedure;

    procedure test_copy is
        variable src, dest : bit_vector(1 to 4096);
    begin
        for i in 1 to 100 loop
            do_copy(dest, src);
        end loop;
    end procedure;

end package body;
