package test_pkg is

    type t_test_rec is record
        a  : bit_vector;
        b  : bit_vector;
    end record t_test_rec;

    -- first declaration
    procedure test_proc(
        t : in t_test_rec
    );

    -- overload
    procedure test_proc(
        a : in bit_vector;
        b : in bit_vector
    );

end package test_pkg;

package body test_pkg is

    -- first declaration
    procedure test_proc(
        t : in t_test_rec
    ) is
    begin
        -- do something
    end procedure test_proc;

    -- overload
    procedure test_proc(
        a : in bit_vector;
        b : in bit_vector
    ) is
    begin
        test_proc(
            t.a => a,
            t.b => b
        );
    end procedure test_proc;

end package body test_pkg;
