entity access_bug is
end entity;

architecture test of access_bug is
type rec_t is record
    foo: integer;
end record rec_t;
type rec_ptr_t is access rec_t;
type rec_arr_t is array (integer range <>) of rec_ptr_t;
type rec_arr_ptr_t is access rec_arr_t;
procedure bug_procedure is
    variable rec_ptr: rec_arr_ptr_t;
begin
    rec_ptr(0) := new rec_t'(foo => 42);
    rec_ptr(0).all.foo := 2; -- this works
    rec_ptr(0).foo := 2; -- <-- bug here
end;
begin
end architecture;
