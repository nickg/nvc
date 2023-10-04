entity protected13 is
end entity;

architecture test of protected13 is

    type t_test is protected
        impure function get_path return string;
    end protected;

    type t_test is protected body
        variable v : integer;

        impure function get_path return string is
        begin
            return v'path_name;
        end function;
    end protected body;

    type t_test_ptr is access t_test;

    impure function make_test return t_test_ptr is
    begin
        return new t_test;
    end function;

begin

    p1: process is
    begin
        report make_test.get_path;

        -- The LRM doesn't seem to specify this
        assert make_test.get_path = ":protected13:make_test[return t_test_ptr]::v";

        wait;
    end process;

end architecture;
