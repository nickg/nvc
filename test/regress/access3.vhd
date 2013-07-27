entity access3 is
end entity;

architecture test of access3 is

    type int_ptr is access integer;
    type string_ptr is access string;

begin

    process is
        variable i : int_ptr;
        variable s : string_ptr;
    begin
        i := new integer;
        assert i.all = integer'left;
        i := new integer'(3);
        assert i.all = 3;
        s := new string'("");
        assert s.all = "";
        assert s'length = 0;
        s := new string'("hello");
        assert s.all = "hello";
        wait;
    end process;

end architecture;
