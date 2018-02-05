entity issue341 is
end entity;

architecture test of issue341 is
begin

    process is
        function func(b : bit) return boolean is
        begin
            return b = '1';
        end function;

        function func(b : character) return boolean is
        begin
            return b = '1';
        end function;

        alias func_bit is func [bit return boolean];
        alias func_char is func [character return boolean];
    begin
        assert func_bit('1');           -- OK
        assert func_char('1');          -- OK
        assert func_bit('y');           -- Error
        wait;
    end process;

    no_parameter : process
        function func return boolean is
        begin
            return false;
        end;

        alias func_alias is func[return boolean];
        variable value : boolean := func_alias;
    begin
        if func_alias then
            assert false;
        end if;

        while func_alias loop
            assert false;
        end loop;
        wait;
    end process;

    with_parameter : process
        function func(value : boolean) return boolean is
        begin
            return value;
        end;

        alias func_alias is func[boolean return boolean];
        variable value : boolean := func_alias(false);
    begin
        if func_alias(false) then
            assert false;
        end if;

        while func_alias(false) loop
            assert false;
        end loop;
        wait;
    end process;

end architecture;
