entity attr21 is
end entity;

architecture test of attr21 is
    function foo (x : integer) return boolean is
    begin
        return true;
    end function;

    function foo (y : bit) return string is
    begin
        return "hello";
    end function;

    type my_enum is ('1', '2');

    attribute test : string;
    attribute test of '1' [return my_enum] : literal is "hello";
begin

    p: process is
    begin
        report foo [integer return boolean]'instance_name;  -- OK
        report foo [bit return string]'instance_name;  -- OK
        report '1' [return my_enum]'test;  -- OK
        wait;
    end process;

end architecture;
