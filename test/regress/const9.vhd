entity const9 is
    constant str : string := const9'path_name;
end entity;

architecture test of const9 is
begin

    p1: process is
    begin
        report str;
        assert str = ":const9:";
        wait;
    end process;

end architecture;
