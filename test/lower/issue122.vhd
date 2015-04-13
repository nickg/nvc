entity issue122 is
end entity;

architecture test of issue122 is
    function func(x : integer) return integer is
        function nested return integer is
        begin
            return x;
        end function;
        variable v : integer := nested;
    begin
        return v;
    end function;
begin
end architecture;
