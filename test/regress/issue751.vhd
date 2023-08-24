entity issue751 is
end entity;

architecture test of issue751 is
    type t_rec is record
        x : integer;
        y : string(1 to 5);
    end record;

    impure function get_rec return t_rec is
    begin
        return (1, "hello");
    end function;

    constant k : t_rec := get_rec;
begin

    -- This process has no label
    process is
        function get_x return integer is
        begin
            return k.x + 1;
        end function;

        constant c : integer := get_x;
    begin
        assert c = 2;
        wait;
    end process;

end architecture;
