entity issue1728 is
end entity;

architecture test of issue1728 is
    subtype t_ptr is natural range 0 to 1;

    function next_ptr (p : natural) return t_ptr is
    begin
        return p + 1;                   -- Error when p = 1
    end function;
begin

    p1: process is
        variable v : natural := 0;
    begin
        for i in 1 to 4 loop
            v := next_ptr(v);
            report "v=" & integer'image(v);
            assert v = 1;               -- Should not get here a second time
        end loop;
        wait;
    end process;

end architecture;
