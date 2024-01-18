entity issue839 is
end entity;

architecture test of issue839 is
    type t_rec is record
        f : integer;
    end record;

    type t_array is array (natural range <>) of t_rec;
    subtype t_sub is t_array;

    signal s : t_sub(1 to 2);
begin

    check: process is
    begin
        -- This would call the wrong helper function
        assert s = (1 to 2 => (f => integer'left));
        s(2).f <= 6;
        wait for 0 ns;
        assert s(2) = (f => 6);
        wait;
    end process;

end architecture;
