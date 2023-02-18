entity const10 is
end entity;

architecture test of const10 is
    shared variable count : integer := 0;

    impure function get_id return integer is
    begin
        count := count + 1;
        return count;
    end function;

    type int_vec is array (natural range <>) of integer;

    constant list : int_vec(1 to 2) := (get_id, get_id);
begin

    p1: process is
    begin
        -- GET_ID should only be called twice
        assert list(1) = 1;
        assert list(1) = 1;
        assert list(2) = 2;
        assert list(1) = 1;
        assert list(2) = 2;
        wait;
    end process;

end architecture;
