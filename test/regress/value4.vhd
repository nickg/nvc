entity value4 is
end entity;

architecture test of value4 is
    type t_abc is ('a', 'b', 'c');
    type t_abc_vec is array (natural range <>) of t_abc;
begin

    process is
    begin
        assert t_abc_vec'value("abX") = "abc";  -- Error
        wait;
    end process;

end architecture;
