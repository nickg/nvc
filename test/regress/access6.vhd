entity access6 is
end entity;

architecture test of access6 is
    type int_ptr is access integer;
begin

    process is
        variable p : int_ptr;
    begin
        p.all := 5;
        wait;
    end process;

end architecture;
