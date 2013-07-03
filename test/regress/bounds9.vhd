entity bounds9 is
end entity;

architecture test of bounds9 is

    procedure foo(x : bit_vector) is
        variable s : string(x'range);
    begin
        report "should not print this" severity failure;
    end procedure;

begin

    process is
        variable v : bit_vector(0 to 3);
    begin
        foo(v);
        wait;
    end process;

end architecture;
