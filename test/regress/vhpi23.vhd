entity sub is
end entity;

architecture test of sub is
    procedure check (x : in integer);

    procedure check (x : in integer) is
    begin
        assert false report "foreign binding was not resolved" severity failure;
    end procedure;

    attribute foreign : string;
    attribute foreign of check : procedure is "VHPI vhpi23 check";
begin
    process is
    begin
        check(42);
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity vhpi23 is
end entity;

architecture test of vhpi23 is
    -- Instantiate through a component rather than directly so the foreign
    -- subprogram is declared inside a component instance
    component sub is
    end component;
begin
    u: component sub;
end architecture;
