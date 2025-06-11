entity issue1213 is
    generic (
        PARAM : string := "hello" );
    port (
        a, b : in bit;
        c    : out bit );
end entity;

architecture test of issue1213 is
    component and_gate is
        port (
            a, b : in bit;
            c    : out bit );
    end component;
begin

    ifgen_label: if PARAM = "hello" generate
        and_gate_inst: and_gate
            port map (a, b, c);
    else generate
        assert false report "entity not instantiated" severity failure;
    end generate;

    casegen_label: case PARAM generate
        when "hello" =>
        when others =>
            assert false report "wrong case branch" severity failure;
    end generate;

end architecture;
