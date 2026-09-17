entity b is
    generic (
        function op(lhs, rhs : natural) return natural
    );
    port (
        lhs     : in  natural;
        rhs     : in  natural;
        out_val : out natural
    );
end entity;

architecture rtl of b is
begin
    out_val <= op(lhs, rhs);
end architecture;

entity a is
    generic (
        function op(lhs, rhs : natural) return natural
    );
    port (
        lhs     : in  natural;
        rhs     : in  natural;
        out_val : out natural;
        out_val2 : out natural
    );
end entity;

architecture rtl of a is
    function op_wrapper(lhs, rhs : natural) return natural is
    begin
        return op(lhs, rhs);
    end function;

    alias op_alias is op [natural, natural return natural];
begin
    inst_b : entity work.b
        generic map (
            -- op => op_wrapper
            op => op
        )
        port map (
            lhs     => lhs,
            rhs     => rhs,
            out_val => out_val
        );

    inst_b2 : entity work.b
        generic map (
            -- op => op_wrapper
            op => op_alias
        )
        port map (
            lhs     => lhs,
            rhs     => rhs,
            out_val => out_val2
        );
end architecture;


entity issue1658 is
end entity;

architecture sim of issue1658 is
    signal x, y, z, z2 : natural := 0;

    function add_natural(lhs, rhs : natural) return natural is
    begin
        return lhs + rhs;
    end function;
begin
    dut : entity work.a
        generic map (
            op => add_natural
        )
        port map (
            lhs     => x,
            rhs     => y,
            out_val => z,
            out_val2 => z2
        );

    process is
    begin
        x <= 3;
        y <= 4;

        wait for 1 ns;

        report "result = " & integer'image(z);
        assert z = 7;
        assert z2 = 7;
        wait;
    end process;
end architecture;
