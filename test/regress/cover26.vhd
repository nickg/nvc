entity sub is
    generic ( nest : natural );
end entity;

architecture test of sub is
begin

    sub_hier_gen : if (nest > 0) generate

        sub_inst_long_path : entity work.sub
        generic map (
            nest => nest - 1
        );

    end generate;

    leaf_hier_gen : if (nest = 0) generate

        process
        begin
            wait for 1 ns;
            report "I am at leaf level";
            wait for 1 ns;
            wait;
        end process;

    end generate;

end architecture;


entity cover26 is
end entity;

architecture test of cover26 is
begin

    sub_inst_long_path : entity work.sub
    generic map (
        nest => 60
    );

end architecture;
