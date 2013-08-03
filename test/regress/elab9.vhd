entity sub_ent is
end entity;

architecture test of sub_ent is
begin

    process is
    begin
        report sub_ent'path_name;
        report sub_ent'instance_name;
        report test'path_name;
        report test'instance_name;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab9 is
end entity;

architecture test of elab9 is
begin

    sub_i: entity work.sub_ent;

    process is
    begin
        wait for 1 ns;
        report elab9'path_name;
        report elab9'instance_name;
        report test'path_name;
        report test'instance_name;
        wait;
    end process;

end architecture;
