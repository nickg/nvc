entity issue369 is
end entity;

architecture foo of issue369 is
begin
THIS_PROCESS:
    process
        type twovalue is ('0', '1');
        subtype string4 is string(1 to 4);
        attribute a: string4;
        attribute a of '1' : literal is "TRUE";
    begin
        assert THIS_PROCESS.'1''a = "TRUE";
        assert THIS_PROCESS.'1''a'RIGHT = 4;
        wait;
    end process;
end architecture;
