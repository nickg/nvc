entity ir1045 is
end entity;

architecture foo of ir1045 is
begin
THIS_PROCESS:
    process
        type twovalue is ('0', '1');
        subtype string4 is string(1 to 4);
        attribute a: string4;
        attribute a of '1' : literal is "TRUE";
    begin
        assert THIS_PROCESS.'1''a /= "TRUE"
            report "'1''a /= ""TRUE"" is FALSE";
        report "THIS_PROCESS.'1''a'RIGHT = " &
            integer'image(THIS_PROCESS.'1''a'RIGHT);
        wait;
    end process;
end architecture;
