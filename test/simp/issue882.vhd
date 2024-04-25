entity test is
end entity;

architecture arch of test is
    signal my_a : integer := 0;
    signal my_b : integer := 1;

begin
    process (all) is
        procedure do_thing (signal a : in integer; signal b : out integer) is
        begin
            b <= a;
        end procedure;
    begin

        my_b <= my_a;
    end process;
end architecture;
