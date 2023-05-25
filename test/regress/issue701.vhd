entity issue701 is
end entity;

architecture test of issue701 is
   type sample is array (natural range <>) of integer;
   type dozen_samples is array (1 to 12) of sample;
   subtype dozen_short_samples is dozen_samples(open)(0 to 9);

   signal my_dozen: dozen_short_samples;
begin

    p1: process is
        variable index : natural;
    begin
        my_dozen(1)(0) <= 4;
        my_dozen(12) <= (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
        index := 10;
        wait for 0 ns;
        assert my_dozen(1)(0) = 4;
        assert my_dozen(12)(2) = 3;

        my_dozen(6)(index) <= 4;           -- Error

        wait;
    end process;

end architecture;
