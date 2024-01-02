entity issue821 is
end entity;

architecture test of issue821 is
    signal x : integer;
begin

    g: for i in 1 to 2 generate
    begin
        p: process is
        begin
            if i > 1 then
                x <= 1;
            elsif x < 100 then
                x <= 2;
            end if;
            wait;
        end process;
    end generate;

end architecture;
