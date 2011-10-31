entity image is
end entity;

architecture test of image is
begin

    process is
        variable i : integer;
    begin
        report integer'image(4);
        report integer'image(-42);
        i := 73;
        report integer'image(i);
        wait;
    end process;
    
end architecture;
