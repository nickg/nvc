architecture a of b is
    signal x : bit_vector(1 to 3);
    signal y : bit_vector(1 to 2);
begin

    x <= y & '0';

    process is
    begin
        p := q & g & ('1', '1');
    end process;

end architecture;
