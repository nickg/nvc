entity b is end entity;

architecture a of b is
    signal x : bit_vector(1 to 3);
    signal y : bit_vector(1 to 2);
    signal p : bit_vector(1 to 3);
    signal q, g : bit;
begin

    x <= y & '0';

    process is
        variable c : bit_vector(1 to 4);
    begin
        c := q & g & ('1', '1');
    end process;

end architecture;
