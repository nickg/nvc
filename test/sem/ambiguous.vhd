entity e is
end entity;

architecture a of e is
    type foo is (a, b, c);
    type bar is (a, b, c);

    signal x : foo := a;
    signal y : bar := b;
begin

    process is
    begin
        x <= c;
        y <= a;
    end process;

    process is
    begin
        x <= foo'(a);
        y <= bar'(a);
    end process;

    process is
        type baz is (a, b, c, d);
        variable z : baz := b;
    begin
        z := d;
        z := a;
        x <= a;
    end process;

    process is
    begin
        x <= bar'(c);                   -- Error!
    end process;

    process is
        type small is range 10 downto -5;
        variable z : small := -5;
        variable a : boolean;
    begin
        a := z = -5;
        a := -5 = z;
    end process;

    process is
        variable a : bit_vector(3 downto 0);
        variable x : character;
        variable b : boolean;
    begin
        b := x = '1';                   -- OK
        b := '1' = x;                   -- OK
        b := a = ('0', '1', '0', '1');  -- OK
        b := ('0', '1', '0', '1') = a;  -- OK
        b := ('0', '1') = ('0', '1');   -- Error
    end process;
    
end architecture;
