-- Type qualified expressions
entity bar is end entity;
architecture foo of bar is
    type foo is (a, b, 'c');
    type bar is (a, b, c);

    signal x : foo;
begin

    process is
    begin
        x <= foo'(b);
        --x <= foo'('c');
        x <= foo'( 'c' );
    end process;

end architecture;
