entity issue1080 is
end entity;

architecture test of issue1080 is
    type t_enum is (a, b, c);
    type t_other is array (natural range <>) of bit;

    subtype t_nat_sub is natural range 3 downto 0;
    signal s2 : t_other(3 downto 0);
    signal s3 : bit_vector(5 downto 0);
begin

    u: block is
        port ( o : out bit_vector );
        port map ( s3 );

        type t_array is array (t_enum range a to c) of bit_vector(o'range);
        signal s1 : t_array;
    begin
        p: s1(b) <= (t_nat_sub => bit_vector(s2), others => '0');
    end block;

end architecture test;
