entity range3 is
end entity;

architecture test of range3 is
    type t_rec is record
        x : bit_vector;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal s1 : t_rec(x(1 to 3));
    signal s2 : t_rec_array(1 to 2)(x(1 to 3));
begin

    b1: block is
        port ( p : out t_rec );
        port map ( s1 );
    begin

        g: for i in p.x'range generate
        begin
            p.x(i) <= '1';
        end generate;

    end block;

    b2: block is
        port ( p : out t_rec_array );
        port map ( s2 );
    begin

        g: for i in p'range generate
        begin
            p(i).x <= "111";
        end generate;

    end block;

    b3: block is
        port ( p : in t_rec_array );
        port map ( s2 );

        signal s3 : p'subtype;
    begin

        s3 <= s2;
        assert s3(1).x'length = 3;

    end block;

    check: process is
    begin
        wait for 1 ns;
        assert s1 = (x => "111");
        assert s2 = (1 to 2 => (x => "111"));
        wait;
    end process;

end architecture;
