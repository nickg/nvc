entity c is
    generic (e : bit);
    port (i : in bit);
begin
    assert i = e;
end entity c;

architecture a of c is
begin
end architecture a;

entity issue53 is
begin
end entity issue53;

architecture a of issue53 is
    component c is
        generic (e : bit);
        port (i : in bit);
    end component c;
begin
    u0 : c generic map ('0') port map (i => '0');
    u1 : c generic map ('1') port map (i => '1');
end architecture a;
