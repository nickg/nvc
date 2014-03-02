entity issueD is
begin
end entity issueD;

architecture a of issueD is
    component c is
        generic (g : bit_vector);
    end component c;
begin
    u : c
    generic map (g => (1 downto 0 => '1'));
end architecture a;
