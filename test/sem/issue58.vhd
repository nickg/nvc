entity issueG is
begin
end entity issueG;

architecture a of issueG is
    type t is record
        g : bit;
    end record t;
    constant c : t := (
        g => '0'
    );
    component comp is
        generic (g : bit_vector := "0");
    end component comp;
begin
    u : comp
    generic map (g => (1 downto 0 => c.g));
end architecture a;
