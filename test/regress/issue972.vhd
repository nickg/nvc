entity issue972 is
end entity;

architecture a of issue972 is
    type t_rec is record
	f : natural;
    end record;

    type t_array is array (natural range <>) of t_rec;

    subtype t_ports is natural range 0 to 1;

    signal x, y : t_array(t_ports'high downto 0);

begin

    x <= (0 => (f => 0), 1 => (f => 1)) after 1 ns,
	 (0 => (f => 0), 1 => (f => 2)) after 2 ns;

    b: block is
	generic ( type t );
	generic map ( t => t_array );
	port ( i : in t; o : out t );
	port map ( x, y );
    begin
	o <= i after 1 ns;
    end block;

end architecture;
