entity incorrect_choice_length is
end entity;

architecture arch of incorrect_choice_length is

type some_datastructure_t is record
	name  : string;
	value : natural range 0 to 3;
end record;

type some_datastructure_array is array (natural range <>) of some_datastructure_t;

constant some_datastructure : some_datastructure_array(0 to 3) := (
	0 => (name => "zarro", value => 0),
	1 => (name => "one",   value => 1),
	2 => (name => "two",   value => 2),
	3 => (name => "three", value => 3)
);

begin

	process is
		variable int : integer;
		procedure printValue(constant x: some_datastructure_t) is
		begin
			case x.name is
				when "zarro" =>
					report "We got value : " & natural'image(x.value);
				when "one" =>
					report "We got value : " & natural'image(x.value);
				when "two" =>
					report "We got value : " & natural'image(x.value);
				when "three" =>
					report "We got value : " & natural'image(x.value);
			end case;
		end procedure;
	begin
		printValue(some_datastructure(0));
		printValue(some_datastructure(1));
		printValue(some_datastructure(2));
		printValue(some_datastructure(3));
	end process;

end architecture;
