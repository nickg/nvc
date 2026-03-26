package p is
	type array_of_int is array (integer range <>) of integer;

	type my_record is record
		data : array_of_int;
	end record;

	type array_of_records is array (integer range <>) of my_record;
end package;

-------------------------------------------------------------------------------

use work.p.all;

entity issue1463 is
	port (
		sig : in array_of_records(0 to 1)(data(0 to 1))
	);
end entity;

architecture arch of issue1463 is
begin
end architecture;
