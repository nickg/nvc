package pkg is
	generic (
		UNUSED: boolean
	);

	function fun return boolean;
end;

package body pkg is
	function fun return boolean is begin
		return false;
	end;

	constant c: boolean := fun;
end;

entity test is
end;

architecture test of test is
	package p is new work.pkg generic map (UNUSED => false);
begin
end;
