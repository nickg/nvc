-- Compile into library "foo"
--

entity foo is
	port (
		i: in bit;
		o: out bit
	);
end;

architecture bar of foo is
begin
	o <= i;
end;

configuration foo_bar of foo is
	for bar
	end for;
end;
