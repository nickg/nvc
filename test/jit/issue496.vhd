package genpack is
    generic ( type t; n : t );
    constant k : t := n;
end package;

package issue496 is
    constant one : string := "one";
    package gen_one is new work.genpack generic map ( t => string, n => one );
    constant c : string(1 to 3) := gen_one.k;
end package;
