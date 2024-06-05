package genpack is
    generic ( constant a, b : real );
    constant k : real := a / b;
end package;

package inst is new work.genpack
    generic map ( 4.0, 2.0 );
