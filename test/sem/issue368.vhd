-- this package has mis-labeled "in" and "out" parameters in the procedure declaration
-- but no errors or warnings are generated.

package nvc_package_bug is
    procedure in_through_the_out_door (
        signal sig_in: out bit; --note this is labeled an "out" parameter but is really "in"
        signal sig_out: in bit --note this is labeled an "in" parameter but is really "out"
	);

    procedure foo (x : in integer);
    procedure bar (signal x : in integer);
    function baz(x : integer) return integer;
end nvc_package_bug;

package body nvc_package_bug is
    procedure in_through_the_out_door (
        signal sig_in: in bit;
        signal sig_out: out bit) is
    begin
        sig_out <= sig_in;
    end procedure in_through_the_out_door;

    procedure foo (y : in integer) is   -- Error
    begin
    end procedure;

    procedure bar (x : in integer) is   -- Error
    begin
    end procedure;

    function baz(y : integer) return integer is  -- Error
    begin
        return 0;
    end function;
end package body nvc_package_bug;
