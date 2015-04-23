-- a_pkg.vhd
package A_PKG is
    type POS_TYPE is record
         X : integer;
         Y : integer;
    end record;
end package;

-- a_ng.vhd
use WORK.A_PKG.all;
entity A_NG is
    generic (POS: POS_TYPE := (X => 0, Y => 0));
end entity;
architecture MODEL of A_NG is
    procedure GET_X(X:out integer) is
    begin
        X := POS.X;                     -- "no visible declaration for POS.X"
    end procedure;
begin
end architecture;
