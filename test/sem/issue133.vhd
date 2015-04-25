package A_PKG is
    type POS_TYPE is record
         X : integer;
         Y : integer;
    end record;
    constant  POS_ZERO : POS_TYPE := (X => 0, Y=> 0);
end package;

use WORK.A_PKG.all;
entity A_NG is
end entity;
architecture MODEL of A_NG is
    procedure GET_ZERO_X(X:out integer) is
    begin
        X := POS_ZERO.X;                -- OK
    end procedure;
begin
end architecture;
