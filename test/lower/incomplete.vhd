package p is
    constant cp : integer;
end package p;

package body p is
    constant c : integer := 1;
    alias ca : integer is c;
    constant cp : integer := ca;
end package body p;
