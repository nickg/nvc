entity e is end entity;

package foo is
    function "and"(x, y : integer) return bit;
end package;

architecture a of e is
    signal v : bit_vector(1 to 3);
    type rec is record
        z : integer;
    end record;
    function f(x : integer) return rec;
begin

    process is
        variable x, y, z : integer; variable b : boolean; variable q : bit_vector(1 to 3);
    begin
        b := not (x > y);
        x := abs (-5);
        x := y ** z;
        x := f(4).z;
        q := q sll 1;
        q := q srl 1;
        q := q sla 1;
        q := q sra 1;
        q := q rol 1;
        q := q ror 1;
        x := work.foo."and"(1, 2);
        x(y'range) := y;
        v := (1 => 1, x'range => 2);
    end process;

end architecture;
