package prot1 is
    impure function fetch_and_add (n : integer) return integer;
end package;

package body prot1 is

    type pt is protected
        procedure increment (n : integer);
        impure function get return integer;
    end protected;

    type pt is protected body
        variable counter : integer := 0;

        procedure increment (n : integer) is
        begin
            counter := counter + n;
        end procedure;

        impure function get return integer is
        begin
            return counter;
        end function;
    end protected body;

    shared variable p : pt;

    impure function fetch_and_add (n : integer) return integer is
    begin
        p.increment(n);
        return p.get;
    end function;

end package body;
