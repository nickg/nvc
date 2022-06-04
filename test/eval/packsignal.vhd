package packsignal is
    type int_vec is array (natural range <>) of integer;

    function resolved (x : int_vec) return integer;

    subtype rint is resolved integer;

    signal s : rint;
end package;

package body packsignal is

    function resolved (x : int_vec) return integer is
    begin
        return 0;
    end function;

end package body;
