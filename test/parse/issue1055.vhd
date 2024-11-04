package pack1 is
    type t_int_vec is array (natural range <>) of integer;
    type t_real_vec is array (natural range <>) of real;

    function resolved (x : t_int_vec) return integer;
    function resolved (x : t_real_vec) return real;
end package;

-------------------------------------------------------------------------------

package pack2 is
    subtype t_int is work.pack1.resolved integer;
end package;
