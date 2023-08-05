package order1 is
    type t is (A, B, C);
    type t_vec is array (natural range <>) of t;
end package;

package order1 is
    type t is (C, B, A);                -- Redefine t
    type t_vec is array (1 to 2) of t;
    constant x : boolean := t_vec'(A, A) < t_vec'(C, C);      -- False
end package;
