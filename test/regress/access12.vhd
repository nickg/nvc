entity access12 is
end entity;

architecture test of access12 is
    type node_color is (RED, BLACK, NIL) ;
    type node ;
    type node_ptr is access node ;

    type node is record
        data   : integer ;
        left   : node_ptr ;
        right  : node_ptr ;
        parent : node_ptr ;
        color  : node_color ;
    end record ;

    procedure handle_black_sibling_with_red_child(variable n : inout node_ptr ;
                                                  variable node_is_left_child : out boolean) is
    begin
        -- Triggers LLVM verification error
        node_is_left_child := (n.parent.left = n) ;
    end procedure;
begin

    p1: process is
        variable n : node_ptr;
        variable b : boolean;
    begin
        n := new node;
        n.parent := new node;
        handle_black_sibling_with_red_child(n, b);
        assert not b;
        n.parent.left := n;
        handle_black_sibling_with_red_child(n, b);
        assert b;
        wait;
    end process;

end architecture;
