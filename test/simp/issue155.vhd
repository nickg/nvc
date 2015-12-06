package issue155 is
    type W_TYPE is record
        A : integer;
        B : integer;
        C : integer;
    end record;

    constant  W : W_TYPE := (A => 8, B => 4, C => 2);
    signal    A : bit_vector(W.A-1 downto 0);
    signal    B : bit_vector(W.B-1 downto 0);
    signal    C : bit_vector(W.C-1 downto 0);

    constant  V : W_TYPE := (1, 2, 3);
    signal    D : bit_vector(V.C-1 downto V.A);
end package;
