package issue269 is
    constant CWIDTH : positive := 10;
    type int_vec is array (integer range <>) of integer;
    signal s1 : int_vec(CWIDTH downto 0);
end package;
