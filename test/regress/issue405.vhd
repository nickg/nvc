entity buf is
    port ( a : in bit;  y : out bit );
end entity;

architecture test of buf is
begin
    y <= a after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity fanout_tree is
    generic ( h : natural;  d : positive );
    port ( input : in bit;  output : out bit_vector (0 to d**h - 1) );
end fanout_tree;

architecture recursive of fanout_tree is
    component buf
        port ( a : in bit;  y : out bit );
    end component;

    component fanout_tree
        generic ( h : natural;  d : positive );
        port ( input : in bit;  output : out bit_vector(0 to d**h - 1) );
    end component;

    signal buffered_input : bit_vector(0 to d - 1);
begin
    degenerate_tree : if h = 0 generate
        output(0) <= input;
    end generate degenerate_tree;

    compound_tree : if h > 0 generate
        subtree_array : for i in 0 to d - 1 generate
            the_buffer : buf
                port map ( a => input, y => buffered_input(i) );
            the_subtree : fanout_tree
                generic map ( h => h - 1, d => d )
                port map ( input => buffered_input(i),
                           output => output(i * d**(h-1) to (i+1) * d**(h-1) -1) );
        end generate subtree_array;
    end generate compound_tree;
end recursive;

-------------------------------------------------------------------------------

entity issue405 is
end entity;

architecture test of issue405 is
    signal input : bit;
    signal output : bit_vector(0 to 4**3 - 1);
begin

    top_i: entity work.fanout_tree
        generic map ( h => 3, d => 4 )
        port map ( input, output );

    check: process is
    begin
        wait for 5 ns;
        assert output = (output'range => '0');
        input <= '1';
        wait for 5 ns;
        assert output = (output'range => '1');
        wait;
    end process;

end architecture;
