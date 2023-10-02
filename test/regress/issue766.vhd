entity sub is
    generic ( WIDTH, DEPTH : natural );
end entity;

architecture test of sub is
    subtype t_word is bit_vector(WIDTH - 1 downto 0);

    type t_mem is protected
        impure function get (addr : natural) return t_word;
        procedure put (addr : natural; word : t_word);
    end protected;

    type t_mem is protected body
        type t_mem is array (0 to WIDTH - 1) of t_word;
        variable mem : t_mem;

        impure function get (addr : natural) return t_word is
        begin
            return mem(addr);
        end function;

        procedure put (addr : natural; word : t_word) is
        begin
            mem(addr) := word;
        end procedure;
    end protected body;

    shared variable pt : t_mem;

begin

    p1: process is
        constant w : t_word := (others => '1');
    begin
        for i in 0 to WIDTH - 1 loop
            pt.put(i, w);
        end loop;
        wait for 1 ns;
        for i in 0 to WIDTH - 1 loop
            assert pt.get(i) = w;
        end loop;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue766 is
end entity;

architecture test of issue766 is
begin

    u: entity work.sub
        generic map ( 8, 16 );

end architecture;
