entity mux4to1 is
    port (
        A,B : in bit;
        Y1 : out bit
    );
end entity mux4to1;

architecture behv of mux4to1 is
    --signal test : bit_vector(1 downto 0);
begin
    y1proc : process(A, B)
        type foo is (F1, F2);
        type foo_vec1 is array (integer range <>) of foo;
        type foo_vec2 is array (integer range <>) of foo;
        variable x : integer;
        variable f : foo;
    begin
        --test <= A & B;
        case (A&B) is                   -- Error
        --case test is
            when others => Y1 <= '0';
        end case;
        case (x & x) is                 -- Error
            when others =>  null;
        end case;
        case (f & f) is                 -- Error
            when others =>  null;
        end case;
    end process;

    process is
        type int_vec is array (integer range <>) of integer;
        variable x : integer;
    begin
        case (x & x) is                 -- Error
            when others => null;
        end case;
    end process;
end architecture;
