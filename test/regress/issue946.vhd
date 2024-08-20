package pack is
    generic ( width : natural );
    type t_rec is record
        f : integer_vector(1 to width);
    end record;
    signal s : t_rec;
    alias a is s.f;
    procedure get (index : positive; result : out integer);
end package;

package body pack is
    procedure get (index : positive; result : out integer) is
    begin
        result := a(index);
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity issue946 is
end entity;

architecture test of issue946 is
    package pack6 is new work.pack generic map ( 3 );
    use pack6.all;
begin

    check: process is
        variable result : integer;
    begin
        a <= (1, 2, 3);
        wait for 0 ns;
        for i in 1 to 3 loop
            get(i, result);
            assert result = i;
        end loop;
        wait;
    end process;

end architecture;
