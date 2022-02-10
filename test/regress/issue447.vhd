entity issue447 is
end entity;

architecture test of issue447 is

    procedure assign(v : out bit_vector; b : in bit) is
        variable canary : integer := 42;
    begin
        v := (v'range => b);
        report integer'image(canary);
    end procedure;

    procedure proc (x : natural) is
        type rec is record
            f : bit_vector(1 to x);
        end record;

        variable v : rec;
    begin
        wait for 1 ns;
        assert v.f = (1 to x => '0');
        wait for 1 ns;
        assign(v.f, '1');
        wait for 1 ns;
        assert v.f = (1 to x => '1');
    end procedure;

begin

    main: process is
    begin
        proc(12);                       -- This call would clobber stack
        wait;
    end process;

end architecture;
