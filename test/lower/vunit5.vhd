entity vunit5 is
end entity;

architecture test of vunit5 is

    type rec is record
        f : bit_vector;
    end record;

    procedure proc (f : bit_vector) is
        variable r : rec(f(f'length - 1 downto 0));
    begin
        r.f := f;
    end procedure;

begin
end architecture;
