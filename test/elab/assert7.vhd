entity assert7 is
end entity;

architecture test of assert7 is

    impure function func (x : integer) return integer is
    begin
        assert x > 0;
        return -x;
    end function;

    function resolved (x : bit_vector) return bit is
    begin
        return '1';
    end function;

    subtype rbit is resolved bit;

    -- Disables init side effects temporarily
    signal s : rbit;

    -- Asserts during initialisation
    constant c : integer := func(-1);

begin
end architecture;
