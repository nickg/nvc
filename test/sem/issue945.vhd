entity test1 is
    port (
        clk_i   : bit;
        data_i  : bit_vector(1 downto 0);
        stb_i   : bit
    );
end test1;

architecture arch of test1 is

begin

end arch;

entity test0 is
end test0;

architecture arch of test0 is

type test_record is record
    x : bit;
    y : bit;
end record;

type larger_record is record
    tr  : test_record;
end record;

function RecordToVec(r : test_record) return bit_vector is
begin
    return r.y & r.x;
end function;

signal clk      : bit := '0';
signal stb      : bit := '0';
signal r        : larger_record;

begin

inst0 : entity work.test1
    port map (
        clk_i   => clk,
        stb_i   => stb,
        data_i  => RecordToVec(r.tr)    -- OK
    );

inst1 : entity work.test1
    port map (
        clk_i   => clk,
        stb_i   => stb,
        data_i  => RecordToVec(r)       -- Error
    );

inst2 : entity work.test1
    port map (
        clk_i   => clk,
        stb_i   => stb,
        data_i  => RecordToVec(r.tr.x)  -- Error
    );

clk <= not clk after 5 ns;

end arch;
