entity issue884 is
end entity;

architecture test of issue884 is
    procedure proc is
    begin
        << signal .issue884.uut.x : integer >> <= 42;  -- Error
    end procedure;
begin
end architecture;
