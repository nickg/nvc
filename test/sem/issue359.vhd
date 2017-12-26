entity issue359 is
end entity;

architecture test of issue359 is

    signal foo : integer;

    procedure foo (signal foo : in integer) is  -- Error
    begin
    end procedure;

begin

    process is
    begin
        foo(foo);
        wait;
    end process;

end architecture;
