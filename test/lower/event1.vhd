entity event1 is
end entity;

architecture test of event1 is
    type rec is record
        f : integer_vector;
    end record;

    procedure do_wait(signal s : in rec) is
    begin
        wait on s;
    end procedure;
begin
end architecture;
