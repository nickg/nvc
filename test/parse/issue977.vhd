entity issue977 is
end entity;

architecture test of issue977 is
    procedure foo is
    begin
    end procedure;

    attribute foreign of foo : procedure is "abc";  -- OK
    attribute foreign of foo : procedure is "123";  -- Error

begin

end architecture;
