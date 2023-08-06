entity case4 is
end entity;

architecture test of case4 is
    type int_vec is array (natural range <>) of integer;

    procedure test (s : string) is
    begin
        case s is                       -- Cannot check
            when "hello" => null;
        end case;
    end procedure;

begin

    process is
        variable s : string(1 to 12);
    begin
        case s is                       -- Error
            when "0123456789ab" => null;
        end case;
    end process;

end architecture;
