entity case4 is
end entity;

architecture test of case4 is
    type int_vec is array (natural range <>) of integer;

    procedure test (s : string) is
    begin
        case s is                       -- Error
            when "hello" => null;
        end case;
    end procedure;

    procedure test2 (n : natural) is
        subtype t_not_static is natural range 0 to n;
        variable v : t_not_static;
    begin
        case v is                       -- Error
            when 0 => null;
            when 1 => null;
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
