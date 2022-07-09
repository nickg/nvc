entity driver13 is
end entity;

architecture test of driver13 is

    constant MAX_NAME_LENGTH : positive := 20;

    type t_channel is (NA, ALL_CHANNELS, RX, TX);

    type t_record_unresolved is record
        trigger           : bit;
        vvc_name          : string(1 to MAX_NAME_LENGTH);
        vvc_instance_idx  : integer;
        vvc_channel       : t_channel;
    end record;

    constant C_VVC_TARGET_RECORD_DEFAULT : t_record_unresolved := (
      trigger            =>  '0',
      vvc_name           =>  (others => '?'),
      vvc_instance_idx   =>  -1,
      vvc_channel        =>  NA
      );   --
    type t_record_drivers is array (natural range <> ) of t_record_unresolved;

    function resolved ( input : t_record_drivers) return t_record_unresolved;

    subtype t_record is resolved t_record_unresolved;

    function resolved ( input : t_record_drivers) return t_record_unresolved is
        variable v_result : t_record_unresolved := input(input'low);
    begin
        for i in input'range loop
            report to_string(i) & ": trigger=" & to_string(input(i).trigger)
                & " vvc_name=" & input(i).vvc_name;

            assert input(i).vvc_name(1 to 5) = "hello"
                or input(i).vvc_name(1 to 5) = "world"
                or input(i).vvc_name(1 to 5) = (1 to 5 => NUL);
        end loop;
        return v_result;
    end resolved;

    signal s : t_record;
begin

    p1: s <= ('0', ("hello", others => NUL), 1, RX);
    p2: s <= ('1', ("world", others => NUL), 2, TX);

end architecture;
