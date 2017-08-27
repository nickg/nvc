entity issue350 is
end entity;

architecture test of issue350 is
    constant STAY_AT_LIMIT_STRING : string  := "STAY_AT_LIMIT";
    constant COUNTER_WRAPAROUND_MAX : integer := STAY_AT_LIMIT_STRING'length;
    signal COUNTER_WRAPAROUND_PAD   : string(1 to COUNTER_WRAPAROUND_MAX);
begin

end architecture;
