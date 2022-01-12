entity  SAMPLE is
    generic (
        QUEUE_SIZE      : integer := 0
    );
end entity;
architecture RTL of SAMPLE is
begin
    QUEUE_SIZE_VALID: if (QUEUE_SIZE >= 0) generate
    end generate;
end RTL;

entity  issue435 is
    generic (
        PORT_DATA_BITS  : integer := 32;
        POOL_DATA_BITS  : integer := 32;
        ALIGNMENT_BITS  : integer := 8;
        QUEUE_SIZE      : integer := 1
    );
end entity;
architecture RTL of issue435 is
    type     SETTING_TYPE   is record
             Q_Q_SIZE       : integer;
    end record;
    function SET_SETTING return SETTING_TYPE is
        variable setting    : SETTING_TYPE;
        constant POOL_WORDS : integer := POOL_DATA_BITS / ALIGNMENT_BITS;
        constant PORT_WORDS : integer := PORT_DATA_BITS / ALIGNMENT_BITS;
    begin
        if (PORT_DATA_BITS /= ALIGNMENT_BITS) or
           (POOL_DATA_BITS /= ALIGNMENT_BITS) then
            setting.Q_Q_SIZE := POOL_WORDS*(QUEUE_SIZE+1)+PORT_WORDS-1;
        else
            setting.Q_Q_SIZE := -1;
        end if;
        return setting;
    end function;
    constant SET            : SETTING_TYPE := SET_SETTING;
begin
    Q: entity WORK.SAMPLE generic map (QUEUE_SIZE => SET.Q_Q_SIZE);
end RTL;
