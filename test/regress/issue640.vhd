ENTITY issue640 IS
END ENTITY issue640;

ARCHITECTURE issue640_arch of issue640 IS

   TYPE  t_cmd IS (IDLE, READ, WRITE);
   SIGNAL cmd: t_cmd := IDLE;

   signal state : natural;
BEGIN

    bfm: PROCESS IS
    BEGIN
        WAIT ON cmd'TRANSACTION;
        IF cmd = IDLE THEN
            state <= 0;
        ELSIF cmd = READ THEN
            state <= 1;
        ELSIF cmd = WRITE THEN
            state <= 2;
        END IF;
    END PROCESS bfm;


    sim: PROCESS IS
    BEGIN
        cmd <= IDLE;
        WAIT FOR 10 ns;
        assert state = 0;
        cmd <= READ;
        WAIT FOR 10 ns;
        assert state = 1;
        cmd <= WRITE;
        wait for 0 ns;
        assert state = 1;
        wait for 0 ns;
        assert state = 2;
        WAIT;
    END PROCESS sim;

END ARCHITECTURE;
