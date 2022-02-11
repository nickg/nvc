entity  IMAGE_STREAM_BUFFER_TEST_BENCH is
    generic (
        NAME            : STRING                  := "test";
        SCENARIO_FILE   : STRING                  := "test.snr";
        ELEMENT_SIZE    : integer                 := 8*1024;
        LINE_SIZE       : integer                 := 0;
        BANK_SIZE       : integer                 := 0;
        FINISH_ABORT    : boolean                 := FALSE
    );
end     IMAGE_STREAM_BUFFER_TEST_BENCH;
-----------------------------------------------------------------------------------
--
-----------------------------------------------------------------------------------
architecture MODEL of IMAGE_STREAM_BUFFER_TEST_BENCH is
begin
    assert ELEMENT_SIZE > 0;
end MODEL;
-----------------------------------------------------------------------------------
-- ELEM_BITS=2bit, CHANNEL_SIZE=0, I.C=32, I.X=1, I.Y=1, O.C=32, O.X=3, O.Y=3 D_SIZE=8
-----------------------------------------------------------------------------------
entity  issue448 is
    generic (
        NAME            : STRING                  := "test_0_2_32x1x1_32x4x3x3_bug1";
        SCENARIO_FILE   : STRING                  := "test_0_2_32x1x1_32x4x3x3_bug1.snr";

        BANK_SIZE       : integer                 := 0;
        LINE_SIZE       : integer                 := 0;
        FINISH_ABORT    : boolean                 := FALSE
    );
end     issue448;
architecture MODEL of issue448 is
    component IMAGE_STREAM_BUFFER_TEST_BENCH is
        generic (
            NAME            : STRING                  := "test";
            SCENARIO_FILE   : STRING                  := "test.snr";
            BANK_SIZE       : integer                 := 0;
            LINE_SIZE       : integer                 := 0;
            FINISH_ABORT    : boolean                 := FALSE
        );
    end component;
begin
    TB: IMAGE_STREAM_BUFFER_TEST_BENCH generic map (
        NAME            => NAME         ,
        SCENARIO_FILE   => SCENARIO_FILE,
        BANK_SIZE       => BANK_SIZE    ,
        LINE_SIZE       => LINE_SIZE    ,
        FINISH_ABORT    => FINISH_ABORT
    );
end MODEL;
