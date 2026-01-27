entity issue1393 is begin
end entity;

architecture a of issue1393 is
    signal s0 : bit_vector(3 downto 0);

    subtype t1 is bit_vector(3 downto 0);
    signal s1 : t1;

    type r2 is record
        d : bit_vector(3 downto 0);
    end record;
    signal s2 : r2;

    type r3 is record
        d : t1;
    end record;
    signal s3 : r3;

    type r4 is record
    end record;
    signal s4 : r4;

    type r5 is record
        d : bit_vector;
    end record;
    signal s5 : r5(d(3 downto 0));

    type r6 is record
        d : bit_vector;
        d2 : bit_vector(3 downto 0);
    end record;
    signal s6 : r6(d(3 downto 0));

    type r7 is record
        d : bit_vector(t1'range);
    end record;
    signal s7 : r7;

    type r8 is record
        d : bit_vector(3 downto 0);
        d2 : bit_vector(3 downto 0);
    end record;
    signal s8 : r8;

    type r9 is record
        d : bit_vector(3 downto 0);
        d2 : bit_vector;
    end record;
    signal s9 : r9(d2(1 downto 0));

    type r10 is record
        d : bit_vector(1 downto 0);
        d2 : bit_vector(1 downto 0);
        d3 : bit_vector(1 downto 0);
        d4 : bit_vector(1 downto 0);
        d5 : bit_vector(1 downto 0);
        d6 : bit_vector(1 downto 1);
    end record;
    signal s10 : r10;

begin
    process is
    begin
        report s0'image; -- expected: "0000",   ok
        report s1'image; -- expected: "0000",   ok
        report s2'image; -- expected: ("0000"), but got: ("")
        report s3'image; -- expected: ("0000"), but got: ("")
        report s4'image; -- expected: (),       but got: (
        report s5'image; -- expected: ("0000"), ok
        report s6'image; -- expected: ("0000","0000"), but got: ("0000","000000")
        report s7'image; -- expected: ("0000"),        but got: ("")
        report s8'image; -- expected: ("0000","0000"), but got: ("","00")
        report s9'image; -- expected: ("0000","00"), but got: ("0000","000000")

        -- expected: ("00","00","00","00","00","0")
        -- but got:  ("","00","0000","000000","00000000","0000¥¥0000")
        -- note:     values in last element seem to be partly random
        report s10'image;
        wait;
    end process;
end architecture;
