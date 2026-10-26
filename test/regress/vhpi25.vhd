entity vhpi25 is
end entity;

architecture test of vhpi25 is
    type u1_t is range 0 to 1;
    type u8_t is range 0 to 255;
    type s8_t is range -128 to 127;
    type u16_t is range 0 to 65535;
    type u8_vec_t is array (natural range <>) of u8_t;
    type s8_vec_t is array (natural range <>) of s8_t;
    type n8_vec_t is array (natural range <>) of integer range 0 to 255;
    type i64_t is range -2**40 to 2**40;
    type i64_vec_t is array (natural range <>) of i64_t;

    signal s1 : u8_t := 200;
    signal s2 : s8_t := -5;
    signal s3 : u16_t := 40000;
    signal s4 : u8_t range 0 to 200 := 150;
    signal s5 : u1_t := 1;
    signal v1 : u8_vec_t(0 to 3) := (11, 22, 200, 44);
    signal v2 : s8_vec_t(0 to 2) := (-1, 2, -3);
    signal v3 : n8_vec_t(0 to 2) := (1, 200, 3);
    signal v4 : i64_vec_t(0 to 2) := (1, -2, 300000);
begin
end architecture;
