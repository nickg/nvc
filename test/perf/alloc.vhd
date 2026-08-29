package alloc is
    procedure test_fixed_small_dead;
    procedure test_fixed_small_live;
    procedure test_line_boundaries;
    procedure test_fixed_large;
    procedure test_rand_small;
    procedure test_rand_large;
    procedure test_bimodal;
end package;

package body alloc is

    -- Each call to a benchmark allocates the same amount of array payload so
    -- that jitperf's operations/second result is directly comparable.
    constant ALLOC_BYTES : positive := 1024 * 1024;

    type bv_ptr is access bit_vector;
    type bv_ptr_array is array (natural range <>) of bv_ptr;
    type bv_ptr_array_ptr is access bv_ptr_array;

    shared variable live          : bv_ptr_array_ptr;
    shared variable live_count    : natural := 0;
    shared variable replace_index : natural := 0;
    shared variable random_index  : natural := 0;

    type size_array is array (natural range <>) of positive;

    function make_sizes (max_size : positive) return size_array is
        variable result : size_array(0 to 255);
        variable seed   : integer := 12456;
        variable scale  : integer;
    begin
        -- Park-Miller generator using Schrage's method to avoid overflow.
        for i in result'range loop
            scale := seed / 127773;
            seed := 16807 * (seed - scale * 127773) - 2836 * scale;
            if seed <= 0 then
                seed := seed + 2147483647;
            end if;
            result(i) := 1 + seed mod max_size;
        end loop;
        return result;
    end function;

    constant RAND_SMALL_SIZES : size_array := make_sizes(1000);
    constant RAND_LARGE_SIZES : size_array := make_sizes(100000);

    procedure allocate_one (size : positive; max_live : positive) is
        variable p    : bv_ptr;
        variable slot : natural;
    begin
        if live = null then
            live := new bv_ptr_array(0 to max_live - 1);
        else
            assert live.all'length = max_live;
        end if;

        p := new bit_vector(1 to size);

        if live_count < max_live then
            slot := live_count;
            live_count := live_count + 1;
        else
            slot := replace_index;
            replace_index := (replace_index + 1) mod max_live;
        end if;

        live(slot) := p;
    end procedure;

    procedure do_fixed_test (size : positive; max_live : positive) is
        variable allocated : natural := 0;
        variable next_size : positive;
    begin
        while allocated < ALLOC_BYTES loop
            next_size := size;
            if next_size > ALLOC_BYTES - allocated then
                next_size := ALLOC_BYTES - allocated;
            end if;

            allocate_one(next_size, max_live);
            allocated := allocated + next_size;
        end loop;
    end procedure;

    procedure do_rand_test (sizes : size_array; max_live : positive) is
        variable allocated : natural := 0;
        variable next_size : positive;
    begin
        while allocated < ALLOC_BYTES loop
            next_size := sizes(random_index);
            random_index := (random_index + 1) mod sizes'length;
            if next_size > ALLOC_BYTES - allocated then
                next_size := ALLOC_BYTES - allocated;
            end if;

            allocate_one(next_size, max_live);
            allocated := allocated + next_size;
        end loop;
    end procedure;

    procedure test_fixed_small_dead is
    begin
        do_fixed_test(16, 1);
    end procedure;

    procedure test_fixed_small_live is
    begin
        do_fixed_test(16, 4096);
    end procedure;

    procedure test_line_boundaries is
        constant sizes : size_array := (31, 32, 33, 63, 64, 65);
        variable allocated : natural := 0;
        variable index     : natural := sizes'low;
        variable next_size : positive;
    begin
        while allocated < ALLOC_BYTES loop
            next_size := sizes(index);
            if next_size > ALLOC_BYTES - allocated then
                next_size := ALLOC_BYTES - allocated;
            end if;

            allocate_one(next_size, 4096);
            allocated := allocated + next_size;
            if index = sizes'high then
                index := sizes'low;
            else
                index := index + 1;
            end if;
        end loop;
    end procedure;

    procedure test_fixed_large is
    begin
        do_fixed_test(4096, 512);
    end procedure;

    procedure test_rand_small is
    begin
        do_rand_test(RAND_SMALL_SIZES, 500);
    end procedure;

    procedure test_rand_large is
    begin
        do_rand_test(RAND_LARGE_SIZES, 100);
    end procedure;

    procedure test_bimodal is
        variable allocated : natural := 0;
        variable sequence  : natural := 0;
        variable next_size : positive;
    begin
        while allocated < ALLOC_BYTES loop
            if sequence mod 16 = 0 then
                next_size := 64 * 1024;
            else
                next_size := 32;
            end if;

            if next_size > ALLOC_BYTES - allocated then
                next_size := ALLOC_BYTES - allocated;
            end if;

            allocate_one(next_size, 512);
            allocated := allocated + next_size;
            sequence := sequence + 1;
        end loop;
    end procedure;

end package body;
