ENTITY issue1125 IS END ENTITY;

ARCHITECTURE arch OF issue1125 IS
BEGIN

    gen_if : IF true GENERATE
        ASSERT gen_if'simple_name = "gen_if";
        ASSERT gen_if'path_name = ":issue1125:gen_if:";
    END GENERATE;

    -- *_name attributes in if generate with alternative labels not supported:
    -- f : FOR i IN 0 TO 2 GENERATE
    --     gen_if : IF gen_if_0 : i = 0 GENERATE
    --         ASSERT gen_if'simple_name = "gen_if";
    --         ASSERT gen_if'path_name = ":issue1125:f(0):gen_if:";
    --
    --         ASSERT gen_if_0'simple_name = "gen_if_0";
    --         ASSERT gen_if_0'path_name = ":issue1125:f(0):gen_if_0:";
    --
    --     ELSIF gen_if_1 : i = 1 GENERATE
    --         ASSERT gen_if'simple_name = "gen_if";
    --         ASSERT gen_if'path_name = ":issue1125:f(1):gen_if:";
    --
    --         ASSERT gen_if_1'simple_name = "gen_if_1";
    --         ASSERT gen_if_1'path_name = ":issue1125:f(1):gen_if_1:";
    --
    --     ELSE GENERATE
    --         ASSERT gen_if'simple_name = "gen_if";
    --         ASSERT gen_if'path_name = ":issue1125:f(2):gen_if:";
    --     END GENERATE;
    --
    --     ASSERT gen_if'simple_name = "gen_if";
    --     ASSERT gen_if'path_name = ":issue1125:f(" & INTEGER'image(i) & "):gen_if:";
    -- END GENERATE;

    ASSERT gen_if'simple_name = "gen_if";
    -- Error, currently resolves to ":issue1125:"
    -- ASSERT gen_if'path_name   = ":issue1125:gen_if:";

    gen_for : FOR i IN 0 TO 1 GENERATE
        ASSERT gen_for'simple_name = "gen_for";
        ASSERT gen_for'path_name = ":issue1125:gen_for(" & INTEGER'image(i) & "):";
    END GENERATE;

    ASSERT gen_for'simple_name = "gen_for";
    -- Error, currently resolves to ":issue1125:"
    -- ASSERT gen_for'path_name   = ":issue1125:gen_for:";

    gen_case : CASE true GENERATE
        WHEN true =>
            ASSERT gen_case'simple_name = "gen_case";
            ASSERT gen_case'path_name   = ":issue1125:gen_case:";
        WHEN false =>
    END GENERATE;

    ASSERT gen_case'simple_name = "gen_case";
    -- Error, currently resolves to ":issue1125:"
    -- ASSERT gen_case'path_name   = ":issue1125:gen_case:";

    -- *_name attributes in case generate with alternative labels not supported:
    -- gen_case : CASE true GENERATE
    --     WHEN gen_case_true : true =>
    --         ASSERT gen_case'simple_name = "gen_case";
    --         ASSERT gen_case'path_name   = ":issue1125:gen_case:";
    --
    --         ASSERT gen_case_true'simple_name = "gen_case_true";
    --         ASSERT gen_case_true'path_name   = ":issue1125:gen_case_true:";
    --     WHEN false =>
    -- END GENERATE;
END ARCHITECTURE;
