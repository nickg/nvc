library ieee;
use ieee.std_logic_1164.all;

entity cover18 is
end cover18;

architecture test of cover18 is

    -- Example of simple FSM that fits into one byte
    type t_fsm_state is (
        ST_IDLE,
        ST_ONE,
        ST_TWO,
        ST_THREE,
        ST_WAITING,
        ST_DONE
    );

    -- Example of FSM that fits into two bytes
    type t_larger_fsm_state is (
        S_1     ,S_2    ,S_3    ,S_4	,S_5	,S_6	,S_7    ,S_8    ,S_9    ,S_10	,S_11	,S_12	,S_13	,S_14	,S_15	,S_16
        ,S_17	,S_18	,S_19	,S_20	,S_21	,S_22	,S_23	,S_24	,S_25	,S_26	,S_27	,S_28	,S_29	,S_30	,S_31	,S_32
        ,S_33	,S_34	,S_35	,S_36	,S_37	,S_38	,S_39	,S_40	,S_41	,S_42	,S_43	,S_44	,S_45	,S_46	,S_47	,S_48
        ,S_49	,S_50	,S_51	,S_52	,S_53	,S_54	,S_55	,S_56	,S_57	,S_58	,S_59	,S_60	,S_61	,S_62	,S_63	,S_64
        ,S_65	,S_66	,S_67	,S_68	,S_69	,S_70	,S_71	,S_72	,S_73	,S_74	,S_75	,S_76	,S_77	,S_78	,S_79	,S_80
        ,S_81	,S_82	,S_83	,S_84	,S_85	,S_86	,S_87	,S_88	,S_89	,S_90	,S_91	,S_92	,S_93	,S_94	,S_95	,S_96
        ,S_97	,S_98	,S_99	,S_100	,S_101	,S_102	,S_103	,S_104	,S_105	,S_106	,S_107	,S_108	,S_109	,S_110	,S_111	,S_112
        ,S_113	,S_114	,S_115	,S_116	,S_117	,S_118	,S_119	,S_120	,S_121	,S_122	,S_123	,S_124	,S_125	,S_126	,S_127	,S_128
        ,S_129	,S_130	,S_131	,S_132	,S_133	,S_134	,S_135	,S_136	,S_137	,S_138	,S_139	,S_140	,S_141	,S_142	,S_143	,S_144
        ,S_145	,S_146	,S_147	,S_148	,S_149	,S_150	,S_151	,S_152	,S_153	,S_154	,S_155	,S_156	,S_157	,S_158	,S_159	,S_160
        ,S_161	,S_162	,S_163	,S_164	,S_165	,S_166	,S_167	,S_168	,S_169	,S_170	,S_171	,S_172	,S_173	,S_174	,S_175	,S_176
        ,S_177	,S_178	,S_179	,S_180	,S_181	,S_182	,S_183	,S_184	,S_185	,S_186	,S_187	,S_188	,S_189	,S_190	,S_191	,S_192
        ,S_193	,S_194	,S_195	,S_196	,S_197	,S_198	,S_199	,S_200	,S_201	,S_202	,S_203	,S_204	,S_205	,S_206	,S_207	,S_208
        ,S_209	,S_210	,S_211	,S_212	,S_213	,S_214	,S_215	,S_216	,S_217	,S_218	,S_219	,S_220	,S_221	,S_222	,S_223	,S_224
        ,S_225	,S_226	,S_227	,S_228	,S_229	,S_230	,S_231	,S_232	,S_233	,S_234	,S_235	,S_236	,S_237	,S_238	,S_239	,S_240
        ,S_241	,S_242	,S_243	,S_244	,S_245	,S_246	,S_247	,S_248	,S_249	,S_250	,S_251	,S_252	,S_253	,S_254	,S_255	,S_256
        ,S_257	,S_258	,S_259	,S_260	,S_261	,S_262	,S_263	,S_264	,S_265	,S_266	,S_267	,S_268	,S_269	,S_270	,S_271	,S_272
    );

    signal curr_state : t_fsm_state;
    signal state_reg  : t_larger_fsm_state;

    -- To check coverage for built-in enums is not emmited
    signal my_sig     : STD_LOGIC;
    signal my_bool    : boolean;

begin

    process begin
        curr_state <= ST_TWO;
        wait for 5 ns;
        curr_state <= ST_WAITING;
        wait for 5 ns;
        curr_state <= ST_DONE;
        wait for 5 ns;

        state_reg <= S_4;
        wait for 5 ns;
        state_reg <= S_8;
        wait for 5 ns;
        state_reg <= S_16;
        wait for 5 ns;
        state_reg <= S_32;
        wait for 5 ns;
        state_reg <= S_64;
        wait for 5 ns;
        state_reg <= S_128;
        wait for 5 ns;
        state_reg <= S_256;
        wait for 5 ns;
        state_reg <= S_264;

        wait for 5 ns;

        wait;
    end process;

end architecture;
