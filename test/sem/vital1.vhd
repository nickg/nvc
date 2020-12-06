package vital1 is
    TYPE VitalTransitionType IS ( tr01, tr10, tr0z, trz1, tr1z, trz0,
                                  tr0X, trx1, tr1x, trx0, trxz, trzx);

    SUBTYPE VitalDelayType     IS TIME;
    TYPE VitalDelayType01   IS ARRAY (VitalTransitionType   RANGE tr01 to tr10)
        OF TIME;                        -- OK
end package;
