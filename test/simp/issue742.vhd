PACKAGE tb_test_package IS

	CONSTANT C_VEC    : bit_vector(2 DOWNTO 0) := "111";

	PROCEDURE test (
		o: OUT bit
	);

END PACKAGE tb_test_package;

PACKAGE BODY tb_test_package IS

	PROCEDURE test (
		o: OUT bit
	) IS
	BEGIN
		IF C_VEC /= "000" THEN
			o := '1';
		ELSE
			o := '0';
		END IF;
	END test;

END PACKAGE BODY tb_test_package;
