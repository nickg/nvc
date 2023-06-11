library ieee;
use ieee.std_logic_1164.all;

entity vhpi8 is
end entity;

architecture test of vhpi8 is
   type enum is (
      e000, e001, e002, e003, e004, e005, e006, e007, e008, e009,
      e010, e011, e012, e013, e014, e015, e016, e017, e018, e019,
      e020, e021, e022, e023, e024, e025, e026, e027, e028, e029,
      e030, e031, e032, e033, e034, e035, e036, e037, e038, e039,
      e040, e041, e042, e043, e044, e045, e046, e047, e048, e049,
      e050, e051, e052, e053, e054, e055, e056, e057, e058, e059,
      e060, e061, e062, e063, e064, e065, e066, e067, e068, e069,
      e070, e071, e072, e073, e074, e075, e076, e077, e078, e079,
      e080, e081, e082, e083, e084, e085, e086, e087, e088, e089,
      e090, e091, e092, e093, e094, e095, e096, e097, e098, e099,
      e100, e101, e102, e103, e104, e105, e106, e107, e108, e109,
      e110, e111, e112, e113, e114, e115, e116, e117, e118, e119,
      e120, e121, e122, e123, e124, e125, e126, e127, e128, e129,
      e130, e131, e132, e133, e134, e135, e136, e137, e138, e139,
      e140, e141, e142, e143, e144, e145, e146, e147, e148, e149,
      e150, e151, e152, e153, e154, e155, e156, e157, e158, e159,
      e160, e161, e162, e163, e164, e165, e166, e167, e168, e169,
      e170, e171, e172, e173, e174, e175, e176, e177, e178, e179,
      e180, e181, e182, e183, e184, e185, e186, e187, e188, e189,
      e190, e191, e192, e193, e194, e195, e196, e197, e198, e199,
      e200, e201, e202, e203, e204, e205, e206, e207, e208, e209,
      e210, e211, e212, e213, e214, e215, e216, e217, e218, e219,
      e220, e221, e222, e223, e224, e225, e226, e227, e228, e229,
      e230, e231, e232, e233, e234, e235, e236, e237, e238, e239,
      e240, e241, e242, e243, e244, e245, e246, e247, e248, e249,
      e250, e251, e252, e253, e254, e255, e256, e257, e258, e259,
      e260, e261, e262, e263, e264, e265, e266, e267, e268, e269,
      e270, e271, e272, e273, e274, e275, e276, e277, e278, e279,
      e280, e281, e282, e283, e284, e285, e286, e287, e288, e289,
      e290, e291, e292, e293, e294, e295, e296, e297, e298, e299
   );
   type enum_vector is array(natural range <>) of enum;
   type severity_vector is array(natural range <>) of severity_level;

   signal lv: std_logic_vector(0 to 3) := "UX01";
   signal sv: severity_vector(0 to 3) := (note, warning, error, failure);
   signal ev: enum_vector(0 to 3) := (e000, e001, e002, e003);
   signal bv: bit_vector(3 to 0);
   signal cv: string(1 to 4) := "NVC!";
   signal rv: real_vector(0 to 3) := (0.0, 0.5, 1.0, -1.0);
begin
end architecture;
