
--  This file is supposed to be generated from a configuration.
--  For Alire we use this file with default values.

with Adl_Middleware_Config;

package ADL_Config is
   Max_Mount_Points : constant := Adl_Middleware_Config.Max_Mount_Points;

   Max_Mount_Name_Length : constant :=
     Adl_Middleware_Config.Max_Mount_Name_Length;

   Max_Path_Length : constant := Adl_Middleware_Config.Max_Path_Length;
end ADL_Config;
