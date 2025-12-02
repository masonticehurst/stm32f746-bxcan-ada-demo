with System;
with GUI;            use GUI;
with Ada.Exceptions; use Ada.Exceptions;

package body Exception_Handler is
   procedure Last_Chance_Handler (E : Exception_Occurrence) is
   begin
      Put_Line (Exception_Information (E), Red);
   end Last_Chance_Handler;

   procedure NMI_Handler is
   begin
      loop
         null;
      end loop;
   end NMI_Handler;

   procedure HardFault_Handler is
   begin
      loop
         null;
      end loop;
   end HardFault_Handler;

   procedure BusFault_Handler is
   begin
      loop
         null;
      end loop;
   end BusFault_Handler;

   procedure UsageFault_Handler is
   begin
      loop
         null;
      end loop;
   end UsageFault_Handler;

   procedure MemManage_Handler is
   begin
      loop
         null;
      end loop;
   end MemManage_Handler;

end Exception_Handler;
