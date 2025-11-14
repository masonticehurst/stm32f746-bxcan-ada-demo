with HAL.CAN; use HAL.CAN;
with STM32_SVD;
with STM32_SVD.CAN;

package STM32.CAN_Internal is

   type Internal_CAN_Port is private;

   type CAN_Port (Periph : not null access Internal_CAN_Port) is
   new HAL.CAN.CAN_Port with null record;

private
   type Internal_CAN_Port is new STM32_SVD.CAN.CAN_Peripheral;
end STM32.CAN_Internal;
