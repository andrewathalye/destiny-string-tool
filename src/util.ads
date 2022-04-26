with Interfaces; use Interfaces;

with Data_Types; use Data_Types;

package Util is
	-- Exceptions
	Invalid_Package_Exception : exception;

	-- Subprograms
	function Package_ID (Hash : Package_Entry_Hash) return Unsigned_16;
	function Entry_ID (Hash : Package_Entry_Hash) return Unsigned_16;
	function Hex_String (Num : Unsigned_16) return String;
	function Decipher (S : String; Obf : Unsigned_16) return String;
end Util;
