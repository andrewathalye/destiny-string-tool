with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Util is
	-- Subprograms
	-- Return Package ID given Hash. See data_types.ads for more information
	function Package_ID (Hash : Package_Entry_Hash) return Unsigned_16 is
		ID : constant Unsigned_16 := Unsigned_16 (Shift_Right (Hash, 16#D#) and 16#FFF#);
	begin
		-- Put_Line ("[Debug] Hash ID was " & Package_Entry_Hash'Image (Hash));
		-- Put_Line ("[Debug] Package portion was " & Unsigned_16'Image (ID));
		if (ID and 16#800#) > 0 and (ID and 16#400#) > 0 then -- If bit 12 and 11 set, value is encoded with bit 11 unset
			return ID and 16#BFF#;
		elsif (ID and 16#400#) = 0 then -- If bit 11 unset, set bit 11 and unset bit 10
			return (ID and 16#3FF#) or 16#400#;
		elsif (ID and 16#200#) = 0 then -- If bit 11 set and bit 10 unset, value is encoded with both unset
			return ID and 16#1FF#;
		elsif (ID and 16#400#) > 0 then -- If bit 11 set and bit 10 set, value is encoded with bit 11 unset
			return ID and 16#3FF#; 
		else 
			raise Invalid_Package_Exception with "Unknown package encoding configuration.";
		end if;
	end Package_ID;

	-- Return Entry ID given Hash
	function Entry_ID (Hash : Package_Entry_Hash) return Unsigned_16 is (Unsigned_16 (Hash and 16#1FFF#));

	-- Print Unsigned_16 as big endian hex string
	function Hex_String (Num : Unsigned_16) return String is
		-- Hex Digit Array
		Hex_Digits : constant array (0 .. 15) of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
	begin
		return	(Hex_Digits (Natural (Shift_Right (Num and 16#f000#, 12))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00#, 8))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0#, 4))),
			Hex_Digits (Natural (Num and 16#f#)));
	end Hex_String;

	-- Decode Obfuscated UTF-8 strings
	function Decipher (S : String; Obf : Unsigned_16) return String is
	begin
		-- Check for special Obfuscators
		case Obf is
			when 16#E142# => return "[Arc Kill]";
			when 16#E13F# => return "[Solar Kill]";
			when 16#E143# => return "[Void Kill]";
			-- TODO: Find [Stasis Kill]
			when others => null;	
		end case;

		-- Convert String (UTF-8) to UTF-32 WW_String
		declare
			WW : Wide_Wide_String := Decode(S);
		begin
			for I in WW'Range loop
				WW (I) := Wide_Wide_Character'Val (Wide_Wide_Character'Pos (WW (I)) + Obf);
			end loop;

			return Encode (WW);
		end;
	end Decipher;
end Util;
