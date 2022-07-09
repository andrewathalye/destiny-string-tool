with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Conversions;
	use Ada.Strings.UTF_Encoding;
	use Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
	use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Interfaces; use Interfaces;

with Config; use Config;

package body String_Display is
	-- Decode Mode Constants
	-- Some strings require additional processing before printing.
	Decode_UTF_8_Clear : constant := 0; -- No Obfuscator
	Decode_UTF_8 : constant := 244; -- Requires Obfuscator
	Decode_UTF_16LE : constant := 240; -- No Obfuscator

	-- Print strings, given Entries, Hashes, and File Data
	procedure Print_Strings (
		Entries : Entry_Array;
		Hashes : Hash_Array;
		Bank_File : Ada.Streams.Stream_IO.File_Type;
		Bank_Stream : Stream_Access)
	is
		-- Subprograms
		-- Decode Obfuscated UTF-8 strings
		function Decipher (S : String; Obf : Unsigned_16) return String is
		begin
			-- Check for special Obfuscators
			-- TODO: Remove / rewrite
			case Obf is
				when 16#E142# => return "[Arc Kill]";
				when 16#E13F# => return "[Solar Kill]";
				when 16#E143# => return "[Void Kill]";
					-- TODO: Find [Stasis Kill]
				when others => null;
			end case;

			-- Convert String (UTF-8) to UTF-32 WW_String
			declare
				WW : Wide_Wide_String := Decode (S);
			begin
				Convert_Strings :
				for I in WW'Range loop
					WW (I) := Wide_Wide_Character'Val (
						Wide_Wide_Character'Pos (WW (I)) + Obf);
				end loop Convert_Strings;

				return Encode (WW);
			end;
		end Decipher;
	begin
		if Debug then
			Put_Line ("[Debug] Print Strings");
		end if;

		Process_Entries :
		for E of Entries loop
			-- Filter out invalid Entries
			if E.Read_Length > 0 then
				Set_Index (
					Bank_File,
					Ada.Streams.Stream_IO.Positive_Count (E.Offset_String + 1));

				-- Process individual Entry
				case E.Decode_Mode is
					when Decode_UTF_8_Clear => -- UTF-8 string
						declare
							S : String (1 .. Natural (E.Read_Length));
						begin
							String'Read (Bank_Stream, S);
							Put_Line (
								String_Hash'Image (Hashes (
									Natural (E.Hash_Index)))
								& ": " & S);
						exception
							when Constraint_Error =>
								Put_Line ("ERR: "
									& S);
						end;

					when Decode_UTF_8 => -- UTF-8 string with cipher applied
						declare
							S : String (1 .. Natural (E.Read_Length));
						begin
							String'Read (Bank_Stream, S);
							Put_Line (
								String_Hash'Image (Hashes (
									Natural (E.Hash_Index)))
								& ": " & Decipher (S, E.Obfuscator));
						exception
							when Constraint_Error =>
								Put_Line ("ERR: "
									& Decipher (S, E.Obfuscator));
						end;

					when Decode_UTF_16LE => -- Ordinary UTF-16LE string
						declare
							WS : UTF_16_Wide_String (1 .. Natural (E.Read_Length));
						begin
							UTF_16_Wide_String'Read (Bank_Stream, WS);
							Put_Line (String_Hash'Image (Hashes (
								Natural (E.Hash_Index)))
							& ": " & Convert (WS, UTF_8));
						exception
							when Constraint_Error => Put_Line ("ERR: " & Convert (WS, UTF_8));
						end;

					when others =>
						begin
							Put_Line (Standard_Error,
								String_Hash'Image (Hashes (
									Natural (E.Hash_Index)))
								& ": [Decode Error]");
						exception
							when Constraint_Error =>
								Put_Line (Standard_Error,
									"ERR: [Decode Error]");
						end;
				end case;
			end if;
		end loop Process_Entries;

		-- Display a message for Strings which aren't present in this translation
		Invalidate_Single_Entries :
		for Entry_Index in Entries'First + 1 .. Entries'Last loop
			Invalidate_Contiguous_Hash_Indices :
			for Hash_Index in Entries (Entry_Index - 1).Hash_Index + 1
				.. Entries (Entry_Index).Hash_Index - 1
			loop
				Put_Line (
					String_Hash'Image (Hashes (
						Natural (Hash_Index)))
					& ": [Unavailable]");
			end loop Invalidate_Contiguous_Hash_Indices;
		end loop Invalidate_Single_Entries;
	end Print_Strings;
end String_Display;
