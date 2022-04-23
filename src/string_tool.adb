with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Interfaces; use Interfaces;

procedure String_Tool is
	-- Packages
	package Unsigned_16_IO is new Modular_IO (Num => Unsigned_16);

	-- Types
	subtype String_Hash is Unsigned_32;
	type Hash_Array is array (Positive range <>) of String_Hash;

	-- Enum Type for Languages
	type Language_Type is (English, Japanese, German, French, Spanish_LA, Spanish_ES, Italian, Portuguese, Polish, Russian, Symbols);

	-- Buffers / Storage Arrays
	type Discard_Array is array (Natural range <>) of Unsigned_8;
	type Data_Array is array (Positive range <>) of Unsigned_8;

	-- Reference File Header: (*.ref) categorises strings by language
	type Ref_Header is record
		Discard_1 : Discard_Array (0 .. 16#17#);	
		English_Hash : String_Hash; -- 18 .. 1B
		Japanese_Hash : String_Hash; -- 1C .. 1F
		German_Hash : String_Hash; -- 20 .. 23
		French_Hash : String_Hash; -- 24 .. 27
		Spanish_LA_Hash : String_Hash; -- 28 .. 2B
		Spanish_ES_Hash : String_Hash; -- 2C .. 2F
		Italian_Hash : String_Hash; -- 30 .. 33
		Symbols_Hash : String_Hash; -- 34 .. 37, believed to be Destiny icon characters
		Unknown_2 : String_Hash; -- 38 .. 3B, appears to contain weapon manufacturer names
		Unknown_3 : String_Hash; -- 3C .. 3F, still no idea if valid
		Portuguese_Hash : String_Hash; -- 40 .. 43
		Polish_Hash : String_Hash; -- 44 .. 47
		Russian_Hash : String_Hash; -- 48 .. 4B
		Discard_2 : Discard_Array (16#4C# .. 16#5F#);
		Num_Hashes : Unsigned_32; -- 60 .. 63
		Discard_3 : Discard_Array (16#64# .. 16#6F#);
	end record;

	-- Bank File Header: (*.str) stores raw string data
	type Bank_Header is record
		File_Length : Unsigned_32; -- 0 .. 3
		Discard_1 : Discard_Array (4 .. 7);
		Num_Metas : Unsigned_32; -- 8 .. B
		Discard_2 : Discard_Array (16#C# .. 16#47#);
		Num_Strings : Unsigned_32; -- 48 .. 4B
		Discard_3 : Discard_Array (16#4C# .. 16#4F#);
		Offset_Meta : Unsigned_32; -- 50 .. 53, add 0x60
	end record;

	-- Stores information about Entries
	type Meta_Header is record
		Offset_Entry : Unsigned_64; -- 0 .. 7, add BH.Offset_Meta + num * 0x10
		Num_Entries : Unsigned_32; -- 8 .. B
		Discard : Discard_Array (16#C# .. 16#F#);
	end record;

	type Meta_Array is array (Positive range <>) of Meta_Header;

	-- Stores information about an encoded string
	type Entry_Header is record
		Discard_1 : Discard_Array (0 .. 7);
		Offset_String : Unsigned_32; -- 8 .. B
		Discard_2 : Discard_Array (16#C# .. 16#13#);
		Read_Length : Unsigned_16; -- 14 .. 15
		Discard_3 : Discard_Array (16#16# .. 16#17#);
		Obfuscator : Unsigned_16; -- 18 .. 19
		Discard_4 : Discard_Array (16#1A# .. 16#1F#);
	end record;

	package Entry_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Entry_Header);

	-- Stores list of entries for each meta
	type Entries_Array is array (Positive range <>) of Entry_Lists.List;

	-- Local Variables
	SE : Search_Type;
	D : Directory_Entry_Type;

	-- Subprograms
	-- Return Package ID given Hash
	function Package_ID (Hash : String_Hash) return Unsigned_16 is
		R : Unsigned_32;
	begin
		if Hash < 16#80FFFFFF# then
			R := Shift_Right (Hash, 16#D#) and 16#3FF#;
		else
			R := (Shift_Right (Hash, 16#D#) and 16#3FF#) or 16#400#;
		end if;

		if R = 16#519# or R = 16#539# then
			return Unsigned_16 (R + 16#400#);
		else
			return Unsigned_16 (R);
		end if;
	end Package_ID;

	-- Return Entry ID given Hash
	function Entry_ID (Hash : String_Hash) return Unsigned_16 is (Unsigned_16 (Hash and 16#1FFF#));

	-- Print Hex String for Unsigned_16
	function Hex_String (Num : Unsigned_16) return String is
		S : String (1 .. 8); -- 16#XXXX#
		First : Natural := 0;
		Last : Natural := 0;
		O : String (1 .. 4) := "0000"; -- XXXX
		Pad : Natural := 0;
	begin
		Unsigned_16_IO.Put(S, Num, 16);

		-- First is first X, last last X in XXXX, XXX, XX, or X
		for I in S'Range loop
			if S (I) = '#' then
				if First = 0 then
					First := I + 1;
				else
					Last := I - 1;
				end if;
			end if;
		end loop;

		-- Fill in only used digits to ensure consistent length
		Pad := 3 - (Last - First);
		for I in First .. Last loop
			O (Pad + I - First + O'First) := S (I);
		end loop;

		return To_Lower (O);
	end Hex_String;

	-- Decode Destiny-format Strings
	function Decode_String (DA : Data_Array; Obf : Unsigned_16) return String is
		S : String (DA'Range);
	begin
		-- Check for special Obfuscators
		case Obf is
			when 16#E142# => return "[Arc Kill]";
			when 16#E13F# => return "[Solar Kill]";
			when 16#E143# => return "[Void Kill]";
			-- TODO: Find [Stasis Kill]
			when others => null;	
		end case;

		-- Convert Data Array to String
		for I in DA'Range loop
			S (I) := Character'Val (Natural (DA (I)));
		end loop;
	
		-- Convert String (UTF-8) to UTF-32 WW_String
		declare
			WW : Wide_Wide_String := Decode(S);
		begin
			for I in WW'Range loop
				WW (I) := Wide_Wide_Character'Val (Wide_Wide_Character'Pos (WW (I)) + Obf);
			end loop;

			return Encode (WW);
		end;
	exception
		-- Occasionally input data can be invalid UTF-8. TODO: Add real workaround.
		-- This primarily affects Japanese, but sometimes French as well
		-- Please open a Pull Request or Issue if you have any insight
		when Ada.Strings.UTF_Encoding.Encoding_Error => return "[Decode Error]";
	end Decode_String;
begin
	Put_Line (Standard_Error, "Destiny String Tool v0.4");

	-- Check for sufficient arguments
	case Argument_Count is
		when 1 | 2 => null;
		when others =>
			Put_Line ("Usage: " & Command_Name & "[LANGUAGE] STRING_DIR");
			return;
	end case;
	

	-- Main loop
	declare
		Language : String renames Argument (1);
		String_Dir : String renames Argument (Argument_Count);
		RH : Ref_Header;
		RF : Stream_IO.File_Type;
		RS : Stream_Access;
		Chosen : String_Hash; -- Chosen language string hash
	begin
		Start_Search (SE, String_Dir, "*.ref");
		while More_Entries (SE) loop
			<<Read_Entry>>
			Get_Next_Entry (SE, D);	
			Put_Line (Standard_Error, "[Info] Processing " & Full_Name (D));
			Open (RF, In_File, Full_Name (D));
			RS := Stream (RF);

			-- Read Reference File Header
			if (Ref_Header'Size / 8) > Size (RF) then
				Put_Line (Standard_Error, "[Error] Reference file too small");
				Close (RF);
				if More_Entries (SE) then
					goto Read_Entry;
				else
					exit;
				end if;
			end if;

			Ref_Header'Read (RS, RH);

			-- Select language
			if Argument_Count = 2 then
				Chosen := (case Language_Type'Value (Language) is
					when English => RH.English_Hash,
					when Japanese => RH.Japanese_Hash,
					when German => RH.German_Hash,
					when French => RH.French_Hash,
					when Spanish_LA => RH.Spanish_LA_Hash,
					when Spanish_ES => RH.Spanish_ES_Hash,
					when Italian => RH.Italian_Hash,
					when Portuguese => RH.Portuguese_Hash,
					when Polish => RH.Polish_Hash,
					when Russian => RH.Russian_Hash,
					when Symbols => RH.Symbols_Hash
				);
			else
				Chosen := RH.English_Hash;
			end if;

			declare
				HA : Hash_Array (1 .. Natural (RH.Num_Hashes));
				BF : Stream_IO.File_Type;
				BS : Stream_Access;
				BH : Bank_Header;
			begin
				Hash_Array'Read (RS, HA);
				Close (RF);

				-- Start handling bank file
				Open (BF, In_File, String_Dir & "/" & Hex_String (Package_ID (Chosen)) & "-" & Hex_String (Entry_ID (Chosen)) & ".str");
				BS := Stream (BF);
				Bank_Header'Read (BS, BH);
				BH.Offset_Meta := BH.Offset_Meta + 16#60#; -- Add 0x60 to offset meta to make it match the real meta offset

				-- Set index to Meta Offset
				Set_Index (BF, Stream_IO.Positive_Count (BH.Offset_Meta + 1));

				-- Read meta headers
				declare
					MA : Meta_Array (1 .. Natural (BH.Num_Metas));
				begin
					-- Manually read in each Meta Header since Offset Entry must be adjusted
					for M in MA'Range loop
						if Size (BF) - Index (BF) < (Meta_Header'Size / 8) - 1 then
							Put_Line (Standard_Error, "[Error] Prematurely stopped reading metas.");	
							MA (M).Offset_Entry := 0;
							MA (M).Num_Entries := 0;
						else
							Meta_Header'Read (BS, MA (M));
							MA (M).Offset_Entry := Unsigned_64 (BH.Offset_Meta) + MA (M).Offset_Entry + Unsigned_64 ((M - 1) * 16#10#);
							-- Note: This relies on overflowing an unsigned 64-bit integer!
						end if;
					end loop;

					-- Read Entry headers (potentially multiple for each Meta Header)
					declare
						EA : Entries_Array (1 .. Natural (BH.Num_Metas));
						EH : Entry_Header;
						I : Positive := HA'First; -- Used as String Hash Index (Index to HA)
					begin
						for M in EA'Range loop
							Set_Index (BF, Stream_IO.Positive_Count (MA (M).Offset_Entry + 1));
							for E in 1 .. MA (M).Num_Entries loop
								if Size (BF) - Index (BF) < (Entry_Header'Size / 8) - 1 then
									Put_Line (Standard_Error, "[Error] Prematurely stopped reading entries.");
									EH.Offset_String := 0;
									EH.Read_Length := 0;
								else
									Entry_Header'Read (BS, EH);
									EH.Offset_String := EH.Offset_String + Unsigned_32 (Index (BF)) - 25;
									Entry_Lists.Append (EA (M), EH);
								end if;
							end loop;
						end loop;

						-- Print strings
						for M in EA'Range loop
							for E of EA (M) loop
								declare
									DA : Data_Array (1 .. Natural (E.Read_Length));
								begin
									if E.Read_Length > 0 then
										Set_Index (BF, Stream_IO.Positive_Count (E.Offset_String + 1));
										Data_Array'Read (BS, DA);
										if I <= HA'Last then
											Put_Line (String_Hash'Image (HA (I)) & ": " & Decode_String (DA, E.Obfuscator));
										else
											Put_Line ("UNK" & ": " & Decode_String (DA, E.Obfuscator));
										end if;
									end if;
								end;
							end loop; -- TODO: Should it go here?
								I := I + 1; -- Increment String Hash Index
						end loop;
						Close (BF);
					end;
				end;
			end;
		end loop;
		End_Search (SE);
	end;
end String_Tool;
