with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

procedure String_Tool is
	-- Packages
	package Unsigned_16_IO is new Modular_IO (Num => Unsigned_16);

	-- Types
	subtype String_Hash is Unsigned_32;
	type Hash_Array is array (Positive range <>) of String_Hash;

	-- Buffers / Storage Arrays
	type Discard_Array is array (Natural range <>) of Unsigned_8;
	type Data_Array is array (Positive range <>) of Unsigned_8;

	-- Reference File Header: (*.ref) categorises strings by language
	type Ref_Header is record
		Discard_1 : Discard_Array (0 .. 16#17#);	
		English_Hash : String_Hash; -- 18 .. 1B
		Unknown_1 : String_Hash; -- 1C .. 1F
		German_Hash : String_Hash; -- 20 .. 23
		Unknown_2 : String_Hash; -- 24 .. 27
		Unknown_3 : String_Hash; -- 28 .. 2B
		Spanish_Hash : String_Hash; -- 2C .. 2F
		Discard_2 : Discard_Array (16#30# .. 16#5F#);
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
		Offset_Entry : Unsigned_64; -- 0 .. 7
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
		String_Length : Unsigned_16; -- 16 .. 17
		Obfuscator : Unsigned_16; -- 18 .. 19
		Discard_3 : Discard_Array (16#1A# .. 16#1F#);
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

begin
	Put_Line ("Destiny String Tool v0.2");

	-- Check for sufficient arguments
	if Argument_Count /= 1 then
		Put_Line ("Usage: " & Command_Name & " STRING_DIR");
		return;
	end if;
	

	-- Main loop
	declare
		String_Dir : String renames Argument (1);
		RH : Ref_Header;
		RF : Stream_IO.File_Type;
		RS : Stream_Access;
	begin
		Start_Search (SE, String_Dir, "*.ref");
		while More_Entries (SE) loop
			Get_Next_Entry (SE, D);	
			Put_Line ("[Info] Processing " & Full_Name (D));
			Open (RF, In_File, Full_Name (D));
			RS := Stream (RF);

			-- Read Reference File Header
			Ref_Header'Read (RS, RH);
			Put_Line ("Raw English Hash: " & String_Hash'Image (RH.English_Hash));
			Put_Line ("Tentative file path: " & String_Dir & "/" & Hex_String (Package_ID (RH.English_Hash)) & "-" & Hex_String (Entry_ID (RH.English_Hash)) & ".str");
			declare
				HA : Hash_Array (1 .. Positive (RH.Num_Hashes));
				BF : Stream_IO.File_Type;
				BS : Stream_Access;
				BH : Bank_Header;
			begin
				Hash_Array'Read (RS, HA);
				Close (RF);

				Put_Line ("[Debug] Hashes read, open BF");

				-- Start handling bank file
				Open (BF, In_File, String_Dir & "/" & Hex_String (Package_ID (RH.English_Hash)) & "-" & Hex_String (Entry_ID (RH.English_Hash)) & ".str");
				BS := Stream (BF);
				Bank_Header'Read (BS, BH);

				Put_Line ("[Debug] BH read, set index and read meta");
				Set_Index (BF, Stream_IO.Positive_Count (BH.Offset_Meta + 1));

				-- Read meta headers
				declare
					MA : Meta_Array (1 .. Positive (BH.Num_Metas));
				begin
					Meta_Array'Read (BS, MA);
					Put_Line ("[Debug] Meta Array read");

					-- Read Entry headers (potentially multiple for each Meta Header)
					declare
						EA : Entries_Array (1 .. Positive (BH.Num_Metas));
						EH : Entry_Header;
					begin
						for M in EA'Range loop
							Set_Index (BF, Stream_IO.Positive_Count (MA (M).Offset_Entry + 1));
							for E in 1 .. MA (M).Num_Entries loop
								Entry_Header'Read (BS, EH);
								Entry_Lists.Append (EA (M), EH);
							end loop;
						end loop;

						-- Print strings
						for M in EA'Range loop
							for E of EA (M) loop
								declare
									DA : Data_Array (1 .. Positive (E.Read_Length));
								begin
									Set_Index (BF, Stream_IO.Positive_Count (E.Offset_String + 1));
									Data_Array'Read (BS, DA);
									-- Put_Line (Decode_String (DA, E.Obfuscator));
								end;
							end loop;
						end loop;
						Close (BF);
					end;
				end;
			end;


			Close (RF);
		end loop;
		End_Search (SE);
	end;
end String_Tool;
