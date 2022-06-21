with Interfaces; use Interfaces;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams; use Ada.Streams;

package Data_Types is
	-- Buffers / Storage Array
	type Discard_Array is array (Natural range <>) of Unsigned_8;

	-- Hash Types
	-- Hash containing encoded Package ID and Entry ID
	subtype Package_Entry_Hash is Unsigned_32;
		-- Layout: 1000000 XXXXXXXXXXXX YYYYYYYYYYYYY, where X is used to encode
		-- Package ID (see util.adb for interpretation) and Y to encode Entry ID
		-- (with no complications)

	-- Unique ID for a String (generally stable through versions)
	subtype String_Hash is Unsigned_32;

	-- Mode Type
	type Mode_Type is (d1, d2, d2s17);

	-- Header Types
	-- Reference File Header: (*.ref) categorises strings by language
	-- This changes length whenever new languages are added
	type Ref_Header (Mode : Mode_Type) is record
		File_Length : Unsigned_32; -- 0 .. 3
			-- TODO: Determine if 64 bit value (check big endian files)
		Discard_1 : Discard_Array (4 .. 16#17#);
		English_Hash : Package_Entry_Hash; -- 18 .. 1B
		Japanese_Hash : Package_Entry_Hash; -- 1C .. 1F
		German_Hash : Package_Entry_Hash; -- 20 .. 23
		French_Hash : Package_Entry_Hash; -- 24 .. 27
		Spanish_LA_Hash : Package_Entry_Hash; -- 28 .. 2B
		Spanish_ES_Hash : Package_Entry_Hash; -- 2C .. 2F, region-locked in D1
		Italian_Hash : Package_Entry_Hash; -- 30 .. 33
		Korean_Hash : Package_Entry_Hash; -- 34 .. 37, region-locked in D1
		Chinese_Traditional_Hash : Package_Entry_Hash; -- 38 .. 3B,
			-- region-locked in D1
		Chinese_Simplified_Hash : Package_Entry_Hash; -- 3C .. 3F,
			-- region-locked in D1
		Portuguese_Hash : Package_Entry_Hash; -- 40 .. 43
		Polish_Hash : Package_Entry_Hash; -- 44 .. 47, region-locked in D1
		Russian_Hash : Package_Entry_Hash; -- 48 .. 4B,
			-- does not actually exist in D1

		case Mode is
			when d1 =>
				Discard_2_D1 : Discard_Array (16#4C# .. 16#4F#);
				Num_Hashes_D1 : Unsigned_32; -- 50 .. 53
				Discard_3_D1 : Discard_Array (16#54# .. 16#5F#);
			when d2 | d2s17 =>
				Discard_2_D2 : Discard_Array (16#4C# .. 16#5F#);
				Num_Hashes_D2 : Unsigned_32; -- 60 .. 63
				Discard_3_D2 : Discard_Array (16#64# .. 16#6F#);

		end case;
	end record;

	-- Bank File Header: (*.str) stores raw string data
	-- A custom Read attribute is provided (see body)
	type Bank_Header (Mode : Mode_Type) is record
		File_Length : Unsigned_32;
		Num_Metas : Unsigned_32;
		Num_Strings : Unsigned_32;
		Offset_Meta : Unsigned_32; -- Will be processed by the procedure
	end record
	with
		Read => Read_Bank_Header;

	-- Stores information about Entries
	type Meta_Header is record
		Offset_Entry : Unsigned_64; -- 0 .. 7, add BH.Offset_Meta + num * 0x10
		Num_Entries : Unsigned_32; -- 8 .. B
		Discard : Discard_Array (16#C# .. 16#F#);
	end record;

	-- Stores information about an encoded string
	type Entry_Header is record
		Discard_1 : Discard_Array (0 .. 7);
		Offset_String : Unsigned_32; -- 8 .. B
		Discard_2 : Discard_Array (16#C# .. 16#13#);
		Read_Length : Unsigned_16; -- 14 .. 15
		Discard_3 : Discard_Array (16#16# .. 16#17#);
		Obfuscator : Unsigned_16; -- 18 .. 19
		Discard_4 : Unsigned_8; -- 1A
		Decode_Mode : Unsigned_8; -- 1B, see below
		Discard_5 : Discard_Array (16#1C# .. 16#1F#);
	end record;

	-- Decode Mode Constants
	Decode_UTF_8_Clear : constant Unsigned_8 := 0; -- No Obfuscator
	Decode_UTF_8 : constant Unsigned_8 := 244; -- Requires Obfuscator
	Decode_UTF_16LE : constant Unsigned_8 := 240; -- No Obfuscator

	-- Packages
	package Entry_Lists is new Ada.Containers.Doubly_Linked_Lists
		(Element_Type => Entry_Header);

	-- Storage Types
	-- Stores list of entries for each meta
	type Entries_Array is array (Positive range <>) of Entry_Lists.List;

	-- Stores list of string hashes in ref file
	type Hash_Array is array (Positive range <>) of String_Hash;

	-- Stores list of collected metas
	type Meta_Array is array (Positive range <>) of Meta_Header;

	-- Subprograms
	procedure Read_Bank_Header (S : not null access Root_Stream_Type'Class;
		BH : out Bank_Header);
end Data_Types;
