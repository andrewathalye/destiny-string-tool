with Ada.Streams.Stream_IO;
	use Ada.Streams;
	use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

with Common; use Common;

package Bank_Files is
	-- Bank File Header: (*.str) stores raw string data
	-- A custom Read attribute is provided (see body)
	type Bank_Header_Type (Mode : Mode_Type) is record
		File_Length : Unsigned_32;
		Num_Metas : Unsigned_32;
		Num_Strings : Unsigned_32;
		Offset_Meta : Unsigned_32; -- Will be processed by the procedure
	end record
	with
		Read => Read_Bank_Header_Type;

	-- Stores information about one or more Entries
	-- Note: Num_Entries and Loop_Count are intentionally set to 0
	-- in order to avoid reading unitialised memory if Meta reading fails.
	type Meta_Header_Type is record
		Offset_Entry : Unsigned_64; -- 0 .. 7, needs local processing
		Num_Entries : Unsigned_32 := 0; -- 8 .. B
		Loop_Count : Unsigned_32 := 0; -- C .. F, actually Discard, space reused.
	end record;

	-- Stores information about an encoded string
	type Entry_Header_Type is record
		Discard_1 : Discard_Array (0 .. 7);
		Offset_String : Unsigned_32; -- 8 .. B
		Discard_2 : Discard_Array (16#C# .. 16#13#);
		Read_Length : Unsigned_16; -- 14 .. 15
		Discard_3 : Discard_Array (16#16# .. 16#17#);
		Obfuscator : Unsigned_16; -- 18 .. 19
		Discard_4 : Unsigned_8; -- 1A
		Decode_Mode : Unsigned_8; -- 1B, see below
		Hash_Index : Unsigned_32; -- 1C .. 1F, actually Discard, space reused
	end record;

	-- Stores lists of collected components
	type Meta_Array is array (Positive range <>) of Meta_Header_Type;
	type Entry_Array is array (Positive range <>) of Entry_Header_Type;

	-- Subprograms
	-- Version-agnostic implementation of 'Read for Bank headers
	procedure Read_Bank_Header_Type (S : not null access Root_Stream_Type'Class;
		Item : out Bank_Header_Type);

	-- Reads Meta Headers from a file, and returns an array
	function Read_Metas (
		Bank_Header : Bank_Header_Type;
		Bank_File : File_Type;
		Bank_Stream : Stream_Access)
	return Meta_Array;

	-- Reads Entry Headers from a file, and returns an array
	function Read_Entries (
		Metas : Meta_Array;
		Bank_File : File_Type;
		Bank_Stream : Stream_Access)
	return Entry_Array;

end Bank_Files;
