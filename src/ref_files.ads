with Ada.Streams;

with Interfaces; use Interfaces;

with Common; use Common;

package Ref_Files is
	-- Exceptions
		Invalid_Package_Exception : exception;

	-- Enum Type for Languages
	type Language_Type is (English, Japanese, German, French, Spanish_LA,
		Spanish_ES, Italian, Portuguese, Polish, Russian, Korean,
		Chinese_Traditional, Chinese_Simplified);

	-- Hash containing encoded Package ID and Entry ID
	subtype Package_Entry_Hash is Unsigned_32;
		-- Layout: 1000000 XXXXXXXXXXXX YYYYYYYYYYYYY, where X is used to encode
		-- Package ID (see util.adb for interpretation) and Y to encode Entry ID
		-- (with no complications)

	-- Unique ID for a String (generally stable through versions)
	subtype String_Hash is Unsigned_32;

	-- Stores list of String Hashes in ref file
	type Hash_Array is array (Positive range <>) of String_Hash;

	-- Reference File Header: (*.ref) categorises strings by language
	-- This version-agnostic format uses a custom 'Read attribute
	type Ref_Header_Type (Mode : Mode_Type) is record
		File_Length : Unsigned_32; -- TODO: Determine if 64-bit value
		English_Hash : Package_Entry_Hash;
		Japanese_Hash : Package_Entry_Hash;
		German_Hash : Package_Entry_Hash;
		French_Hash : Package_Entry_Hash;
		Spanish_LA_Hash : Package_Entry_Hash;
		Spanish_ES_Hash : Package_Entry_Hash;
			-- region-locked in D1
		Italian_Hash : Package_Entry_Hash;
		Korean_Hash : Package_Entry_Hash;
			-- region-locked in D1
		Chinese_Traditional_Hash : Package_Entry_Hash;
			-- region-locked in D1
		Chinese_Simplified_Hash : Package_Entry_Hash;
			-- region-locked in D1
		Portuguese_Hash : Package_Entry_Hash;
		Polish_Hash : Package_Entry_Hash; -- region-locked in D1
		Russian_Hash : Package_Entry_Hash; -- absent in D1
		Num_Hashes : Unsigned_32;
	end record
	with
		Read => Read_Ref_Header_Type;

	-- Subprograms
	-- Documentation for these is in the body file
	function Language_Bank (
		Ref_Header : Ref_Header_Type;
		Language : Language_Type)
	return String;

	procedure Read_Ref_Header_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ref_Header_Type);
end Ref_Files;
