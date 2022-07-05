Destiny String Extractor
=========================

This is a Destiny String Extractor written in Ada.  
It pairs best with the Destiny Unpacker located at  
https://www.github.com/andrewathalye/destiny-unpacker-ada

Please let me know if you find any bugs. You can use it as follows:  
`./stringtool [OPTIONS] > output_dump.txt`  
Alternatively, from Windows:  
`stringtool_static [OPTIONS] > output_dump.txt`  

Run the program without arguments to see the available options.

Reference
---------

Valid language values are:   
	English, Japanese, German, French, Spanish_LA, Spanish_ES, Italian, Portuguese, Polish, Russian, Korean, Chinese_Traditional, Chinese_Simplified  
Valid version values are:  
	d1, d2, d2s17

Please note that for Destiny 1, Russian is not available and Polish, Chinese (both variants), Spanish\_ES, and Korean have region-locked access.

Building From Source
--------------------

To build from source, you'll need GNAT FSF >=12 and GPRBuild.  
Run `gprbuild -Xmode=static` to produce a release build,
or omit the mode to produce a development build (smaller and faster, but not distributable).
