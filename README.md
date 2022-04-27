Destiny Strings Extractor
=========================

This is a Destiny String Extractor written in Ada.  
It pairs best with the Destiny Unpacker located at  
https://www.github.com/andrewathalye/destiny-unpacker-linux  

Please let me know if you find any bugs. You can use it as follows:  
`./stringtool D1 | D2 [LANGUAGE] PATH_TO_STR_SUBDIRECTORY 2>/dev/null > output_dump.txt`  
Valid language values are:  
	English, Japanese, German, French, Spanish_LA, Spanish_ES, Italian, Portuguese, Polish, Russian, Korean, Chinese_Traditional, Chinese_Simplified  
If Language is omitted, English will be chosen.  
Please note that for Destiny 1, Russian is not available and Polish, Chinese (both variants), Spanish_ES, and Korean have region-locked access.

Known Issues
------------
I am not a native Spanish speaker, so I can't tell whether I have labelled Spanish_LA and Spanish_ES correctly. If you can
correct me, please do let me know whether I got it wrong or right.  
