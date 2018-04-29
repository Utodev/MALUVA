PROGRAM SC2DRG;

{$MODE OBJFPC}

USES sysutils;

CONST TWO_KILOBYTES = 2048;

TYPE cpcHeaderType =  ARRAY[0..127] of Byte;

PROCEDURE Syntax();
BEGIN
	Writeln();
	WriteLn('SYNTAX: SC2DAAD <target> <inputFilename> <outputFilename> <lines>');
	Writeln();
	Writeln('Target may be cpc, msx or zx');
	WriteLn('Type SC2DAAD <target> HELP to get specific help per each platform.');
	Writeln();
	Halt(1);
END;


PROCEDURE Help(target: string);
BEGIN
    IF target='cpc'	THEN
    BEGIN
		WriteLn('For CPC you will requiere a CPC .SCR file together with a .PAL file for the palette, both with same base name.');
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8 and less or equal to 96.');
		WriteLn('The file created would already have a CPC header.');
		WriteLn('Note: although max lines is 96, having a 200 lines picture (full screen) is allowed. Just can''t create an image in the 97-199 range');
		Halt(0);
	END;
    IF target='zx'	THEN
    BEGIN
		WriteLn('For Spectrum you will requiere a Spectrum .SCR file (6912 bytes).');
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8.');
		WriteLn('The file created would not have a +3DOS header, you will have to add it IF you use the graphic in a Spectrum +3 environment. Don''t add a header IF you are going to use the graphic under ESXDOS or compatible.');
		Halt(0);
	END;
    IF target='msx'	THEN
    BEGIN
	END;
	Writeln('Unknown target.');
	Halt(1);
END;	


PROCEDURE Error(S:String);
BEGIN
	Writeln(S + '.');
	Writeln();
	Syntax;
END;



PROCEDURE WriteCPCHeaderWord(var header: cpcHeaderType;index: Byte; val: Word);
BEGIN
  header[index] := val MOD 256;
  header[index+1] := val DIV 256;
END;

PROCEDURE CopyBlock(VAR inFile, outFile:FILE; size:word);
VAR buffer: ARRAY [0..$8000] OF byte;
BEGIN
	Blockread(inFile, Buffer, size);
	BlockWrite(outFile, Buffer, size)
END;


FUNCTION GetCPCFirmwarePalette(HwValue:Byte):Byte;
BEGIN
	CASE HwValue OF
	 $54: Result:= 0;
	 $44: Result:= 1;
	 $55: Result:= 2;
	 $5C: Result:= 3;
	 $58: Result:= 4;
	 $5D: Result:= 5;
	 $4C: Result:= 6;
	 $45: Result:= 7;
	 $4D: Result:= 8;
	 $56: Result:= 9;
	 $46: Result:= 10;
	 $57: Result:= 11;
	 $5E: Result:= 12;
	 $40: Result:= 13;
	 $5F: Result:= 14;
	 $4E: Result:= 15;
	 $47: Result:= 16;
	 $4F: Result:= 17;
	 $52: Result:= 18;
	 $42: Result:= 19;
	 $53: Result:= 20;
	 $5A: Result:= 21;
	 $59: Result:= 22;
	 $5B: Result:= 23;
	 $4A: Result:= 24;
	 $43: Result:= 25;
	 $4B: Result:= 26;
	 $41: Result:= 13;
	 $48: Result:= 7;
	 $49: Result:= 24;
	 $50: Result:= 1;
	 $51: Result:= 19;
	ELSE
	 Result := 0;
	END; 
END;

PROCEDURE CPC(inputFilename, outputFilename: string; lines:word;NumPaletteEntries:byte);
VAR i, bytesToRead, current2KBuffercount: word;
	palFileName:String;
	inFile, outFile: FILE;
	TempBuffer: ARRAY [0..2047] OF byte;
	Palette : ARRAY[0..15] OF byte;
	cpcHeader : cpcHeaderType;
	AuxStr: String;
	CheckSum: Word;
	ScansPer2kBuffer: Byte;
	FileSize : Word;
	(* 
	 CPC FILE FORMAT IS (after the AMSDOS header):
	 00    : scans per 2k buffer, 
	 01-04 : palette used, first one is for background, second one for text ink
	 05-06  :number of lines multiplied by 10
	 07-   : data as in a SCR file, but with less lines - or not.

	 Exception: 200 lines graphic contains 16384 bytes, 16384 for data, and 4 for palette, but the palette is over the last 4 bytes of data, where
	 the spare 48 bytes are. 16384 in CPC header and use CAS_IN_DIRECT to load directly to VRAM, then set the palette (better set all palette to black first
	 to avoid strange colours on screen)
	 *)
	
BEGIN
	palFileName := changeFileExt(inputFilename,'.pal');
	AssignFile(inFile, palFileName);  
	TRY
		Reset(inFile, 1);
		Seek(inFile, 3+128);
		Blockread(inFile, Palette[0], 1);
		Seek(inFile, 15+128);
		Blockread(inFile, Palette[1], 1);
		Seek(inFile, 27+128);
		Blockread(inFile, Palette[2], 1);
		Seek(inFile, 39+128);
		Blockread(inFile, Palette[3], 1);
	EXCEPT
		on E: Exception DO Error('PAL file not found, the .PAL file for the image should be located at same folder than SCR file.' + E.Message);
	END; 
	CloseFile(inFile);

	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	Seek(inFile, 128); // Skip AMSDOS header
	EXCEPT
	 on E: Exception DO Error('Invalid file.');
	END; 

	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid file.');
	END; 
	// Prepare Header
	IF Lines=200 THEN FileSize := 16384
				 ELSE FileSize := 7 + lines*80;

	FillChar(cpcHeader, 128, 0);
	AUXSTR:='UTO DEV HCK';
	Move(AuxStr[1],cpcHeader[1],11);
	cpcHeader[18]:=2; //File type binary
	WriteCPCHeaderWord(cpcHeader,24, FileSize);
	WriteCPCHeaderWord(cpcHeader,64, FileSize);
	Checksum := 0;
	for i:=0 to 66 do Checksum := Checksum + cpcHeader[i];
	WriteCPCHeaderWord(cpcHeader,67, Checksum);


	//Save header
	BlockWrite(outFile, cpcHeader, 128);		

	FOR i:= 0 to NumPaletteEntries-1 do Palette[i]:=GetCPCFirmwarePalette(Palette[i]);



	IF lines=200 THEN //Full screen image, just save all 16.000 bytes plus palette
	BEGIN
		FOR I:=1 to 8 DO CopyBlock(inFile, outFile, 2048);
		Seek(outFile, 16384 - NumPaletteEntries + 128);
		BlockWrite(outFile, Palette, NumPaletteEntries);
	END
	ELSE
	BEGIN             // Partial file, more tricky
		BytesToRead := lines * 10;

		ScansPer2kBuffer := TWO_KILOBYTES DIV BytesToRead;
		// Save number of lines and palette
		BlockWrite(outFile, ScansPer2kBuffer, 1);
		BlockWrite(outFile, Palette, 4);
		BlockWrite(outFile, BytesToRead, 2);		

		current2KBuffercount := 7;
	    FOR i:= 1 TO 8 DO
		BEGIN
		 	Blockread(inFile, TempBuffer, TWO_KILOBYTES);
		 	BlockWrite(outFile, TempBuffer, BytesToRead);
		 	current2KBuffercount := current2KBuffercount + bytesToRead;
		 	IF (current2KBuffercount+bytesToRead > TWO_KILOBYTES) THEN
		 	BEGIN
		 		Fillchar(TempBuffer, TWO_KILOBYTES - current2KBuffercount, 0);
		 		BlockWrite(outFile, TempBuffer, TWO_KILOBYTES - current2KBuffercount);
		 		current2KBuffercount := 0;
		 	END;
		END;
	END;
	CloseFile(inFile);
	CloseFile(outFile);
END;

PROCEDURE MSX(inputFilename, outputFilename: string; lines:byte);
BEGIN
END;



PROCEDURE ZX(inputFilename, outputFilename: string; lines:byte);
VAR i,rows, bytesToRead, bytesToSkip, attributesSize: word;
	inFile, outFile: FILE;
	TempBuffer: ARRAY [0..255] OF byte;
	(* 
	 ZX FILE FORMAT IS:
	 00    : number of lines
	 01-   : data as in a SCR file, but with less lines - or not - for both pixels and attributes.
	*)
BEGIN
	attributesSize := (lines DIV 8) * 32;
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid file.');
	END; 
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid file.');
	END; 

	BlockWrite(outFile, lines, 1);

	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // First third
	 lines := lines - 64;
	END;
	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // Second third
	 lines := lines - 64;
	END;
	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // Third third
	 lines := lines - 64;
	END;

	IF lines<>0 THEN
	BEGIN
	 rows := lines DIV 8;
	 bytesToRead := rows * 32;
	 bytesToSkip := (8 - rows) * 32;
	 FOR i:= 1 TO 8 DO
	 BEGIN
	 	CopyBlock(inFile, outFile, bytesToRead);
	 	Blockread(inFile, TempBuffer, bytesToSkip);
	 END;
	END;
	
	Seek(inFile, TWO_KILOBYTES*3); //Move to attributes area


	CopyBlock(inFile, outFile, attributesSize);

	CloseFile(inFile);
	CloseFile(outFile);
END;



VAR target, inputFilename, outputFilename: string;
	lines : word;
	NumPaletteEntries : Byte;
	LinesString: string;

BEGIN
	IF ParamCount()<>4 THEN Syntax();
	target := ParamStr(1);
	inputFilename := ParamStr(2);
	outputFilename := ParamStr(3);

	// Check param Errors

    IF (target<>'cpc') AND (target<>'zx') AND (target<>'msx') THEN Error('Invalid target.');

    IF (uppercase(inputFilename) = 'HELP' ) THEN Help(target);	

    NumPaletteEntries := 4;
    LinesString := ParamStr(4);
    IF (uppercase(LinesString)='MODE1') THEN
    BEGIN
    	LinesString := '200';
    	NumPaletteEntries := 16;
    END;

	TRY
		lines := StrToInt(LinesString);
    EXCEPT
        On E : EConvertError DO Error('Invalid number of lines');
    END;

    IF ((target='zx') OR (target='msx')) AND (lines >192) THEN Error('Maximum bnumber of lines for MSX/ZX is 192');
    IF (target='cpc') AND (lines >96) AND (lines <>200) THEN Error('Maximum number of lines for CPC is 96, althought you can choose full screen with 200 lines');

    IF ((lines MOD 8) <> 0) THEN Error('Number of lines must be a multiple of 8');

    IF (NOT FileExists(inputFilename)) THEN Error('File not found');

    // Let's do it

    IF target = 'zx' THEN ZX(inputFilename, outputFilename, lines) 
    ELSE IF target = 'cpc' THEN CPC(inputFilename, outputFilename, lines, NumPaletteEntries) 
    ELSE IF target = 'msx' THEN MSX(inputFilename, outputFilename, lines);

END.

