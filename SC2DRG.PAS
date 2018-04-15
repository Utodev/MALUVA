program SC2DRG;

{$MODE OBJFPC}

uses sysutils;

procedure Syntax();
begin
	Writeln();
	WriteLn('SYNTAX: SC2DRG <target> <inputFilename> <outputFilename> <lines>');
	Writeln();
	Writeln('Target may be cpc, msx or zx');
	Writeln('Input file must be a CPC .SCR file for CPC, a Spectrum .SCR for Spectrum, or a .SC2 file for MSX');
	Writeln('Lines set the number of lines,from top one, to keep inside the DRG file');
	Writeln();
	Halt(1);
end;

procedure Error(S:String);
begin
	Writeln(S + '.');
	Writeln();
	Syntax;
end;

procedure CopyBlock(var inFile, outFile:file; size:word);
var buffer: array [0..$8000] of byte;
begin
	Blockread(inFile, Buffer, size);
	BlockWrite(outFile, Buffer, size)
end;

procedure CPC(inputFilename, outputFilename: string; lines:byte);
begin
end;

procedure MSX(inputFilename, outputFilename: string; lines:byte);
begin
end;



procedure ZX(inputFilename, outputFilename: string; lines:byte);
var i,rows, bytesToRead, bytesToSkip, attributesSize: word;
	inFile, outFile: file;
	TempBuffer: array [0..255] of byte;
begin
	attributesSize := (lines div 8) * 32;
	AssignFile(inFile, inputFilename);
	try
	Reset(inFile, 1);
	except
	 on E: Exception do Error('Invalid file.');
	end; 
	AssignFile(outFile, outputFilename);
	try
	Rewrite(outFile, 1);
	except
	 on E: Exception do Error('Invalid file.');
	end; 

	BlockWrite(outFile, lines, 1);

	if lines >= 64 then
	begin
	 CopyBlock(inFile, outFile, 2048); // First third
	 lines := lines - 64;
	 Writeln('Copied whole first third.')
	end;
	if lines >= 64 then
	begin
	 CopyBlock(inFile, outFile, 2048); // Second third
	 lines := lines - 64;
	 Writeln('Copied whole second third.')
	end;
	if lines >= 64 then
	begin
	 CopyBlock(inFile, outFile, 2048); // Third third
	 lines := lines - 64;
	 Writeln('Copied whole third third.')
	end;

	if lines<>0 then
	begin
	 rows := lines div 8;
	 bytesToRead := rows * 32;
	 bytesToSkip := (8 - rows) * 32;
	 Writeln('Lines left: ', lines);
	 Writeln('Rows left: ', rows);
	 Writeln('BytesToRead:', bytesToRead);
	 Writeln('BytesToSkip:', bytesToSkip);
 	 Writeln('Copying rest of data [read:', 8*bytesToRead, '] [skip:', 8*bytesToSkip, '] bytes.' );
	 for i:= 1 to 8 do
	 begin
	 	CopyBlock(inFile, outFile, bytesToRead);
	 	Blockread(inFile, TempBuffer, bytesToSkip);
	 	//Seek(inFile, FilePos(inFile) + bytesToSkip);
	 end;
	end;
	
	Seek(inFile, 2048*3); //Move to attributes area


	Writeln('FilePos = ', FilePos(inFile));
	Writeln('Copying attributes [', attributesSize ,'] bytes.');
	CopyBlock(inFile, outFile, attributesSize);

	CloseFile(inFile);
	CloseFile(outFile);
end;



var target, inputFilename, outputFilename: string;
var	lines : word;
begin
	if ParamCount()<>4 then Syntax();
	target := ParamStr(1);
	inputFilename := ParamStr(2);
	outputFilename := ParamStr(3);

	// Check param Errors

    if (target<>'cpc') and (target<>'zx') and (target<>'msx') then Error('Invalid target.');

	try
		lines := StrToInt(ParamStr(4));
    except
        On E : EConvertError do Error('Invalid number of lines');
    end;

    if ((target='zx') or (target='msx')) and (lines >192) then Error('Maximum bnumber of lines for MSX/ZX are 192');
    if (target='cpc') and (lines >200) then Error('Maximum number of lines for CPC are 200');

    if ((lines mod 8) <> 0) then Error('Number of lines must be a multiple of 8');

    if (not FileExists(inputFilename)) then Error('File not found');

    // Let's do it

    if target = 'zx' then ZX(inputFilename, outputFilename, lines) 
    else if target = 'cpc' then CPC(inputFilename, outputFilename, lines) 
    else if target = 'msx' then MSX(inputFilename, outputFilename, lines);

end.

