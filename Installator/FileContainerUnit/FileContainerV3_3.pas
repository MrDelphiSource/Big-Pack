unit FileContainerV3_3;

interface

uses
  Winapi.Windows, Controls,
  SysUtils, System.Classes, System.IOUtils,
  DCPsha256, DCPrijndael;

type
  TEntryType = (etFolder = 0, etFile = 1); // data type


  TEntryMetadata = record
    EntryType: TEntryType;
    FullPath : string;   // "Root\Sub\file.txt" or "Root\Sub"
    Offset   : Int64;    // only files
    FileSize : Int64;    // only files (encrypted size if cipher is used)
    Deleted  : Boolean;  // flag variable to indicate deleted data
  end;

  TFileContainer = class
  public
    // ===== NEW: global progress callback (0..100) =====
    class var OnProgressGlobal: TProc<Integer>;
  private
    FContainerStream: TFileStream;
    FContainerRedact: Boolean;
    FFileList       : array of TEntryMetadata;
    FFileName       : string;
    FPassword       : string;
    FDataEnd        : Int64; // position of the first byte after the "raw" file data


    //Thread's params and progress bar
    FBusy           : Boolean;
    FTotalBytes: Int64;
    FProcessedBytes: Int64;
    FProgressCallback: TProc<Integer>; // these are the bytes that were added

    // Helpers
    procedure AddEntryRecord(const AEntry: TEntryMetadata);
    function  NormalizeContainerPath(const Path: string): string;
    function  EntryExistsInContainer(const Path: string; EntryType: TEntryType): Boolean;
    procedure AddFolderRecursive(const SourcePath, ContainerPath: string);
    procedure EnsureOpen;
    procedure UpdateDataEndFromList;

    // Progress bar
    procedure ReportProgress(const Processed, Total: Int64);
    function  CalcTotalBytes_Compact: Int64;
    function  CalcTotalBytes_AddFolder(const SourcePath: string): Int64;
    function  CalcTotalBytes_Extract_All: Int64;
    function  CalcTotalBytes_Extract_Root(const RootFolderName: string): Int64;

    // Crypto
    procedure EncryptFile_AES256_CBC(const InFile, OutFile, Password: string);
    procedure DecryptFile_AES256_CBC(const InFile, OutFile, Password: string);
    procedure SetPassword(const APwd: string); // Set password for crypt

  public
    constructor Create(const ContainerFileName: string; Load: Boolean; FConReadOnly: Boolean; Pass: string);
    destructor  Destroy; override;



    // MetaData
    procedure SaveMetadata;
    procedure LoadMetadata;

    // Addition
    procedure AddEmptyRootFolder(const RootName: string; AskReplace: Boolean = True);
    procedure AddFolder(const FolderPath: string);
    procedure AddFileToFolder(const FilePath: string; const ContainerFolderPath: string);
    procedure AddSecondLevelFolderWithContent(const RootFolderName, SecondLevelFolderName, SpecialData, SourcePath: string);

    // Deletion (lazy)
    procedure DeleteFile(const ContainerFullPath: string);
    procedure DeleteFolder(const RootFolderName: string);

    // Optimization (physical removal of marked elements)
    procedure Compact;

    // Extraction
    procedure ExtractFolder(const RootFolderName: string; const DestinationPath: string; SkipRoot: Boolean = False);
    procedure ExtractAll(const DestinationPath: string);
    function  ExtractFileToStream(const ContainerFullPath: string; Stream: TStream): Boolean;

    // Information
    function RootFolderExists(const RootName: string): Boolean;
    function GetRootFolders: TStringList;
    function GetSecondLevelFolders(const RootName: string): TStringList;
    function GetEntriesUnderRoot(const RootFolderName: string): TStringList;
    function GetAllEntries: TStringList;
    function FileExistsInContainer(const Path: string): Boolean;
    function FolderExistsInContainer(const Path: string): Boolean;

    // ===== Ќќ¬≈: властив≥сть Busy =====
    property Busy: Boolean read FBusy;
  end;

implementation

uses
  Unit1;

const
  // stable signature at the end of the file
  SIGNATURE: AnsiString = '{*Signature*File*Control*}';
  // A marker that we put at the beginning of the metadata before Count and encrypt with it
  META_MAGIC: AnsiString = '{*Meta*Header_6B65792D70726F74656374*MyAS*}';
  // Block size for byte-by-byte copying
  COPY_BUF_SIZE = 64 * 1024;

{ ===== LOCAL COMPUTING FLOW FOR PROGRESS BAR ===== }
// Minimal wrapper over any source: every time it is read from, we
// add to the global ProcessedBytes counter and fetch ReportProgress.
type
  TCountingStream = class(TStream)
  private
    FBase: TStream;
    FOnCount: TProc<Integer>; // how many bytes were just read
  public
    constructor Create(ABase: TStream; AOnCount: TProc<Integer>);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Size: Int64; reintroduce;
  end;

constructor TCountingStream.Create(ABase: TStream; AOnCount: TProc<Integer>);
begin
  inherited Create;
  FBase := ABase;
  FOnCount := AOnCount;
end;

function TCountingStream.Read(var Buffer; Count: Integer): Longint;
var R: Longint;
begin
  R := FBase.Read(Buffer, Count);
  if (R > 0) and Assigned(FOnCount) then
    FOnCount(R);
  Result := R;
end;

function TCountingStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FBase.Write(Buffer, Count);
  // for our tasks we do not count Write (we count by Read from the source)
end;

function TCountingStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FBase.Seek(Offset, Origin);
end;

function TCountingStream.Size: Int64;
begin
  Result := FBase.Size;
end;

{ ======== UTILS ======== }

procedure TFileContainer.EnsureOpen;
begin
  if not Assigned(FContainerStream) then
    raise Exception.Create('Container stream not assigned.');
end;

procedure TFileContainer.AddEntryRecord(const AEntry: TEntryMetadata);
var
  idx: Integer;
begin
  idx := Length(FFileList);
  SetLength(FFileList, idx + 1);
  FFileList[idx] := AEntry;
end;

function TFileContainer.NormalizeContainerPath(const Path: string): string;
var
  s: string;
begin
  s := Path.Replace('/', '\');
  while (s <> '') and (s[1] = '\') do Delete(s, 1, 1);
  while (s <> '') and (s[Length(s)] = '\') do Delete(s, Length(s), 1);
  Result := s;
end;

function TFileContainer.EntryExistsInContainer(const Path: string; EntryType: TEntryType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and
       SameText(FFileList[i].FullPath, Path) and
       (FFileList[i].EntryType = EntryType) then
      Exit(True);
end;

procedure TFileContainer.UpdateDataEndFromList;
var
  i: Integer;
  e: Int64;
begin
  FDataEnd := 0;
  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFile) then
    begin
      e := FFileList[i].Offset + FFileList[i].FileSize;
      if e > FDataEnd then FDataEnd := e;
    end;
end;

{ ======== PROGRESS ======== }

procedure TFileContainer.ReportProgress(const Processed, Total: Int64);
var
  Pct: Integer;
begin
  if (Total <= 0) then
  begin
    if Assigned(OnProgressGlobal) then
      OnProgressGlobal(0);
    Exit;
  end;

  Pct := Round(Processed * 100.0 / Total);
  if Pct < 0 then Pct := 0 else if Pct > 100 then Pct := 100;

  if Assigned(OnProgressGlobal) then
    OnProgressGlobal(Pct);
end;

function TFileContainer.CalcTotalBytes_Compact: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFile) then
      Inc(Result, FFileList[i].FileSize);
end;

function TFileContainer.CalcTotalBytes_AddFolder(const SourcePath: string): Int64;
var
  SR: TSearchRec;
  Path: string;
begin
  Result := 0;
  if FindFirst(TPath.Combine(SourcePath, '*'), faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      Path := TPath.Combine(SourcePath, SR.Name);
      if (SR.Attr and faDirectory) = faDirectory then
        Inc(Result, CalcTotalBytes_AddFolder(Path))
      else
        Inc(Result, SR.Size);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function TFileContainer.CalcTotalBytes_Extract_All: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFile) then
      Inc(Result, FFileList[i].FileSize);
end;

function TFileContainer.CalcTotalBytes_Extract_Root(const RootFolderName: string): Int64;
var
  i: Integer;
  prefix: string;
begin
  Result := 0;
  prefix := NormalizeContainerPath(RootFolderName);
  if prefix <> '' then prefix := prefix + '\';

  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and
       SameText(Copy(FFileList[i].FullPath, 1, Length(prefix)), prefix) and
       (FFileList[i].EntryType = etFile) then
      Inc(Result, FFileList[i].FileSize);
end;

{ ======== CRYPTO ======== }

procedure TFileContainer.EncryptFile_AES256_CBC(const InFile, OutFile, Password: string);
var
  Cipher: TDCP_rijndael;
  Source, Dest: TFileStream;
begin
  Source := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    Dest := TFileStream.Create(OutFile, fmCreate);
    try
      Cipher := TDCP_rijndael.Create(nil);
      try
        Cipher.InitStr(Password, TDCP_sha256);
        Cipher.EncryptStream(Source, Dest, Source.Size);
        Cipher.Burn;
      finally
        Cipher.Free;
      end;
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TFileContainer.DecryptFile_AES256_CBC(const InFile, OutFile, Password: string);
var
  Cipher: TDCP_rijndael;
  Source, Dest: TFileStream;
begin
  Source := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    Dest := TFileStream.Create(OutFile, fmCreate);
    try
      Cipher := TDCP_rijndael.Create(nil);
      try
        Cipher.InitStr(Password, TDCP_sha256);
        Cipher.DecryptStream(Source, Dest, Source.Size);
        Cipher.Burn;
      finally
        Cipher.Free;
      end;
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

{ ======== LIFECYCLE ======== }

constructor TFileContainer.Create(const ContainerFileName: string; Load: Boolean; FConReadOnly: Boolean; Pass: string);
begin
  inherited Create;
  FFileName := ContainerFileName;

  if Pass = '' then
    raise Exception.Create('Password not entered! Unable to open file!');

  if
   FConReadOnly
  then
   FContainerRedact := False
  else
   FContainerRedact := True;

  if Load then
  begin
    FContainerStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);
    try
      SetPassword(Pass);
      LoadMetadata; // If the password is incorrect Ч an exception will be thrown here
      EnsureOpen;
      FContainerStream.Position := FDataEnd;
    except
      FreeAndNil(FContainerStream); // Must be closed!!!!
                                   //  If not closed - the destructor will be triggered - the destroy
                                  // method will call SaveMetadata, which in turn will corrupt
                                  // the metadata in the opened file!
      raise; // Forward the exception
    end;
  end
  else
  begin
    FContainerStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    SetPassword(Pass);
    SetLength(FFileList, 0);
    FDataEnd := 0;
    SaveMetadata;               // Create a valid empty container: [0-meta][MetaSize=0][SIGNATURE]
    FContainerStream.Position := FDataEnd;
  end;
end;

destructor TFileContainer.Destroy;
begin
 if
  FContainerRedact
 then
  begin
   try
    if Assigned(FContainerStream) then
      SaveMetadata; // finalizing metadata
   except
    // do not raise an exception in the destructor
   end;
  end;

  FreeAndNil(FContainerStream);
  inherited;
end;

procedure TFileContainer.SetPassword(const APwd: string);
begin
  FPassword := APwd;
end;

{ ======== METADATA I/O ======== }


function FindLastValidFooter(const FS: TFileStream; out MetaSize: Int64; out MetaStart: Int64): Boolean;
var
  sigLen: Integer;
  tryPos, footerPos: Int64;
  sigBuf: AnsiString;
begin
  Result := False;
  MetaSize := 0;
  MetaStart := 0;

  sigLen := Length(SIGNATURE);
  if FS.Size < sigLen + SizeOf(Int64) then
    Exit;

  // look for exactly the "last" valid footer: [MetaSize:Int64][SIGNATURE]
 // Start from the theoretical footer location and check the signature.
  footerPos := FS.Size - sigLen;
  FS.Position := footerPos;
  SetLength(sigBuf, sigLen);
  FS.ReadBuffer(PAnsiChar(sigBuf)^, sigLen);

  if sigBuf = SIGNATURE then
  begin
    // read the MetaSize that appears before the signature
    FS.Position := footerPos - SizeOf(Int64);
    FS.ReadBuffer(MetaSize, SizeOf(MetaSize));
    if (MetaSize >= 0) and (footerPos - SizeOf(Int64) - MetaSize >= 0) then
    begin
      MetaStart := footerPos - SizeOf(Int64) - MetaSize;
      Result := True;
      Exit;
    end;
  end;

  // If something goes wrong (or the tail is dirty), we try to find the signature,
  // moving back through the file (a couple of dozen steps is cheap and reliable).
  // In normal life, this branch will not work, because the first check will pass.
  tryPos := FS.Size - sigLen;
  while (tryPos >= 0) and (tryPos >= FS.Size - 1024 * 1024) do
  begin
    FS.Position := tryPos;
    SetLength(sigBuf, sigLen);
    FS.ReadBuffer(PAnsiChar(sigBuf)^, sigLen);
    if sigBuf = SIGNATURE then
    begin
      FS.Position := tryPos - SizeOf(Int64);
      if FS.Position >= 0 then
      begin
        FS.ReadBuffer(MetaSize, SizeOf(MetaSize));
        if (MetaSize >= 0) and (tryPos - SizeOf(Int64) - MetaSize >= 0) then
        begin
          MetaStart := tryPos - SizeOf(Int64) - MetaSize;
          Result := True;
          Exit;
        end;
      end;
    end;
    Dec(tryPos); // one byte back
  end;
end;



procedure TFileContainer.SaveMetadata;
var
  MetaStream, EncStream: TMemoryStream;
  Count, i, NameLen: Integer;
  MetaSize: Int64;
  DataEnd, tmpEnd: Int64;
  etByte: Byte;
  Cipher: TDCP_rijndael;
begin
  EnsureOpen;

  if FPassword = '' then
    raise Exception.Create('Password not set!');

  MetaStream := TMemoryStream.Create;
  EncStream  := TMemoryStream.Create;
  try
    // ---- 1) We form a metablock in memory: [MagicLen:Int32][MagicBytes][Count][... records ...]
    // Magic
    NameLen := Length(META_MAGIC);
    MetaStream.WriteBuffer(NameLen, SizeOf(NameLen)); // here NameLen = magic length
    if NameLen > 0 then
      MetaStream.WriteBuffer(PAnsiChar(META_MAGIC)^, NameLen);

    // Count
    Count := Length(FFileList);
    MetaStream.WriteBuffer(Count, SizeOf(Count));

    // Entries
    for i := 0 to Count - 1 do
    begin
      etByte := Byte(FFileList[i].EntryType);
      MetaStream.WriteBuffer(etByte, SizeOf(etByte));

      NameLen := Length(FFileList[i].FullPath);
      MetaStream.WriteBuffer(NameLen, SizeOf(NameLen));
      if NameLen > 0 then
        MetaStream.WriteBuffer(PChar(FFileList[i].FullPath)^, NameLen * SizeOf(Char));

      MetaStream.WriteBuffer(FFileList[i].Offset,   SizeOf(Int64));
      MetaStream.WriteBuffer(FFileList[i].FileSize, SizeOf(Int64));
      MetaStream.WriteBuffer(FFileList[i].Deleted,  SizeOf(Boolean));
    end;

    // ---- 2) ЎифруЇмо метаблок в EncStream
    MetaStream.Position := 0;
    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.InitStr(FPassword, TDCP_sha256);
      Cipher.EncryptStream(MetaStream, EncStream, MetaStream.Size);
      Cipher.Burn;
    finally
      Cipher.Free;
    end;

    MetaSize := EncStream.Size;

    // ---- 3) End of raw data (we do NOT trim! we just end)
    DataEnd := 0;
    for i := 0 to Count - 1 do
      if (FFileList[i].EntryType = etFile) then
      begin
        tmpEnd := FFileList[i].Offset + FFileList[i].FileSize;
        if tmpEnd > DataEnd then
          DataEnd := tmpEnd;
      end;

    // 4) Position yourself at DataEnd, but do not truncate the file below this value.
    if FContainerStream.Size < DataEnd then
      raise Exception.Create('Container stream truncated unexpectedly.');

    FContainerStream.Position := DataEnd;
    FDataEnd := DataEnd;

    // ---- 5) We write encrypted metadata, then MetaSize and plaintext SIGNATURE (as before)
    EncStream.Position := 0;
    FContainerStream.CopyFrom(EncStream, MetaSize);

    FContainerStream.WriteBuffer(MetaSize, SizeOf(MetaSize));
    if Length(SIGNATURE) > 0 then
      FContainerStream.WriteBuffer(PAnsiChar(SIGNATURE)^, Length(SIGNATURE));
  finally
    MetaStream.Free;
    EncStream.Free;
  end;
end;

procedure TFileContainer.LoadMetadata;
var
  MetaSize: Int64;
  MetaStartPos: Int64;
  EncStream, DecStream: TMemoryStream;
  Count, i, NameLen: Integer;
  etByte: Byte;
  DeletedFlag: Boolean;
  sName: string;
  MagicLen: Integer;
  MagicBuf: AnsiString;
  Cipher: TDCP_rijndael;
begin
  SetLength(FFileList, 0);
  FDataEnd := 0;

  EnsureOpen;

  if FPassword = '' then
    raise Exception.Create('Password not set!');

  // ---- 1) We are looking for the last valid footer (MetaSize + SIGNATURE) Ч as before
  if not FindLastValidFooter(FContainerStream, MetaSize, MetaStartPos) then
    raise Exception.Create('Signatere not found!');

  // The beginning of the metadata and the end of the raw data coincide
  FDataEnd := MetaStartPos;

  EncStream := TMemoryStream.Create;
  DecStream := TMemoryStream.Create;
  try
    // ---- 2) Reading the encrypted metadata block
    FContainerStream.Position := MetaStartPos;
    EncStream.CopyFrom(FContainerStream, MetaSize);
    EncStream.Position := 0;

    // ---- 3) Decrypt in DecStream
    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.InitStr(FPassword, TDCP_sha256);
      Cipher.DecryptStream(EncStream, DecStream, EncStream.Size);
      Cipher.Burn;
    finally
      Cipher.Free;
    end;

    DecStream.Position := 0;

    // ---- 4) Check the encrypted token at the beginning
    if DecStream.Size < SizeOf(MagicLen) then
      raise Exception.Create('Invalid password or corrupted metadata.');

    DecStream.ReadBuffer(MagicLen, SizeOf(MagicLen));
    if (MagicLen <= 0) or (MagicLen > 1024) or (DecStream.Size - DecStream.Position < MagicLen) then
      raise Exception.Create('Invalid password or corrupted metadata.');

    SetLength(MagicBuf, MagicLen);
    if MagicLen > 0 then
      DecStream.ReadBuffer(PAnsiChar(MagicBuf)^, MagicLen);

    if MagicBuf <> META_MAGIC then
      raise Exception.Create('Invalid password or corrupted metadata.');

    // ---- 5) Then everything is as it was: we read Count and records
    DecStream.ReadBuffer(Count, SizeOf(Count));
    if Count < 0 then Exit;
    SetLength(FFileList, Count);

    for i := 0 to Count - 1 do
    begin
      DecStream.ReadBuffer(etByte, SizeOf(etByte));

      DecStream.ReadBuffer(NameLen, SizeOf(NameLen));
      if NameLen < 0 then NameLen := 0;
      SetLength(sName, NameLen);
      if NameLen > 0 then
        DecStream.ReadBuffer(PChar(sName)^, NameLen * SizeOf(Char))
      else
        sName := '';

      FFileList[i].FullPath := sName;

      DecStream.ReadBuffer(FFileList[i].Offset, SizeOf(Int64));
      DecStream.ReadBuffer(FFileList[i].FileSize, SizeOf(Int64));
      DecStream.ReadBuffer(DeletedFlag, SizeOf(Boolean));
      FFileList[i].Deleted := DeletedFlag;

      if etByte = Byte(etFolder) then
        FFileList[i].EntryType := etFolder
      else
        FFileList[i].EntryType := etFile;
    end;
  finally
    EncStream.Free;
    DecStream.Free;
  end;
end;




{ ======== ADD / DELETE ======== }

procedure TFileContainer.AddFolderRecursive(const SourcePath: string; const ContainerPath: string);
var
  SR: TSearchRec;
  FolderEntry: TEntryMetadata;
  ItemSource, ItemContainer: string;
begin
  // запис папки
  if not EntryExistsInContainer(ContainerPath, etFolder) then
  begin
    FolderEntry.EntryType := etFolder;
    FolderEntry.FullPath  := ContainerPath;
    FolderEntry.Offset    := 0;
    FolderEntry.FileSize  := 0;
    FolderEntry.Deleted   := False;
    AddEntryRecord(FolderEntry);
  end;

  if FindFirst(TPath.Combine(SourcePath, '*'), faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;

      ItemSource := TPath.Combine(SourcePath, SR.Name);
      if ContainerPath = '' then
        ItemContainer := SR.Name
      else
        ItemContainer := ContainerPath + '\' + SR.Name;

      if (SR.Attr and faDirectory) = faDirectory then
        AddFolderRecursive(ItemSource, ItemContainer)
      else
        AddFileToFolder(ItemSource, ItemContainer); // full container path including file name
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TFileContainer.AddEmptyRootFolder(const RootName: string; AskReplace: Boolean);
var
  i, Select: Integer;
  Found: Boolean;
  RootNorm: string;
  Entry: TEntryMetadata;
begin
  EnsureOpen;

  RootNorm := NormalizeContainerPath(RootName);
  if RootNorm = '' then
    raise Exception.Create('Invalid root name.');

  Found := False;
  for i := 0 to High(FFileList) do
    if not FFileList[i].Deleted and
       ( SameText(FFileList[i].FullPath, RootNorm) or
         SameText(Copy(FFileList[i].FullPath, 1, Length(RootNorm) + 1), RootNorm + '\') ) then
    begin
      Found := True;
      Break;
    end;

  if Found then
  begin
    if AskReplace then
    begin
      Select := MessageBox(0, PChar('Folder "' + RootName + '" already exists. Replace?'),
                           PChar(Form1.Caption + ' - confirmation'),
                           MB_ICONQUESTION or MB_YESNO);
      if Select = mrYes then
      begin
        for i := 0 to High(FFileList) do
          if not FFileList[i].Deleted and
             ( SameText(FFileList[i].FullPath, RootNorm) or
               SameText(Copy(FFileList[i].FullPath, 1, Length(RootNorm) + 1), RootNorm + '\') ) then
            FFileList[i].Deleted := True;
        Compact;
      end
      else
        Exit;
    end
    else
      Exit;
  end;

  Entry.EntryType := etFolder;
  Entry.FullPath  := RootNorm;
  Entry.Offset    := 0;
  Entry.FileSize  := 0;
  Entry.Deleted   := False;
  AddEntryRecord(Entry);
end;

procedure TFileContainer.AddFolder(const FolderPath: string);
var
  RootName, SourcePath: string;
  i, Select: Integer;
  Found: Boolean;
begin
  if not DirectoryExists(FolderPath) then
    raise Exception.CreateFmt('Folder not found: %s', [FolderPath]);

  if FBusy then
    raise Exception.Create('Container is busy. Please wait.');

  FBusy := True;
  try
    SourcePath := ExpandFileName(FolderPath);
    RootName   := ExtractFileName(ExcludeTrailingPathDelimiter(SourcePath));

    Found := False;
    for i := 0 to High(FFileList) do
      if not FFileList[i].Deleted and
         ( SameText(FFileList[i].FullPath, RootName) or
           SameText(Copy(FFileList[i].FullPath, 1, Length(RootName) + 1), RootName + '\') ) then
      begin
        Found := True; Break;
      end;

    if Found then
    begin
      Select := MessageBox(0, PChar('Folder "' + RootName + '" already exists. Replace?'),
                           PChar(Form1.Caption + ' - confirmation'),
                           MB_ICONWARNING or MB_YESNO);
      if Select = mrYes then
      begin
        for i := 0 to High(FFileList) do
          if not FFileList[i].Deleted and
             ( SameText(FFileList[i].FullPath, RootName) or
               SameText(Copy(FFileList[i].FullPath, 1, Length(RootName) + 1), RootName + '\') ) then
            FFileList[i].Deleted := True;
        Compact;
      end
      else
        Exit;
    end;

    // === Preparing for smooth progress ===
    FTotalBytes := CalcTotalBytes_AddFolder(SourcePath);
    FProcessedBytes := 0;
    ReportProgress(0, FTotalBytes);

    AddFolderRecursive(SourcePath, RootName);

    ReportProgress(FTotalBytes, FTotalBytes);
  finally
    FBusy := False;
  end;
end;

procedure TFileContainer.AddFileToFolder(const FilePath: string; const ContainerFolderPath: string);
var
  FS: TFileStream;
  Counting: TCountingStream;
  Entry: TEntryMetadata;
  containerPathNorm: string;
  StartPos: Int64;
  Cipher: TDCP_rijndael;
begin
  EnsureOpen;

  if not FileExists(FilePath) then
    raise Exception.CreateFmt('File not found: %s', [FilePath]);

  if FPassword = '' then
    raise Exception.Create('Password not set. Call SetPassword before adding!');

  containerPathNorm := NormalizeContainerPath(ContainerFolderPath);

  if FContainerStream.Position <> FDataEnd then
    FContainerStream.Position := FDataEnd;

  StartPos := FContainerStream.Position;

  FS := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
  try
    Counting := TCountingStream.Create(FS,
     procedure(n: Integer)
      begin
       if Assigned(FProgressCallback) then
        FProgressCallback(n);
      end);

    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.InitStr(FPassword, TDCP_sha256);
      Cipher.EncryptStream(Counting, FContainerStream, Counting.Size);
      Cipher.Burn;
    finally
      Cipher.Free;
      Counting.Free;
    end;

    Entry.EntryType := etFile;
    Entry.FullPath  := containerPathNorm;
    Entry.Offset    := StartPos;
    Entry.FileSize  := FContainerStream.Position - StartPos;
    Entry.Deleted   := False;
    AddEntryRecord(Entry);

    FDataEnd := FContainerStream.Position;
  finally
    FS.Free;
  end;
end;

procedure TFileContainer.AddSecondLevelFolderWithContent(
  const RootFolderName, SecondLevelFolderName, SpecialData, SourcePath: string);
var
  ContainerSecondLevelPath, FullName: string;
  i, Select: Integer;
  Found: Boolean;
  Total, Processed: Int64;
begin
  if not DirectoryExists(SourcePath) then
    raise Exception.CreateFmt('Selected folder was not found: %s', [SourcePath]);

  if FPassword = '' then
    raise Exception.Create('Password not set. Call SetPassword before adding!');

  if FBusy then
    raise Exception.Create('Container is busy. Please wait.');

  FBusy := True;
  try
    FullName := NormalizeContainerPath(SecondLevelFolderName) + '$' + SpecialData;
    ContainerSecondLevelPath := NormalizeContainerPath(RootFolderName) + '\' + FullName;

    Found := False;
    for i := 0 to High(FFileList) do
      if not FFileList[i].Deleted and
         ( SameText(FFileList[i].FullPath, ContainerSecondLevelPath) or
           SameText(Copy(FFileList[i].FullPath, 1, Length(ContainerSecondLevelPath) + 1),
                    ContainerSecondLevelPath + '\') ) then
      begin
        Found := True; Break;
      end;

    if Found then
    begin
      Select := MessageBox(0, PChar('Folder "' + SecondLevelFolderName +
                             '" already exists in "' + RootFolderName + '". Replace?'),
                           PChar('ѕ≥дтвердженн€'),
                           MB_ICONQUESTION or MB_YESNO);
      if Select = mrYes then
      begin
        for i := 0 to High(FFileList) do
          if not FFileList[i].Deleted and
             ( SameText(FFileList[i].FullPath, ContainerSecondLevelPath) or
               SameText(Copy(FFileList[i].FullPath, 1, Length(ContainerSecondLevelPath) + 1),
                        ContainerSecondLevelPath + '\') ) then
            FFileList[i].Deleted := True;
        Compact;
      end
      else
        Exit;
    end;

    // === Smooth progress through all files in a folder
    Total := CalcTotalBytes_AddFolder(SourcePath);
    Processed := 0;
    ReportProgress(0, Total);

    // temporary global callback that will be called from AddFileToFolder
    FProgressCallback :=
      procedure(BytesRead: Integer)
      begin
        Inc(Processed, BytesRead);
        ReportProgress(Processed, Total);
      end;

    try
      AddFolderRecursive(SourcePath, ContainerSecondLevelPath);
    finally
      FProgressCallback := nil;
      ReportProgress(Total, Total);
    end;

  finally
    FBusy := False;
  end;
end;


procedure TFileContainer.DeleteFile(const ContainerFullPath: string);
var
  i: Integer;
  pathNorm: string;
begin
  pathNorm := NormalizeContainerPath(ContainerFullPath);
  for i := 0 to High(FFileList) do
    if SameText(FFileList[i].FullPath, pathNorm) and (FFileList[i].EntryType = etFile) then
    begin
      FFileList[i].Deleted := True;
      SaveMetadata; // record changes
      Exit;
    end;
  raise Exception.CreateFmt('File not found in container: %s', [ContainerFullPath]);
end;

procedure TFileContainer.DeleteFolder(const RootFolderName: string);
var
  i: Integer;
  prefix: string;
begin
  prefix := NormalizeContainerPath(RootFolderName);
  if prefix <> '' then prefix := prefix + '\';

  for i := 0 to High(FFileList) do
    if (SameText(FFileList[i].FullPath, NormalizeContainerPath(RootFolderName))) or
       (SameText(Copy(FFileList[i].FullPath, 1, Length(prefix)), prefix)) then
      FFileList[i].Deleted := True;

  SaveMetadata;
end;

{ ======== EXTRACT ======== }

procedure TFileContainer.ExtractFolder(const RootFolderName: string; const DestinationPath: string; SkipRoot: Boolean);
var
  i, FoundCount: Integer;
  prefix, relPath, outPath, outDir, baseName: string;
  OutFS: TFileStream;
  Cipher: TDCP_rijndael;
  Total, Processed: Int64;
  Counting: TCountingStream;
begin
  EnsureOpen;

  if FPassword = '' then
    raise Exception.Create('Password not set. Call SetPassword before adding!');

  if FBusy then
    raise Exception.Create('Container is busy. Please wait.');

  FBusy := True;
  try
    prefix := NormalizeContainerPath(RootFolderName);
    baseName := ExtractFileName(prefix);
    if Pos('$', baseName) > 0 then
      baseName := Copy(baseName, 1, Pos('$', baseName) - 1);

    if prefix <> '' then
      prefix := prefix + '\';

    FoundCount := 0;
    for i := 0 to High(FFileList) do
      if (not FFileList[i].Deleted) and
         SameText(Copy(FFileList[i].FullPath, 1, Length(prefix)), prefix) then
      begin
        Inc(FoundCount);
        Break;
      end;

    if FoundCount = 0 then
      raise Exception.Create('No embedded programs!');

    // ===== Ќќ¬≈: прогрес
    Total := CalcTotalBytes_Extract_Root(RootFolderName);
    Processed := 0;
    ReportProgress(0, Total);

    for i := 0 to High(FFileList) do
    begin
      if FFileList[i].Deleted then Continue;

      if SameText(Copy(FFileList[i].FullPath, 1, Length(prefix)), prefix) then
      begin
        relPath := Copy(FFileList[i].FullPath, Length(prefix) + 1, MaxInt);

        if SkipRoot then
          outPath := TPath.Combine(DestinationPath, TPath.Combine(baseName, relPath))
        else
          outPath := TPath.Combine(DestinationPath, TPath.Combine(NormalizeContainerPath(RootFolderName), relPath));

        outDir := TPath.GetDirectoryName(outPath);
        if outDir <> '' then
          ForceDirectories(outDir);

        if FFileList[i].EntryType = etFile then
        begin
          FContainerStream.Position := FFileList[i].Offset;
          OutFS := TFileStream.Create(outPath, fmCreate);
          try
            Cipher := TDCP_rijndael.Create(nil);
            try
              Cipher.InitStr(FPassword, TDCP_sha256);

              // NEW: CountingStream on encrypted source (container)
              Counting := TCountingStream.Create(FContainerStream,
                procedure(n: Integer)
                begin
                  Inc(Processed, n);
                  ReportProgress(Processed, Total);
                end);

              Cipher.DecryptStream(Counting, OutFS, FFileList[i].FileSize);
              Cipher.Burn;
            finally
              Cipher.Free;
              Counting.Free; // does not close FContainerStream
            end;
          finally
            OutFS.Free;
          end;
        end
        else
          ForceDirectories(outPath);
      end;
    end;

    ReportProgress(Total, Total);
  finally
    FBusy := False;
  end;
end;

procedure TFileContainer.ExtractAll(const DestinationPath: string);
var
  i: Integer;
  outPath, outDir: string;
  OutFS: TFileStream;
  Cipher: TDCP_rijndael;
  Total, Processed: Int64;
  Counting: TCountingStream;
begin
  EnsureOpen;

  if FPassword = '' then
    raise Exception.Create('Password not set. Call SetPassword before adding!');

  if FBusy then
    raise Exception.Create('Container is busy. Please wait.');

  FBusy := True;
  try
    // NEW: progress total for all files
    Total := CalcTotalBytes_Extract_All;
    Processed := 0;
    ReportProgress(0, Total);

    for i := 0 to High(FFileList) do
    begin
      if FFileList[i].Deleted then Continue;

      if FFileList[i].EntryType = etFolder then
      begin
        ForceDirectories(TPath.Combine(DestinationPath, FFileList[i].FullPath));
        Continue;
      end;

      outPath := TPath.Combine(DestinationPath, FFileList[i].FullPath);
      outDir  := TPath.GetDirectoryName(outPath);
      if outDir <> '' then
        ForceDirectories(outDir);

      FContainerStream.Position := FFileList[i].Offset;

      OutFS := TFileStream.Create(outPath, fmCreate);
      try
        Cipher := TDCP_rijndael.Create(nil);
        try
          Cipher.InitStr(FPassword, TDCP_sha256);

          Counting := TCountingStream.Create(FContainerStream,
            procedure(n: Integer)
            begin
              Inc(Processed, n);
              ReportProgress(Processed, Total);
            end);

          Cipher.DecryptStream(Counting, OutFS, FFileList[i].FileSize);
          Cipher.Burn;
        finally
          Cipher.Free;
          Counting.Free;
        end;
      finally
        OutFS.Free;
      end;
    end;

    ReportProgress(Total, Total);
  finally
    FBusy := False;
  end;
end;

function TFileContainer.ExtractFileToStream(const ContainerFullPath: string; Stream: TStream): Boolean;
var
  i: Integer;
  Meta: TEntryMetadata;
  Cipher: TDCP_rijndael;
  Counting: TCountingStream;
begin
  Result := False;
  for i := 0 to High(FFileList) do
  begin
    Meta := FFileList[i];
    if (not Meta.Deleted) and (Meta.EntryType = etFile) and SameText(Meta.FullPath, ContainerFullPath) then
    begin
      FContainerStream.Position := Meta.Offset;

      if FPassword <> '' then
      begin
        Cipher := TDCP_rijndael.Create(nil);
        try
          Cipher.InitStr(FPassword, TDCP_sha256);

          Counting := TCountingStream.Create(FContainerStream,
            procedure(n: Integer)
            begin
              // local progress can be ignored or output
             // here we don't know Total, so we don't report it - if desired, you can add
             // another callback with absolute bytes
            end);

          Cipher.DecryptStream(Counting, Stream, Meta.FileSize);
          Cipher.Burn;
        finally
          Cipher.Free;
          Counting.Free;
        end;
      end
      else
      begin
        Stream.CopyFrom(FContainerStream, Meta.FileSize);
      end;

      Stream.Position := 0;
      Exit(True);
    end;
  end;
end;

{ ======== INFO ======== }

function TFileContainer.RootFolderExists(const RootName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FFileList) do
    if not FFileList[i].Deleted and
       ( SameText(FFileList[i].FullPath, RootName) or
         SameText(Copy(FFileList[i].FullPath, 1, Length(RootName) + 1), RootName + '\') ) then
      Exit(True);
end;

function TFileContainer.GetRootFolders: TStringList;
var
  i: Integer;
  root, firstSeg: string;
  S: TStringList;
begin
  Result := TStringList.Create;
  S := TStringList.Create;
  try
    S.Sorted := True;
    S.Duplicates := dupIgnore;
    for i := 0 to High(FFileList) do
      if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFolder) then
      begin
        root := NormalizeContainerPath(FFileList[i].FullPath);
        if root = '' then Continue;

        if Pos('\', root) > 0 then
          firstSeg := Copy(root, 1, Pos('\', root) - 1)
        else
          firstSeg := root;

        S.Add(firstSeg);
      end;
    Result.Assign(S);
  finally
    S.Free;
  end;
end;

function TFileContainer.GetSecondLevelFolders(const RootName: string): TStringList;
var
  i: Integer;
  pathRel, secondSeg: string;
  S: TStringList;
begin
  Result := TStringList.Create;
  S := TStringList.Create;
  try
    S.Sorted := True;
    S.Duplicates := dupIgnore;

    for i := 0 to High(FFileList) do
      if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFolder) then
        if Pos(RootName + '\', FFileList[i].FullPath) = 1 then
        begin
          pathRel := Copy(FFileList[i].FullPath, Length(RootName) + 2, MaxInt);
          if (pathRel <> '') and (Pos('\', pathRel) = 0) then
          begin
            secondSeg := NormalizeContainerPath(pathRel);
            S.Add(secondSeg);
          end;
        end;

    Result.Assign(S);
  finally
    S.Free;
  end;
end;

function TFileContainer.GetEntriesUnderRoot(const RootFolderName: string): TStringList;
var
  i: Integer;
  prefix: string;
begin
  Result := TStringList.Create;
  prefix := NormalizeContainerPath(RootFolderName);
  if prefix <> '' then prefix := prefix + '\';

  for i := 0 to High(FFileList) do
    if (not FFileList[i].Deleted) and
       ( SameText(FFileList[i].FullPath, NormalizeContainerPath(RootFolderName)) or
         SameText(Copy(FFileList[i].FullPath, 1, Length(prefix)), prefix) ) then
      Result.Add(FFileList[i].FullPath);
end;

function TFileContainer.GetAllEntries: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to High(FFileList) do
    if not FFileList[i].Deleted then
      Result.Add(FFileList[i].FullPath);
end;

function TFileContainer.FileExistsInContainer(const Path: string): Boolean;
begin
  Result := EntryExistsInContainer(Path, etFile);
end;

function TFileContainer.FolderExistsInContainer(const Path: string): Boolean;
begin
  Result := EntryExistsInContainer(Path, etFolder);
end;

{ ======== COMPACT ======== }

procedure TFileContainer.Compact;
var
  TempFileName: string;
  TempStream: TFileStream;
  i, NewIndex: Integer;
  NewOffset: Int64;
  NewFileList: array of TEntryMetadata;
  Total, Processed: Int64;   // NEW: progress by bytes
  Buf: array[0..COPY_BUF_SIZE-1] of Byte;
  Remaining, ToRead: Int64;
  ReadNow: Integer;
begin
  EnsureOpen;

  if FBusy then
    raise Exception.Create('Container is busy. Please wait.');

  FBusy := True;
  try
    // NEW: Preparation progress
    Total := CalcTotalBytes_Compact;
    Processed := 0;
    ReportProgress(0, Total);

    TempFileName := FFileName + '.tmp';
    TempStream := TFileStream.Create(TempFileName, fmCreate);
    try
      SetLength(NewFileList, 0);

      // copy only non-deleted files; folders Ц only in metadata
      for i := 0 to High(FFileList) do
      begin
        if FFileList[i].Deleted then Continue;

        if FFileList[i].EntryType = etFile then
        begin
          NewOffset := TempStream.Position;
          FContainerStream.Position := FFileList[i].Offset;

          Remaining := FFileList[i].FileSize;
          while Remaining > 0 do
          begin
            ToRead := Remaining;
            if ToRead > COPY_BUF_SIZE then ToRead := COPY_BUF_SIZE;
            ReadNow := FContainerStream.Read(Buf, ToRead);
            if ReadNow <= 0 then Break;
            TempStream.WriteBuffer(Buf, ReadNow);

            Dec(Remaining, ReadNow);
            Inc(Processed, ReadNow);
            ReportProgress(Processed, Total);
          end;
        end
        else
          NewOffset := 0;

        NewIndex := Length(NewFileList);
        SetLength(NewFileList, NewIndex + 1);
        NewFileList[NewIndex] := FFileList[i];
        if NewFileList[NewIndex].EntryType = etFile then
          NewFileList[NewIndex].Offset := NewOffset
        else
          NewFileList[NewIndex].Offset := 0;
      end;
    finally
      TempStream.Free;
    end;

    FContainerRedact := True;
    // close the current stream, change the file
    FreeAndNil(FContainerStream);

    try
      if not SysUtils.DeleteFile(FFileName) then
        raise Exception.CreateFmt('Unable to delete original file: %s', [FFileName]);

      if not RenameFile(TempFileName, FFileName) then
        raise Exception.Create('Could not rename temporary file to original!');
    except
      if FileExists(TempFileName) then
        SysUtils.DeleteFile(TempFileName);
      raise;
    end;

    // apply the new list and open the stream again
    SetLength(FFileList, Length(NewFileList));
    for i := 0 to High(NewFileList) do
      FFileList[i] := NewFileList[i];

    FContainerRedact := False;
    FContainerStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyNone);

    // FDataEnd = end of raw data (now it's the size of the temporary file)
    FDataEnd := 0;
    for i := 0 to High(FFileList) do
      if (not FFileList[i].Deleted) and (FFileList[i].EntryType = etFile) then
        if FFileList[i].Offset + FFileList[i].FileSize > FDataEnd then
          FDataEnd := FFileList[i].Offset + FFileList[i].FileSize;

    SaveMetadata; // add metadata/signature
    FContainerStream.Position := FDataEnd;

    ReportProgress(Total, Total);
  finally
    FBusy := False;
  end;
end;

end.

