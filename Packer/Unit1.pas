unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, FileContainerV3_3,
  System.ImageList, Vcl.ImgList, PngImageList, Vcl.Menus, Vcl.Imaging.pngimage, Vcl.FileCtrl, ShellAPI;

const
 WM_GOTOFOREGROUND = WM_USER + 1;  //Management after notification of duplicate copyt
  WM_MY_BASE            = WM_APP + 200;
  InputBoxMessagePass   = WM_MY_BASE + 0;
  InputBoxMessageString = WM_MY_BASE + 1;

type
  TForm1 = class(TForm)
    MainPanel: TPanel;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    ListView1: TListView;
    ListView2: TListView;
    Splitter1: TSplitter;
    SaveFileCont: TSaveDialog;
    OpenFileCont: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PngImageList1: TPngImageList;
    PngImageList2: TPngImageList;
    CreateCont: TMenuItem;
    OpenCont: TMenuItem;
    UpCont: TMenuItem;
    CloseCont: TMenuItem;
    N1: TMenuItem;
    AddCat: TMenuItem;
    ExtCat: TMenuItem;
    DelCat: TMenuItem;
    AddProg: TMenuItem;
    ExtProg: TMenuItem;
    StartProg: TMenuItem;
    DelProg: TMenuItem;
    SaveArhiveFile: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure CreateContClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OpenContClick(Sender: TObject);
    procedure UpContClick(Sender: TObject);
    procedure CloseContClick(Sender: TObject);
    procedure AddCatClick(Sender: TObject);
    procedure ExtCatClick(Sender: TObject);
    procedure DelCatClick(Sender: TObject);
    procedure AddProgClick(Sender: TObject);
    procedure ExtProgClick(Sender: TObject);
    procedure DelProgClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListView2Deletion(Sender: TObject; Item: TListItem);
    procedure StartProgClick(Sender: TObject);
    procedure ListView2DblClick(Sender: TObject);

  private
    { Private declarations }

   {Private variables}
   Container: TFileContainer; // General file-container stream
   CurrentRootFolderName: string;

   {Private functions & procedures}
   procedure WMGotoForeground(var Msg: TMessage); message WM_GOTOFOREGROUND;
   procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
   procedure ClearFolderRecursive(const Folder: string);
   procedure ExtractAndRunSelectedSubfolder;
   procedure InputBoxSetPasswordChar(var Msg: TMessage); message InputBoxMessagePass;
   procedure InputBoxTypeString(var Msg: TMessage); message InputBoxMessageString;
   procedure RefreshFolderList;
   procedure ClearSecondLevelList;
   procedure RefreshSecondLevelList(const RootName: string);
   procedure CloseContainer;
   function  ExistsInvalidChars(const S: string): Boolean;
  public
    { Public declarations }
   FilePassword : string;
  end;

const
 ScreenSW : LongInt = 790; /// Min width of form
 ScreenSH : LongInt = 480; /// Min height of form

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WMGotoForeground(var Msg: TMessage);  /// Window To the foreground
var
  hWnd, hCurWnd, dwThreadID, dwCurThreadID: THandle;
  OldTimeOut: Cardinal;
  AResult: Boolean;
begin
  if GetActiveWindow = Application.MainForm.Handle then
    Exit;
  Application.Minimize;
  Application.Restore;
  hWnd := Application.Handle;
  SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @OldTimeOut, 0);
  SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, Pointer(0), 0);
  SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  hCurWnd := GetForegroundWindow;
  AResult := False;
  while not AResult do
  begin
    dwThreadID := GetCurrentThreadId;
    dwCurThreadID := GetWindowThreadProcessId(hCurWnd);
    AttachThreadInput(dwThreadID, dwCurThreadID, True);
    AResult := SetForegroundWindow(hWnd);
    AttachThreadInput(dwThreadID, dwCurThreadID, False);
  end;
  SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, Pointer(OldTimeOut), 0);
end;

procedure TForm1.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); //// Limit size of form
begin
  inherited;
  with Msg.MinMaxInfo^.ptMinTrackSize do
  begin
    x := ScreenSW;
    y := ScreenSH;
  end;
end;

procedure TForm1.ClearFolderRecursive(const Folder: string);
var
  SR: TSearchRec;
  Path: string;
begin
  if not DirectoryExists(Folder) then Exit;

  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;

      Path := IncludeTrailingPathDelimiter(Folder) + SR.Name;

      if (SR.Attr and faDirectory) = faDirectory then
      begin
        ClearFolderRecursive(Path); // clean the subfolder
        RemoveDir(Path); // delete the empty one
      end
      else
        DeleteFile(Path);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TForm1.ExtractAndRunSelectedSubfolder;
var
  TempPath, SubFolderName, SpecialParam, ExtractToPath, FullFolderPath, FullExePath, DirSave: string;
  SpecialParamPtr: PString;
begin
  if not Assigned(Container) then
  begin
    MessageBoxEx(0, 'Container is not loaded!', PChar(Caption),
      MB_ICONERROR + MB_OK, 0);
    Exit;
  end;

  if not Assigned(ListView2.Selected) then
  begin
    MessageBoxEx(0, 'No subfolder selected!', PChar(Caption),
      MB_ICONWARNING + MB_OK, 0);
    Exit;
  end;

  // Get a special parameter from Data
  SpecialParamPtr := PString(ListView2.Selected.Data);
  if (SpecialParamPtr = nil) or (SpecialParamPtr^ = '') then
  begin
    MessageBoxEx(0, 'Special parameter is missing!', PChar(Caption),
      MB_ICONERROR + MB_OK, 0);
    Exit;
  end;
  SpecialParam := SpecialParamPtr^;

  // Temp folder
  TempPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Temp';

  if not DirectoryExists(TempPath) then
    ForceDirectories(TempPath);

  // The name of the subfolder in the container (e.g., the name of the application)
  SubFolderName :=  ListView2.Selected.Caption;

  if
   SpecialParam <> EmptyStr
  then
    FullFolderPath := CurrentRootFolderName + '\' + SubFolderName + '$' + SpecialParam
  else
    FullFolderPath := CurrentRootFolderName + '\' + SubFolderName;


  if
   SpecialParam = ('_ARCHIVE!_')
  then
   begin
    if
      SelectDirectory('Select a folder to save to', '', DirSave,[sdNewUI, sdNewFolder])
    then
      begin

       ProgressBar1.Position := 0;
       ProgressBar1.Visible := True;

       TFileContainer.OnProgressGlobal :=
        procedure(p: Integer)
          begin
          TThread.Synchronize(nil,
           procedure
            begin
             ProgressBar1.Position := p;
             Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
            end);
         end;

       TThread.CreateAnonymousThread(
        procedure
         begin
           try
            Container.ExtractFolder(FullFolderPath, DirSave, True);
           finally
            TThread.Synchronize(nil,
             procedure
              begin
               ProgressBar1.Visible := False;
               ProgressBar1.Position := 0;
               TFileContainer.OnProgressGlobal := nil;
               MessageBoxEx(0,PChar('Archive file saved successfully!'),
                PChar(Form1.Caption + ' - saving archive file'),MB_ICONINFORMATION+MB_OK,
                  MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
               StatusBar1.Panels[1].Text := EmptyStr;
              end);
           end;
         end).Start;
      end
    else
     begin
      Exit;
     end;
    Exit;
   end
  else
     begin

      ExtractToPath :=  TempPath;

      ProgressBar1.Position := 0;
      ProgressBar1.Visible := True;

      TFileContainer.OnProgressGlobal :=
         procedure(p: Integer)
           begin
            TThread.Synchronize(nil,
             procedure
              begin
               ProgressBar1.Position := p;
               Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
              end);
           end;

      TThread.CreateAnonymousThread(
         procedure
          begin
           try
            ClearFolderRecursive(ExtractToPath);
            Container.ExtractFolder(FullFolderPath, ExtractToPath, True);
           finally
             TThread.Synchronize(nil,
              procedure
               begin
                ProgressBar1.Visible := False;
                ProgressBar1.Position := 0;
                TFileContainer.OnProgressGlobal := nil;
                StatusBar1.Panels[1].Text := EmptyStr;
                FullExePath := IncludeTrailingPathDelimiter(TempPath + '\' + SubFolderName) + SpecialParam;

                if not FileExists(FullExePath) then
                  begin
                   MessageBoxEx(0, PChar('File not found: ' + FullExePath),
                      PChar(Caption), MB_ICONERROR + MB_OK, 0);
                   Exit;
                  end;
                 // Execute from ShellExecute
                ShellExecute(Handle, 'open', PChar(FullExePath), nil, PChar(ExtractFilePath(FullExePath)), SW_SHOWNORMAL);
               end);
           end;
          end).Start;
     end;

end;

{ === Service helpers === }

procedure TForm1.CloseContainer;
begin
  ClearSecondLevelList;
  ListView1.Clear;

  if Assigned(Container) then
    FreeAndNil(Container);

  CurrentRootFolderName := EmptyStr;
  StatusBar1.Panels[0].Text := 'No container file selected';
end;

procedure TForm1.ClearSecondLevelList;
var
  I: Integer;
begin
  // free hidden PStrings in Data
  for I := 0 to ListView2.Items.Count - 1 do
    if Assigned(ListView2.Items[I].Data) then
    begin
      Dispose(PString(ListView2.Items[I].Data));
      ListView2.Items[I].Data := nil;
    end;

  ListView2.Items.Clear;
  PngImageList2.Clear;
end;

procedure TForm1.RefreshFolderList;
var
  SL: TStringList;
  Item: TListItem;
  I: Integer;
begin
  ListView1.Items.Clear;
  PngImageList1.DrawingStyle := dsTransparent;
  if not Assigned(Container) then
    Exit;

  SL := Container.GetRootFolders;
  try
    for I := 0 to SL.Count - 1 do
    begin
      Item := ListView1.Items.Add;
      Item.Caption := SL[I];
      Item.ImageIndex := 0;
    end;
  finally
    SL.Free;
  end;
end;

procedure TForm1.RefreshSecondLevelList(const RootName: string);
var
  SL: TStringList;
  Item: TListItem;
  I, DelimPos: Integer;
  S, NamePart, SpecialPart, EntryName: string;
  IconStream: TMemoryStream;
  IconPNG: TPNGImage;
  IconIndex: Integer;
  HiddenStr: PString;
begin
  ClearSecondLevelList;

  if not Assigned(Container) then
    Exit;

  SL := Container.GetSecondLevelFolders(RootName);
  try
    PngImageList2.DrawingStyle := dsTransparent;

    for I := 0 to SL.Count - 1 do
    begin
      S := SL[I];

      DelimPos := Pos('$', S);
      if DelimPos > 0 then
      begin
        NamePart := Copy(S, 1, DelimPos - 1);
        SpecialPart := Copy(S, DelimPos + 1, MaxInt);
      end
      else
      begin
        NamePart := S;
        SpecialPart := '';
      end;

      Item := ListView2.Items.Add;
      Item.Caption := NamePart;

      if SpecialPart <> '' then
      begin
        New(HiddenStr);
        HiddenStr^ := SpecialPart;
        Item.Data := HiddenStr;
      end
      else
        Item.Data := nil;

      // logo.png in a subfolder
      if SpecialPart <> '' then
        EntryName := RootName + '\' + NamePart + '$' + SpecialPart + '\logo.png'
      else
        EntryName := RootName + '\' + NamePart + '\logo.png';

      if Container.FileExistsInContainer(EntryName) then
      begin
        IconStream := TMemoryStream.Create;
        try
          if Container.ExtractFileToStream(EntryName, IconStream) then
          begin
            IconStream.Position := 0;
            IconPNG := TPNGImage.Create;
            try
              try
                IconPNG.LoadFromStream(IconStream);
                IconIndex := PngImageList2.AddPng(IconPNG);
                Item.ImageIndex := IconIndex;
              except
                // if the PNG is broken, just ignore it
                Item.ImageIndex := -1;
              end;
            finally
              IconPNG.Free;
            end;
          end;
        finally
          IconStream.Free;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;


procedure TForm1.InputBoxSetPasswordChar(var Msg: TMessage);
var
   hInputForm, hEdit, hButton: HWND;
   Str : string;
begin
  Str := 'Х';
   hInputForm := Screen.Forms[0].Handle;
   if (hInputForm <> 0) then
   begin
     hEdit := FindWindowEx(hInputForm, 0, 'TEdit', nil);
      {
       // Change button text:
       hButton := FindWindowEx(hInputForm, 0, 'TButton', nil);
       SendMessage(hButton, WM_SETTEXT, 0, Integer(PChar('Cancel')));
       }
     SendMessage(hEdit, EM_SETPASSWORDCHAR, Ord(Str[1]), 0);
     SendMessage(hEdit, EM_SETLIMITTEXT, 48, 0);
   end;
end;

procedure TForm1.InputBoxTypeString(var Msg: TMessage);
var
   hInputForm, hEdit, hButton: HWND;
begin
   hInputForm := Screen.Forms[0].Handle;
   if (hInputForm <> 0) then
   begin
     hEdit := FindWindowEx(hInputForm, 0, 'TEdit', nil);
      {
       // Change button text:
       hButton := FindWindowEx(hInputForm, 0, 'TButton', nil);
       SendMessage(hButton, WM_SETTEXT, 0, Integer(PChar('Cancel')));
       }
     SendMessage(hEdit, EM_SETLIMITTEXT, 254, 0);
   end;
end;

function TForm1.ExistsInvalidChars(const S: string): Boolean;
const
  InvalidChars: array[0..8] of Char = ('\', '/', '|', '?', '*', ':', '"', '<', '>');
begin
  Result := S.IndexOfAny(InvalidChars) <> -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListView1.PopupMenu := PopupMenu1;
  ListView2.PopupMenu := PopupMenu2;
  ListView1.SmallImages := PngImageList1;
  ListView2.SmallImages := PngImageList2;
  StatusBar1.Panels[1].Text:= EmptyStr;
  StatusBar1.Panels[0].Text := 'No open container file';
  StatusBar1.Panels[0].Width := Form1.Width - StatusBar1.Panels[1].Width;
  CurrentRootFolderName := EmptyStr;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := False;
  FilePassword := EmptyStr;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 StatusBar1.Panels[0].Width := Form1.Width - StatusBar1.Panels[1].Width;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 CloseContainer;
end;

{ === Popup Menus visibility === }

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
 if
  not(Assigned(Container))
 then
  begin
    AddCat.Visible := False;
    CloseCont.Visible := False;
    UpCont.Visible := False;
    DelCat.Visible := False;
    ExtCat.Visible := False;
  end
 else
  begin
   AddCat.Visible := True;
   CloseCont.Visible := True;
   UpCont.Visible:= True;
   if
    ListView1.Selected = nil
   then
    begin
     DelCat.Visible := False;
     ExtCat.Visible := False;
    end
   else
    begin
     DelCat.Visible := True;
     ExtCat.Visible := True;
    end;
  end;
end;

procedure TForm1.PopupMenu2Popup(Sender: TObject);
begin
 if
   (not(Assigned(Container))) or (ListView1.Selected = nil)
 then
  begin
   AddProg.Visible := False;
   ExtProg.Visible := False;
   StartProg.Visible := False;
   DelProg.Visible := False;
   Exit;
  end;

 if Assigned(ListView2.Selected) then
  begin
   ExtProg.Visible := True;
   StartProg.Visible := True;
   DelProg.Visible := True;
  end
 else
  begin
   ExtProg.Visible := False;
   StartProg.Visible := False;
   DelProg.Visible := False;
  end;

 AddProg.Visible := True;
end;

 { === Container commands === }

procedure TForm1.CreateContClick(Sender: TObject); ///// Create new container file
var
  FileName, NewPass, ConfirmPass: string;
begin
 PostMessage(Handle, InputBoxMessagePass, 0, 0);
 NewPass := InputBox('Set a new password for the container', 'Enter new password:', '');
 if
  NewPass = EmptyStr
 then
   Exit
 else
  begin
   PostMessage(Handle, InputBoxMessagePass, 0, 0);
   ConfirmPass := InputBox('Set a new password for the container again', 'Confirm new password:', '');
  end;
   if
    (ConfirmPass = EmptyStr) or (ConfirmPass <> NewPass)
   then
    begin
     MessageBoxEx(0,PChar('Passwords entered the first time and passwords entered the second time do not match!'),
       PChar(Form1.Caption + ' - confirmation new password'),MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

  SaveFileCont.InitialDir := ExtractFilePath(Application.ExeName);

  if SaveFileCont.Execute then
  begin
    FileName := SaveFileCont.FileName;
    if
     ExtractFileExt(FileName) = EmptyStr
    then
     FileName := FileName + '.ecof';

    if
      Assigned(Container)
    then
      CloseContainer;                           //       |---!SaveMetadata on close (Destructor)
                                                //       |
    Container := TFileContainer.Create(FileName, False, False, ConfirmPass); //// Create new container (False)
    Container.SaveMetadata;

    ListView1.Items.Clear;
    StatusBar1.Panels[0].Text := 'Now opened container file: ' + FileName;
  end;
end;

procedure TForm1.OpenContClick(Sender: TObject); //// Open container file
var
  FileName, TypePass: string;
begin
  if OpenFileCont.Execute then
  begin
    FileName := OpenFileCont.FileName;

    if
     ExtractFileExt(FileName) = EmptyStr
    then
      FileName := FileName + '.ecof';

    if not FileExists(FileName) then
    begin
      MessageBoxEx(0,PChar('Container not found!'),PChar(Form1.Caption + ' - open container'),
              MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
      Exit;
    end;

    if Assigned(Container) then
      FreeAndNil(Container);

   PostMessage(Handle, InputBoxMessagePass, 0, 0);
   TypePass := InputBox('Type a password for the container', 'Enter password:', '');

   try                                          //       |---!SaveMetadata on close (Destructor)
                                                //       |
    Container := TFileContainer.Create(FileName, True, False, TypePass);  //// Open container (True)
    RefreshFolderList;
    StatusBar1.Panels[0].Text := 'Now opened container file: ' + FileName;
   except
    on E: Exception do
    begin
     MessageBoxEx(0,PChar(E.Message),PChar(Form1.Caption + ' - open container'),MB_ICONWARNING+MB_OK,
       MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     StatusBar1.Panels[0].Text := 'No open container file';
    end;
   end;
  end;
end;

{If some files were previously marked as deleted (in the metadata), then after "Update",
the raw data of these files will be erased, and the container will be overwritten. }
procedure TForm1.UpContClick(Sender: TObject); //// Update container
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  ClearSecondLevelList;
  ListView1.Items.Clear;
  PngImageList2.Clear;

  try
    Container.Compact;
    RefreshFolderList;
    MessageBoxEx(0,PChar('Container optimized successfully!'),
      PChar(Form1.Caption + ' - container optimization'),MB_ICONINFORMATION+MB_OK,
       MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
  except
    on E: Exception do
    begin
      MessageBoxEx(0,PChar('Optimization error! ' + E.Message),
       PChar(Form1.Caption + ' - container optimization error!'),
        MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
      RefreshFolderList;
    end;
  end;
end;

procedure TForm1.CloseContClick(Sender: TObject); //// Close container file
begin
 CloseContainer;
end;

procedure TForm1.AddCatClick(Sender: TObject); //// Create new or add category
var
  FolderPath, FolderName, NewRootName: string;
  Choice, Select: Integer;
  Found: Boolean;
begin
  if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  // YES Ч create an empty root directory; NO Ч add from disk
  Choice := MessageBoxEx(0,
    PChar('Create a new category, or add an already collected one?' + sLineBreak +
     'Warning! Before adding an already collected category, check names and parameters!' + sLineBreak +
     'Directory with incorrectly filled names and parameters will cause the entire container to fail!'),
      PChar(Caption + ' - adding new category'), MB_ICONINFORMATION or MB_YESNOCANCEL,
        MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));

  if Choice = IDCANCEL then Exit;

  if Choice = IDYES then
  begin
    Application.ProcessMessages;
    PostMessage(Handle, InputBoxMessageString, 0, 0);
    NewRootName := InputBox('New category', 'Enter new name:', EmptyStr);

    if NewRootName = EmptyStr then
    begin
     MessageBoxEx(0,PChar('Name cannot be empty!'),PChar(Form1.Caption + ' - adding new category'),
       MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

    if ExistsInvalidChars(NewRootName)  then
    begin
     MessageBoxEx(0,PChar('Do not use these characters in names: \/*?:<>"| !'),PChar(Form1.Caption + ' - adding new category'),
       MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

    NewRootName := Trim(NewRootName);

    Found := Container.RootFolderExists(NewRootName);
    if Found then
    begin
      Select := MessageBoxEx(Handle, PChar('Categery "' + NewRootName + '" already exists. Replace?'),
              PChar(Form1.Caption + '- adding new category - confirmation'), MB_ICONWARNING or MB_YESNO,
                 MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));

      if Select <> IDYES then Exit;

      Container.DeleteFolder(NewRootName);
      Container.Compact;
    end;

    Container.AddEmptyRootFolder(NewRootName);
    Container.SaveMetadata;
    RefreshFolderList;
    Exit;
  end;

  // NO: add an existing category from disk as root
  if not SelectDirectory('Select a folder to add', '', FolderPath) then
    Exit;

  FolderName := ExtractFileName(ExcludeTrailingPathDelimiter(FolderPath));

  Found := Container.RootFolderExists(FolderName);
  if Found then
  begin
    Select := MessageBoxEx(Handle, PChar('Categery "' + FolderName + '" already exists. Replace?'),
                         PChar(Form1.Caption + '- adding new category - confirmation'), MB_ICONWARNING or MB_YESNO,
                            MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    if Select <> IDYES then Exit;

    Container.DeleteFolder(FolderName);
    Container.Compact;
  end;

  ProgressBar1.Position := 0;
    ProgressBar1.Visible := True;

  TFileContainer.OnProgressGlobal :=
     procedure(p: Integer)
       begin
         TThread.Synchronize(nil,
          procedure
           begin
            ProgressBar1.Position := p;
            Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
           end);
       end;

   TThread.CreateAnonymousThread(
    procedure
     begin
       try
        Container.AddFolder(FolderPath);
       finally
        TThread.Synchronize(nil,
         procedure
          begin
            ProgressBar1.Visible := False;
            ProgressBar1.Position := 0;
            TFileContainer.OnProgressGlobal := nil;
            Form1.StatusBar1.Panels[1].Text := EmptyStr;
            Container.SaveMetadata;
            RefreshFolderList;
          end);
       end;
     end).Start;


 // Container.AddFolder(FolderPath);
 // Container.SaveMetadata;
 // RefreshFolderList;
end;

procedure TForm1.ExtCatClick(Sender: TObject); //// Extract category
var
  DestinationPath, FolderName: string;
  Select : Integer;
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  if not Assigned(ListView1.Selected) then Exit;

  FolderName := ListView1.Selected.Caption;

  if SelectDirectory('Select a folder to save to', '', DestinationPath) then
  begin
   Select := MessageBoxEx(Handle, PChar('The category will be loaded and saved!'+
   'A category can contain many different and large programs - this may take a long time to complete. Continue?'),
                         PChar(Form1.Caption + '- saving category - warning!'), MB_ICONWARNING or MB_YESNO,
                          MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
   if Select <> IDYES then Exit;

   ProgressBar1.Position := 0;
    ProgressBar1.Visible := True;

   TFileContainer.OnProgressGlobal :=
     procedure(p: Integer)
       begin
         TThread.Synchronize(nil,
          procedure
           begin
            ProgressBar1.Position := p;
            Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
           end);
       end;

   TThread.CreateAnonymousThread(
    procedure
     begin
       try
        Container.ExtractFolder(FolderName, DestinationPath);
       finally
        TThread.Synchronize(nil,
         procedure
          begin
            ProgressBar1.Visible := False;
            ProgressBar1.Position := 0;
            TFileContainer.OnProgressGlobal := nil;
            Form1.StatusBar1.Panels[1].Text := EmptyStr;
           MessageBoxEx(0,PChar('Category uploaded successfully!'),
            PChar(Form1.Caption + ' - saving category'),MB_ICONINFORMATION+MB_OK,
             MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
          end);
       end;
     end).Start;

   // Container.ExtractFolder(FolderName, DestinationPath);
   // MessageBoxEx(0,PChar('Category saved successfully!'),
   //   PChar(Form1.Caption + ' - saving category'),MB_ICONINFORMATION+MB_OK,
   //    MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
  end;
end;

procedure TForm1.DelCatClick(Sender: TObject); //// Delete category
var
  FolderName: string;
  Select : Integer;
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  if Assigned(ListView1.Selected) then
  begin
    Select := MessageBoxEx(Handle, PChar('Delete selected category?'),
      PChar(Form1.Caption + '- delete category - warning!'), MB_ICONWARNING or MB_YESNO,
        MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));

   if Select <> IDYES then Exit;

     FolderName := ListView1.Selected.Caption;
     Container.DeleteFolder(FolderName);
     Container.SaveMetadata;
     RefreshFolderList;
     ClearSecondLevelList;
  end;
end;


{ === Programs (second-level) === }

procedure TForm1.AddProgClick(Sender: TObject); //// Add new program
var
  SourcePath, FolderName, SpecialData: string;
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  if not Assigned(ListView1.Selected) then Exit;

  PostMessage(Handle, InputBoxMessageString, 0, 0);
  FolderName := InputBox('New program', 'Enter new name:', EmptyStr);

   if FolderName = EmptyStr then
    begin
     MessageBoxEx(0,PChar('Name cannot be empty!'),PChar(Form1.Caption + ' - adding new program'),
       MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

   if ExistsInvalidChars(FolderName) then
    begin
     MessageBoxEx(0,PChar('Do not use these characters in names: \/*?:<>"| !'),PChar(Form1.Caption + ' - adding new program'),
       MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

  if Trim(FolderName) = '' then Exit;

  PostMessage(Handle, InputBoxMessageString, 0, 0);
  SpecialData := InputBox('Type installer file name (with extension)', 'Enter installer name:', EmptyStr);

   if SpecialData = EmptyStr then
    begin
     MessageBoxEx(0,PChar('Installer name parameter cannot be empty!'),
       PChar(Form1.Caption + ' - adding new program'),MB_ICONWARNING+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
     Exit;
    end;

  if not SelectDirectory('Select the folder with the prepared program to add', '', SourcePath) then
    Exit;

   ProgressBar1.Position := 0;
    ProgressBar1.Visible := True;

  TFileContainer.OnProgressGlobal :=
     procedure(p: Integer)
       begin
         TThread.Synchronize(nil,
          procedure
           begin
            ProgressBar1.Position := p;
            Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
           end);
       end;

  TThread.CreateAnonymousThread(
    procedure
     begin
       try
        Container.AddSecondLevelFolderWithContent(ListView1.Selected.Caption,FolderName,SpecialData,SourcePath);
       finally
        TThread.Synchronize(nil,
         procedure
          begin
            ProgressBar1.Visible := False;
            ProgressBar1.Position := 0;
            TFileContainer.OnProgressGlobal := nil;
            Form1.StatusBar1.Panels[1].Text := EmptyStr;
           // Container.SaveMetadata; //---------------------------------->>> ???
            RefreshSecondLevelList(ListView1.Selected.Caption);
            MessageBoxEx(0,PChar('Program added successfully!'),
             PChar(Form1.Caption + ' - adding program'),MB_ICONINFORMATION+MB_OK,
             MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
          end);
       end;
     end).Start;

end;

procedure TForm1.ExtProgClick(Sender: TObject); //// Extract program
var
  DestinationPath, FullFolderPath, SpecialData: string;
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  if not Assigned(ListView2.Selected) then Exit;

  if
   Assigned(ListView2.Selected.Data)
  then
   SpecialData := PString(ListView2.Selected.Data)^
  else
    SpecialData := EmptyStr;

  if
   SpecialData <> EmptyStr
  then
    FullFolderPath := CurrentRootFolderName + '\' + ListView2.Selected.Caption + '$' + SpecialData
  else
    FullFolderPath := CurrentRootFolderName + '\' + ListView2.Selected.Caption;

  if SelectDirectory('Select a folder to save to', '', DestinationPath) then
  begin
    TFileContainer.OnProgressGlobal :=
     procedure(p: Integer)
       begin
         TThread.Synchronize(nil,
          procedure
           begin
            ProgressBar1.Position := p;
            Form1.StatusBar1.Panels[1].Text := Format('%d%%', [p]);
           end);
       end;

   TThread.CreateAnonymousThread(
    procedure
     begin
       try
        Container.ExtractFolder(FullFolderPath, DestinationPath, True);
       finally
        TThread.Synchronize(nil,
         procedure
          begin
            ProgressBar1.Visible := False;
            ProgressBar1.Position := 0;
            TFileContainer.OnProgressGlobal := nil;
            MessageBoxEx(0,PChar('Program uploaded successfully!'),
            PChar(Form1.Caption + ' - saving program'),MB_ICONINFORMATION+MB_OK,
             MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
          end);
       end;
     end).Start;

   // Container.ExtractFolder(FullFolderPath, DestinationPath, True);
   // ShowMessage('ѕапка усп≥шно вит€гнута!');
  end;
end;

procedure TForm1.StartProgClick(Sender: TObject);
begin
  ExtractAndRunSelectedSubfolder;
end;


procedure TForm1.DelProgClick(Sender: TObject); //// Delete program
var
  FullFolderName, SpecialData: string;
begin
   if not Assigned(Container) then
  begin
    MessageBoxEx(0,PChar('Main stream of container is missing! Create or open a container first!'),
     PChar(Form1.Caption + ' - container error'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
    Exit;
  end;

  if  Assigned(ListView2.Selected) then
  begin
    if Assigned(ListView2.Selected.Data) then
      SpecialData := PString(ListView2.Selected.Data)^
    else
      SpecialData := '';

    if SpecialData <> '' then
      FullFolderName := ListView2.Selected.Caption + '$' + SpecialData
    else
      FullFolderName := ListView2.Selected.Caption;

    Container.DeleteFolder(CurrentRootFolderName + '\' + FullFolderName);
    Container.SaveMetadata;
    RefreshSecondLevelList(CurrentRootFolderName);
  end
  else
    MessageBoxEx(0,PChar('Select the folder to delete first!'),
     PChar(Form1.Caption + ' - delete program error!'),MB_ICONERROR+MB_OK, MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
end;


{ === List events === }

procedure TForm1.ListView1Click(Sender: TObject);
begin
 if ListView1.Selected = nil then
    ClearSecondLevelList;
end;


procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
 if Selected and Assigned(Item) then
  begin
    CurrentRootFolderName := Item.Caption;
    RefreshSecondLevelList(CurrentRootFolderName);
  end;
end;

procedure TForm1.ListView2DblClick(Sender: TObject);
begin
 ExtractAndRunSelectedSubfolder;
end;

procedure TForm1.ListView2Deletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    Dispose(PString(Item.Data));
    Item.Data := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
end.

//////////////////// Created MrDelphiSource 2025 //////////////////////////////////////////////

