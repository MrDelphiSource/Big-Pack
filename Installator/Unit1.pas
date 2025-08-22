unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, System.ImageList,
  Vcl.ImgList, PngImageList, FileContainerV3_3, LocalizedMessageDialog,  Vcl.StdCtrls, acImage,
  Vcl.Imaging.pngimage, Winapi.ShellAPI, Vcl.FileCtrl;

type
  TForm1 = class(TForm)
    MainPanel: TPanel;
    StatusBar1: TStatusBar;
    ListView1: TListView;
    ListView2: TListView;
    ProgressBar1: TProgressBar;
    PngImageList1: TPngImageList;
    PngImageList2: TPngImageList;
    PopupMenu1: TPopupMenu;
    TopPanel: TPanel;
    sImage1: TsImage;
    ComboBox1: TComboBox;
    Splitter1: TSplitter;
    sImage2: TsImage;
    ExtAndInstall: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListView2Deletion(Sender: TObject; Item: TListItem);
    procedure ListView2DblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ExtAndInstallClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Lang : string;
    ContFileName: string;
    Container: TFileContainer; // General file-container stream
    CurrentRootFolderName: string;
    procedure ImagePNG(ResFile: string; Image: TImage); //// Load From Resources PNG to Image
    procedure RefreshFolderList;
    procedure ClearFolderRecursive(const Folder: string);
    procedure ClearSecondLevelList;
    procedure RefreshSecondLevelList(const RootName: string);
    procedure ExtractAndRunSelectedSubfolder;
    procedure OpenContContainer; //// Open container file
    procedure CloseContainer;
  public
    { Public declarations }
  end;

const
  CONTAINER_FILE = ('BP.ecof');
  CONTAINER_PASSWORD = ('4b530c844e09aacca14d79edd1889d7ee277e07c');

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ImagePNG(ResFile: string; Image: TImage); //// Load From Resources PNG to Image
var
  ResStream : TResourceStream;
  PNG : TPngImage;
begin
 try
  PNG := TPngImage.Create;
  ResStream := TResourceStream.Create(hInstance, ResFile, RT_RCDATA);
  PNG.LoadFromStream(ResStream);
  Image.Picture := nil;
  Image.Picture.Bitmap.Assign(PNG);
  PNG.Free;
  ResStream.Free;
 except
  on E:Exception do
   begin
    if
     Assigned(ResStream)
    then
     begin
      ResStream := nil;
      ResStream.Free;
     end;
    if
     Assigned(PNG)
    then
     begin
      PNG := nil;
      PNG.Free;
     end;
   end;
 end;
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
        ClearFolderRecursive(Path);
        RemoveDir(Path);
      end
      else
        DeleteFile(Path);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TForm1.ClearSecondLevelList;
var
  I: Integer;
begin
  for I := 0 to ListView2.Items.Count - 1 do
    if Assigned(ListView2.Items[I].Data) then
    begin
      Dispose(PString(ListView2.Items[I].Data));
      ListView2.Items[I].Data := nil;
    end;

  ListView2.Items.Clear;
  PngImageList2.Clear;
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


procedure TForm1.ExtractAndRunSelectedSubfolder;
var
  TempPath, SubFolderName, SpecialParam, ExtractToPath, FullFolderPath, FullExePath, DirSave: string;
  SpecialParamPtr: PString;
begin
  if not Assigned(Container) then
  begin
    if
     Lang = ('UKR')
    then
      LocalizedMessageDlg('                                                                 Контейнер не завантажено!                                                                        ', mtError, [mbOK],
         (Form1.Caption + ' - помилка читання контейнеру'), True, 1)
    else
     if
      Lang = ('ENG')
     then
      LocalizedMessageDlg('                                                        Container is not loaded!                                                                 ', mtError, [mbOK],
          (Form1.Caption + ' - container read error'), True, 0)
     else
      LocalizedMessageDlg('                                                        Container is not loaded!                                                                 ', mtError, [mbOK],
          (Form1.Caption + ' - container read error'), True, 0);
    Exit;
  end;

  if not Assigned(ListView2.Selected) then
  begin
    Exit;
  end;

  // Get a special parameter from .Data
  SpecialParamPtr := PString(ListView2.Selected.Data);
  if (SpecialParamPtr = nil) or (SpecialParamPtr^ = '') then
  begin
    if
     Lang = ('UKR')
    then
      LocalizedMessageDlg('                                                                Спеціальний параметр відсутній!                                                                      ', mtError, [mbOK],
         (Form1.Caption + ' - помилка декларування параметрів запуску'), True, 1)
    else
     if
      Lang = ('ENG')
     then
      LocalizedMessageDlg('                                                          Special parameter is missing!                                                                   ', mtError, [mbOK],
          (Form1.Caption + ' - error declaring startup parameters'), True, 0)
     else
      LocalizedMessageDlg('                                                          Special parameter is missing!                                                                   ', mtError, [mbOK],
          (Form1.Caption + ' - error declaring startup parameters'), True, 0);

    Exit;
  end;
  SpecialParam := SpecialParamPtr^;


  TempPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Temp';

  if not DirectoryExists(TempPath) then
    ForceDirectories(TempPath);


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
               Exit;
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
                Exit;
               end);
           end;
          end).Start;
     end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
 if
   ComboBox1.ItemIndex = 0
 then
  begin
   Lang := ('ENG');
   ImagePNG('ENG', sImage2);
   ListView1.Columns.Items[0].Caption := 'ALL CATEGORIES';
   ListView2.Columns.Items[0].Caption := 'ALL PROGRAMS IN CATEGORY';
   ExtAndInstall.Caption := 'Extract and start install';
   if
    Pos('Зараз відкритий файл контейнера: ',StatusBar1.Panels[0].Text) <> 0
   then
    StatusBar1.Panels[0].Text := ('Now opened container file: ' + ContFileName)
   else
    if
     Pos('Немає відкритого файлу контейнера',StatusBar1.Panels[0].Text) <> 0
    then
     StatusBar1.Panels[0].Text := ('No open container file');

  end
 else
  if
   ComboBox1.ItemIndex = 1
  then
   begin
    Lang := ('UKR');
    ImagePNG('UKR', sImage2);
    ListView1.Columns.Items[0].Caption := 'ВСІ КАТЕГОРІЇ';
    ListView2.Columns.Items[0].Caption := 'ВСІ ПРОГРАМИ В КАТЕГОРІЇ';
    ExtAndInstall.Caption := 'Витягнути і почати встановлювати';
    if
     Pos('Now opened container file: ',StatusBar1.Panels[0].Text) <> 0
    then
     StatusBar1.Panels[0].Text := ('Зараз відкритий файл контейнера: ' + ContFileName)
    else
    if
     Pos('No open container file',StatusBar1.Panels[0].Text) <> 0
    then
     StatusBar1.Panels[0].Text := ('Немає відкритого файлу контейнера');
   end;
end;

procedure TForm1.CloseContainer;
begin
  ClearSecondLevelList;
  ListView1.Clear;

  if Assigned(Container) then
    FreeAndNil(Container);

  CurrentRootFolderName := EmptyStr;
  StatusBar1.Panels[0].Text := 'No container file selected';
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 CloseContainer;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Lang := ('ENG');
 ContFileName := EmptyStr;
 ImagePNG('TITLE_LOGO2', sImage1);
 ImagePNG('ENG', sImage2);
 ComboBox1.Items.Add('ENG');
 ComboBox1.Items.Add('UKR');
 ComboBox1.ItemIndex := 0;
 ListView1.SmallImages := PngImageList1;
 ListView2.SmallImages := PngImageList2;
 StatusBar1.Panels[1].Text:= EmptyStr;
 StatusBar1.Panels[0].Text := 'No open container file';
 StatusBar1.Panels[0].Width := Form1.Width - StatusBar1.Panels[1].Width;
 CurrentRootFolderName := EmptyStr;
 ProgressBar1.Position := 0;
 ProgressBar1.Visible := False;
 OpenContContainer;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 StatusBar1.Panels[0].Width := Form1.Width - StatusBar1.Panels[1].Width;
end;

procedure TForm1.OpenContContainer; //// Open container file

begin
 ContFileName := CONTAINER_FILE;
  if
   not (FileExists(ContFileName))
  then
   begin
    if
     Lang = ('UKR')
    then
      LocalizedMessageDlg('                                                   Файл контейнера не знайдено!                                                               ', mtError, [mbOK],
         (Form1.Caption + ' - помилка визначення файлу'), True, 1)
    else
     if
      Lang = ('ENG')
     then
      LocalizedMessageDlg('                                                 Container file not found!                                                           ', mtError, [mbOK],
          (Form1.Caption + ' - file definition error'), True, 0)
     else
      LocalizedMessageDlg('                                                 Container file not found!                                                           ', mtError, [mbOK],
          (Form1.Caption + ' - file definition error'), True, 0);
    Exit;
   end;

    if Assigned(Container) then
      FreeAndNil(Container);

   try                                               //    !Only Read!
    Container := TFileContainer.Create(ContFileName, True, True, CONTAINER_PASSWORD);  //// Open container (True)
    RefreshFolderList;
    StatusBar1.Panels[0].Text := 'Now opened container file: ' + ContFileName;
   except
    on E: Exception do
    begin
     if
      Lang = ('UKR')
     then
      LocalizedMessageDlg('                                          Помилка при декодуванні! Невірний ключ!                                                              ', mtError, [mbOK],
         (Form1.Caption + ' - помилка при декодуванні'), True, 1)
     else
     if
      Lang = ('ENG')
     then
      LocalizedMessageDlg('                                                Decoding error! Invalid key!                                                          ', mtError, [mbOK],
          (Form1.Caption + ' - decoding error!'), True, 0)
     else
      LocalizedMessageDlg('                                                Decoding error! Invalid key!                                                          ', mtError, [mbOK],
          (Form1.Caption + ' - decoding error!'), True, 0);
     StatusBar1.Panels[0].Text := 'No open container file!';
    end;
   end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
 if
   (not(Assigned(Container))) or (ListView1.Selected = nil)
 then
  begin
   ExtAndInstall.Visible := False;
   Exit;
  end;

 if Assigned(ListView2.Selected) then
  begin
   ExtAndInstall.Visible := True;
  end
 else
  begin
   ExtAndInstall.Visible := False;
  end;
end;

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
 if
  Assigned(Container)
 then
  begin
   if
    not (Container.Busy)
   then
    ExtractAndRunSelectedSubfolder;
  end;
end;

procedure TForm1.ExtAndInstallClick(Sender: TObject);
begin
 if
  Assigned(Container)
 then
  begin
   if
    not (Container.Busy)
   then
    ExtractAndRunSelectedSubfolder;
  end;
end;

procedure TForm1.ListView2Deletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    Dispose(PString(Item.Data));
    Item.Data := nil;
  end;
end;


///////////////////////////// Created MrDelphiSource 2025 /////////////////////////////////////////
end.
