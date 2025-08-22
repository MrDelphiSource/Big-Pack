program PBPack;

uses
  Vcl.Forms,
  Winapi.Windows,
  Unit1 in 'Unit1.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  FileContainerV3_3 in 'FileContainerUnit\FileContainerV3_3.pas';

var
 xHNDL : THandle;
 WND : THandle;

{$R *.res}

begin

 xHNDL := CreateMutex(nil, True, 'Mutex_MyASI_First*01');
  if
   (GetLastError = ERROR_ALREADY_EXISTS) or (GetLastError = ERROR_ACCESS_DENIED)
  then
   begin
    WND := FindWindow('TForm1',nil);
    if
     WND <> 0
    then
     begin
      SendMessage(WND,WM_GOTOFOREGROUND,0,0);
     end;
    ReleaseMutex(xHNDL);
    //CloseHandle(xHNDL);
    Application.Terminate;
    Exit;
   end;


  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Dark Vector Blue');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
