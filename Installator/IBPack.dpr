program IBPack;

{$R *.dres}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  FileContainerV3_3 in 'FileContainerUnit\FileContainerV3_3.pas',
  LocalizedMessageDialog in 'LocalizedMessageDialog\LocalizedMessageDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Dark Vector Blue SE');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
