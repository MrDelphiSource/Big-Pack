unit LocalizedMessageDialog;

interface

uses
  Vcl.Dialogs, Vcl.Forms, Vcl.StdCtrls, Windows,  Vcl.Controls;

function LocalizedMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; STitle: string;  CusTitle: Boolean = False; LangID: Integer = 0): TModalResult;

implementation

procedure LocalizeButtons(Dlg: TForm; LangID: Integer);
var
  i: Integer;
  Btn: TButton;
begin
  for i := 0 to Dlg.ControlCount - 1 do
    if Dlg.Controls[i] is TButton then
    begin
      Btn := TButton(Dlg.Controls[i]);
      case LangID of
        0: // US English
          begin
            if Btn.ModalResult = mrYes then Btn.Caption := 'Yes';
            if Btn.ModalResult = mrNo then Btn.Caption := 'No';
            if Btn.ModalResult = mrCancel then Btn.Caption := 'Cancel';
            if Btn.ModalResult = mrOk then Btn.Caption := 'OK';
          end;

        1: // UA Українська
          begin
            if Btn.ModalResult = mrYes then Btn.Caption := 'Так';
            if Btn.ModalResult = mrNo then Btn.Caption := 'Ні';
            if Btn.ModalResult = mrCancel then Btn.Caption := 'Відміна';
            if Btn.ModalResult = mrOk then Btn.Caption := 'ОК';
          end;

         //////
      end;
    end;
end;

function LocalizedMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; STitle: string;  CusTitle: Boolean = False; LangID: Integer = 0): TModalResult;
var
  Dlg: TForm;
  ATitle: string;
begin
      // Title depending on language and type
  case LangID of
    0: // US English
      case DlgType of
        mtWarning: ATitle := 'Warning';
        mtInformation: ATitle := 'Information';
        mtError: ATitle := 'Error';
        mtConfirmation: ATitle := 'Confirmation';
      else
        ATitle := Application.Title;
      end;

    1: // UA Українська
      case DlgType of
        mtWarning: ATitle := 'Попередження';
        mtInformation: ATitle := 'Інформація';
        mtError: ATitle := 'Помилка';
        mtConfirmation: ATitle := 'Підтвердження';
      else
        ATitle := Application.Title;
      end;

     //////
  end;

  // Creating a dialogue
  Dlg := CreateMessageDialog(Msg, DlgType, Buttons);
  try
   if
    CusTitle
   then
    Dlg.Caption := STitle
   else
    Dlg.Caption := ATitle;

    // Button localization
    LocalizeButtons(Dlg, LangID);

    // System sound
    case DlgType of
      mtWarning: MessageBeep(MB_ICONWARNING);
      mtError: MessageBeep(MB_ICONHAND);
      mtInformation: MessageBeep(MB_ICONINFORMATION);
      mtConfirmation: MessageBeep(MB_ICONQUESTION);
    else
      MessageBeep(0);
    end;

    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;


///////////////////////////////////////////////////////////////////////////////////////////////////
{

// Example of use

   1. One "OK" button

 -->       LocalizedMessageDlg('Hello, world!', mtInformation, [mbOK], 'Hello, world!', True,  0);

   2. Two "Yes"/"No" buttons

 -->     if
           LocalizedMessageDlg('Delete file?', mtConfirmation, [mbYes, mbNo], 'My program', True, 0) = mrYes
         then
           ShowMessage('File deleted!');


   3. Three buttons "Yes", "No", "Cancel"

 -->   LocalizedMessageDlg('Save changes?', mtWarning, [mbYes, mbNo, mbCancel], 'My program', True, 0);

                                                                                                }
//////////////////////////////////////////////////////////////////////////////////////////////////
end.
